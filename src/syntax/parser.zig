const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig").Tokenizer;
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");
const StringHelper = @import("./strings.zig");

const util = @import("util");
const types = util.types;

const Node = ast.Node;
const NodeData = ast.NodeData;

const ParseError = error{
    UnexpectedToken,
    OutOfMemory,
    NotSupported,
    IllegalReturn,
    InvalidAssignmentTarget,
} || Tokenizer.Error;
const ParseFn = fn (self: *Self) ParseError!Node.Index;

/// Used to save and restore the parser state.
/// Helpful when backtracking for cover grammars (like arrow functions v/s parenthesized exprs)
const State = struct {
    nodes_len: usize,
    tokens_len: usize,
    node_lists_len: usize,
    extra_data_len: usize,

    current_token: Token,
    next_token: Token,
    tokenizer_state: Tokenizer.State,

    context: ParseContext,
    diagnostics: ?[]Diagnostic,

    pub fn destroy(self: *State, allocator: std.mem.Allocator) void {
        if (self.diagnostics) |diagnostics| {
            allocator.free(diagnostics);
        }
    }
};

/// An error or warning raised by the Parser.
pub const Diagnostic = struct {
    /// Index into the `diagnostic_messages` slice.
    const MessageIndex = enum(usize) { _ };
    /// line/col position where error occurred.
    coord: types.Coordinate,
    /// Message reported by the parser.
    message: MessageIndex,
};

/// Represents the currently active set of grammatical parameters
/// in the parser state.
/// https://tc39.es/ecma262/#sec-grammatical-parameters
pub const ParseContext = packed struct(u8) {
    /// Is a ReturnStatement allowed in the current context?
    /// Unlike `await`, `return` is always parsed as a keyword, regardles of context.
    @"return": bool = false,
    /// Is a BreakStatement allowed in the current context?
    @"break": bool = false,
    /// Is an `await` parsed as an identifier or as a keyword?
    @"await": bool = false,
    /// Is `yield` parsed as an identifier or as a keyword?
    yield: bool = false,
    /// Are we parsing a module? (alternative is a script).
    module: bool = false,
    /// Are we in strict mode?
    strict: bool = false,
    in: bool = false,
    _: bool = false,
};

// arranged in highest to lowest binding

// TODO: Officially, exponentiation operator is defined as:
// ExponentiationExpression :
//  UnaryExpression
//  | UpdateExpression ** ExponentiationExpression
// So this isn't exactly correct.
// The parselet for this will have to be hand-written.
const exponentExpr = makeRightAssoc(.@"**", Self.unaryExpression);

const multiplicativeExpr = makeLeftAssoc(.multiplicative_start, .multiplicative_end, exponentExpr);
const additiveExpr = makeLeftAssoc(.additive_start, .additive_end, multiplicativeExpr);

const shiftExpr = makeLeftAssoc(.shift_op_start, .shift_op_end, additiveExpr);
const relationalExpr = makeLeftAssoc(.relational_start, .relational_end, shiftExpr);
const eqExpr = makeLeftAssoc(.eq_op_start, .eq_op_end, relationalExpr);

const bAndExpr = makeLeftAssoc(.@"&", .@"&", eqExpr);
const bXorExpr = makeLeftAssoc(.@"^", .@"^", bAndExpr);
const bOrExpr = makeLeftAssoc(.@"|", .@"|", bXorExpr);

const lAndExpr = makeLeftAssoc(.@"&&", .@"&&", bOrExpr);
const lOrExpr = makeLeftAssoc(.@"||", .@"||", lAndExpr);

allocator: std.mem.Allocator,

/// Immutable reference to the source code.
source: []const u8,
file_name: []const u8,
tokenizer: Tokenizer,
/// All AST nodes are stored in this flat list and reference
/// each other using their indices.
/// This is an *append only* list.
/// No function other than `saveState`, and `restoreState` should modify
/// the existing items in this list.
nodes: std.ArrayList(Node),
/// Extra information about a node, if any.
extra_data: std.ArrayList(ast.ExtraData),
/// List of tokens that are necessary to keep around
/// e.g - identifiers, literals, function names, etc.
tokens: std.ArrayList(Token),
/// Arguments for function calls, new-expressions, etc.
node_lists: std.ArrayList(Node.Index),
/// List of error messages and warnings generated during parsing.
diagnostics: std.ArrayList(Diagnostic),
diagnostic_messages: std.ArrayList([]const u8),
/// The token that we're currently at.
/// Calling `next()` or `peek()` will return this token.
current_token: Token,
/// The next token that we're going to read.
next_token: Token,
/// Helper struct to manage, escape, and compare strings in source code.
strings: StringHelper,
/// The current grammatical context of the parser. See struct `ParseContext`.
context: ParseContext = .{},

pub fn init(
    allocator: std.mem.Allocator,
    source: []const u8,
    file_name: []const u8,
) ParseError!Self {
    var self = Self{
        .allocator = allocator,
        .source = source,
        .file_name = file_name,
        .tokenizer = try Tokenizer.init(source),
        .current_token = undefined,
        .next_token = undefined,

        .diagnostics = try std.ArrayList(Diagnostic).initCapacity(allocator, 2),
        .diagnostic_messages = try std.ArrayList([]const u8).initCapacity(allocator, 2),
        .nodes = try std.ArrayList(Node).initCapacity(allocator, 32),
        .node_lists = try std.ArrayList(Node.Index).initCapacity(allocator, 32),
        .extra_data = try std.ArrayList(ast.ExtraData).initCapacity(allocator, 32),
        .tokens = try std.ArrayList(Token).initCapacity(allocator, 256),

        .strings = try StringHelper.init(allocator, source),
    };

    errdefer self.deinit();

    // the `null` node always lives at index-0.
    // see: ast.NodeData.none
    const i = try self.addNode(.{ .none = {} }, 0, 0);
    std.debug.assert(i == Node.Index.empty);

    // these calls will initialize `current_token` and `next_token`.
    _ = try self.next();
    _ = try self.next();
    return self;
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit();
    self.tokens.deinit();
    self.node_lists.deinit();
    self.extra_data.deinit();
    self.diagnostics.deinit();
    for (self.diagnostic_messages.items) |m| {
        self.allocator.free(m);
    }
    self.diagnostic_messages.deinit();
    self.strings.deinit();
}

/// Save the parser and tokenizer state, then return an object
/// that can be used to restore the state with `restoreState`.
/// States must be saved and re-stored in a stack-order.
/// i.e: `s1 = save() -> s2 = save() -> restore(s2) -> restore(s1)` is valid,
/// but `s1 = save() -> s2 = save() -> restore(s2) -> restore(s1)` is not.
fn saveState(self: *Self) error{OutOfMemory}!State {
    return State{
        .context = self.context,
        .current_token = self.current_token,
        .next_token = self.next_token,
        .nodes_len = self.nodes.items.len,
        .tokens_len = self.tokens.items.len,
        .extra_data_len = self.extra_data.items.len,
        .node_lists_len = self.node_lists.items.len,
        .tokenizer_state = self.tokenizer.saveState(),
        .diagnostics = if (self.diagnostics.items.len > 0)
            try self.allocator.dupe(Diagnostic, self.diagnostics.items)
        else
            null,
    };
}

/// Restore the parser and tokenizer state to a saved state snapshot.
fn restoreState(self: *Self, state: *State) error{OutOfMemory}!void {
    self.current_token = state.current_token;
    self.next_token = state.next_token;

    self.nodes.items = self.nodes.items[0..state.nodes_len];
    self.tokens.items = self.tokens.items[0..state.tokens_len];
    self.node_lists.items = self.node_lists.items[0..state.node_lists_len];
    self.extra_data.items = self.extra_data.items[0..state.extra_data_len];
    self.tokenizer.restoreState(state.tokenizer_state);

    self.context = state.context;

    if (state.diagnostics) |saved_diagnostics| {
        // "Steal" the diagnostics from the state.
        self.diagnostics.deinit();
        self.diagnostics = std.ArrayList(Diagnostic).fromOwnedSlice(self.allocator, saved_diagnostics);
        state.diagnostics = null;
    }
}

pub fn parse(self: *Self) !Node.Index {
    var statements = std.ArrayList(Node.Index).init(self.allocator);
    defer statements.deinit();

    while (self.current_token.tag != .eof) {
        const stmt = try self.statement();
        try statements.append(stmt);
    }

    const stmt_list = try self.addNodeList(statements.items);
    return try self.addNode(
        .{ .program = stmt_list },
        0,
        @intCast(self.source.len),
    );
}

// ----------------------------------------------------------------------------
// Statements and declarators.
// https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations
// ----------------------------------------------------------------------------

/// https://tc39.es/ecma262/#prod-Statement
fn statement(self: *Self) ParseError!Node.Index {
    var can_end_in_semi = true;
    const stmt = blk: {
        switch (self.peek().tag) {
            .@"{" => {
                can_end_in_semi = false;
                break :blk self.blockStatement();
            },
            .@";" => {
                can_end_in_semi = false;
                break :blk self.emptyStatement();
            },
            .kw_if => {
                can_end_in_semi = false;
                break :blk self.ifStatement();
            },
            .kw_debugger => {
                const token = try self.next();
                break :blk self.addNode(
                    .{ .debugger_statement = {} },
                    token.start,
                    token.start + token.len,
                );
            },

            .kw_function => {
                can_end_in_semi = false;
                const fn_token = try self.next();
                break :blk self.functionDeclaration(fn_token.start, .{});
            },

            .kw_return => break :blk self.returnStatement(),
            .kw_let, .kw_var, .kw_const => break :blk self.variableStatement(),
            else => break :blk self.expressionStatement(),
        }
    };

    if (can_end_in_semi) try self.consume(.@";");

    return stmt;
}

fn ifStatement(self: *Self) ParseError!Node.Index {
    const if_kw = try self.next();
    std.debug.assert(if_kw.tag == .kw_if);

    _ = try self.expect(.@"(");
    const cond = try self.expression();
    _ = try self.expect(.@")");

    const consequent = try self.statement();
    var end_pos = self.nodeSpan(consequent).end;

    var alternate = Node.Index.empty;
    if (self.peek().tag == .kw_else) {
        _ = try self.next();
        alternate = try self.statement();
        end_pos = self.nodeSpan(alternate).end;
    }

    return self.addNode(
        .{
            .if_statement = .{
                .condition = cond,
                .consequent = consequent,
                .alternate = alternate,
            },
        },
        if_kw.start,
        end_pos,
    );
}

fn variableStatement(self: *Self) ParseError!Node.Index {
    const kw = try self.next();
    std.debug.assert(kw.tag == .kw_let or kw.tag == .kw_var or kw.tag == .kw_const);

    // TODO: fast path for single variable declaration?
    var declarators = std.ArrayList(Node.Index).init(self.allocator);
    defer declarators.deinit();

    // parse a VariableDeclarationList
    while (true) {
        const decl = try self.variableDeclarator();
        try declarators.append(decl);
        const token = self.peek();
        if (token.tag != .@",") break;
        _ = try self.next();
    }

    const decls = try self.addNodeList(declarators.items);
    const last_decl = self.node_lists.items[@intFromEnum(decls.to) - 1];

    return self.addNode(
        .{ .variable_declaration = .{
            .kind = switch (kw.tag) {
                .kw_let => .let,
                .kw_var => .@"var",
                .kw_const => .@"const",
                else => unreachable,
            },
            .declarators = decls,
        } },
        kw.start,
        self.getNode(last_decl).end,
    );
}

fn variableDeclarator(self: *Self) ParseError!Node.Index {
    const lhs = try self.assignmentLhsExpr();

    const start_pos = self.nodeSpan(lhs).start;
    var end_pos = self.nodeSpan(lhs).end;

    var rhs: ?Node.Index = null;
    if (self.isAtToken(.@"=")) {
        _ = try self.next();
        const init_expr = try self.assignmentExpression();
        end_pos = self.nodeSpan(init_expr).end;
        rhs = init_expr;
    }

    return self.addNode(
        .{ .variable_declarator = .{ .lhs = lhs, .init = rhs } },
        start_pos,
        end_pos,
    );
}

fn emptyStatement(self: *Self) ParseError!Node.Index {
    const semicolon = try self.next();
    std.debug.assert(semicolon.tag == .@";");

    return addNode(
        self,
        .{ .empty_statement = {} },
        semicolon.start,
        semicolon.start + semicolon.len,
    );
}

fn expressionStatement(self: *Self) ParseError!Node.Index {
    const expr = try self.expression();
    try self.consume(.@";");
    const expr_node = self.getNode(expr);
    return addNode(
        self,
        .{ .expression_statement = expr },
        expr_node.start,
        expr_node.end,
    );
}

fn blockStatement(self: *Self) !Node.Index {
    const start_pos = self.peek().start;

    try self.consume(.@"{");

    var statements = std.ArrayList(Node.Index).init(self.allocator);
    defer statements.deinit();

    while (self.peek().tag != .@"}" and self.peek().tag != .eof) {
        const stmt = try self.statement();
        try statements.append(stmt);
    }

    const rbrace = try self.expect(.@"}");
    const end_pos = rbrace.start + rbrace.len;

    if (statements.items.len == 0) {
        return addNode(
            self,
            .{ .block_statement = null },
            start_pos,
            end_pos,
        );
    }

    const stmt_list_node = try self.addNodeList(statements.items);
    return self.addNode(
        .{ .block_statement = stmt_list_node },
        start_pos,
        end_pos,
    );
}

/// Assuming the parser is at the `function` keyword,
/// parse a function declaration statement.
fn functionDeclaration(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    const name_token = try self.addToken(try self.expect(.identifier));
    return self.parseFunctionBody(start_pos, name_token, flags);
}

/// ReturnStatement:
///    'return' [no LineTerminator here] Expression? ';'
fn returnStatement(self: *Self) ParseError!Node.Index {
    const return_kw = try self.next();
    std.debug.assert(return_kw.tag == .kw_return);

    if (!self.context.@"return") {
        // todo: maybe we should just emit a diagnostic here and continue parsing?
        // that way we can catch more parse errors than just this.
        try self.emitDiagnostic(
            return_kw.startCoord(self.source),
            "Return statement is not allowed outside of a function",
            .{},
        );
        return ParseError.IllegalReturn;
    }

    if (self.current_token.line != return_kw.line or self.current_token.tag == .@";") {
        return self.addNode(
            .{ .return_statement = null },
            return_kw.start,
            return_kw.start + return_kw.len,
        );
    }

    const operand = try self.expression();
    const end_pos = self.nodeSpan(operand).end;
    return self.addNode(.{ .return_statement = operand }, return_kw.start, end_pos);
}

// ------------------------
// Common helper functions
// ------------------------

fn setContext(self: *Self, context: ParseContext) void {
    self.context = context;
    self.tokenizer.context = context;
}

fn addNodeList(self: *Self, nodes: []Node.Index) error{OutOfMemory}!ast.NodeList {
    const from: ast.NodeList.Index = @enumFromInt(self.node_lists.items.len);
    try self.node_lists.appendSlice(nodes);
    const to: ast.NodeList.Index = @enumFromInt(self.node_lists.items.len);
    return ast.NodeList{ .from = from, .to = to };
}

fn addToken(self: *Self, token: Token) error{OutOfMemory}!Token.Index {
    try self.tokens.append(token);
    return @enumFromInt(self.tokens.items.len - 1);
}

/// Append a node to the flat node list.
fn addNode(self: *Self, node: NodeData, start: u32, end: u32) error{OutOfMemory}!Node.Index {
    try self.nodes.append(.{ .data = node, .start = start, .end = end });
    return @enumFromInt(self.nodes.items.len - 1);
}

/// Append an ExtraData item to the list, and return its index.
fn addExtraData(self: *Self, data: ast.ExtraData) error{OutOfMemory}!ast.ExtraData.Index {
    try self.extra_data.append(data);
    return @enumFromInt(self.extra_data.items.len - 1);
}

/// Push an error essage to the list of diagnostics.
fn emitDiagnostic(
    self: *Self,
    coord: types.Coordinate,
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostic_messages.append(message);
    try self.diagnostics.append(Diagnostic{
        .coord = coord,
        .message = @enumFromInt(self.diagnostic_messages.items.len - 1),
    });
}

/// Eat the current token if it matches `tag`.
fn consume(self: *Self, tag: Token.Tag) ParseError!void {
    if (self.peek().tag == tag) {
        _ = try self.next();
    }
}

/// Emit a parse error if the current token does not match `tag`.
fn expect(self: *Self, tag: Token.Tag) ParseError!Token {
    const token = try self.next();
    if (token.tag == tag) {
        return token;
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Expected a '{s}', but found a '{s}'",
        .{ @tagName(tag), token.toByteSlice(self.source) },
    );
    return ParseError.UnexpectedToken;
}

/// Emit a parse error if the current token does not match `tag1` or `tag2`.
fn expect2(self: *Self, tag1: Token.Tag, tag2: Token.Tag) ParseError!Token {
    const token = try self.next();
    if (token.tag == tag1 or token.tag == tag2) {
        return token;
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Expected a '{s}' or a '{s}', but found a '{s}'",
        .{
            @tagName(tag1),
            @tagName(tag2),
            token.toByteSlice(self.source),
        },
    );
    return ParseError.UnexpectedToken;
}

/// Consume the next token from the lexer, skipping all comments.
fn next(self: *Self) ParseError!Token {
    var next_token = try self.tokenizer.next();
    while (next_token.tag == .comment) : (next_token = try self.tokenizer.next()) {
        // TODO: store comments as trivia.
    }

    const ret_token = self.current_token;
    self.current_token = self.next_token;
    self.next_token = next_token;
    return ret_token;
}

inline fn peek(self: *Self) Token {
    return self.current_token;
}

fn isAtToken(self: *Self, tag: Token.Tag) bool {
    return self.peek().tag == tag;
}

// --------------------------------------------------------------------------------------
// Expression parsing.
// https://262.ecma-international.org/15.0/index.html#sec-ecmascript-language-expressions
// --------------------------------------------------------------------------------------

// Expression : AssignmentExpression
//            | Expression, AssignmentExpression
fn expression(self: *Self) !Node.Index {
    const expr = try self.assignmentExpression();
    if (!self.isAtToken(.@",")) return expr;

    var nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer nodes.deinit();

    _ = try nodes.append(expr);

    const start_pos = self.nodes.items[@intFromEnum(expr)].start;
    var end_pos = self.nodes.items[@intFromEnum(expr)].end;
    while (self.isAtToken(.@",")) {
        _ = try self.next(); // eat ','
        const rhs = try self.assignmentExpression();
        end_pos = self.nodes.items[@intFromEnum(rhs)].end;
        try nodes.append(rhs);
    }

    const expr_list = try self.addNodeList(nodes.items);
    return try self.addNode(ast.NodeData{
        .sequence_expr = expr_list,
    }, start_pos, end_pos);
}

fn assignmentLhsExpr(self: *Self) ParseError!Node.Index {
    const token = self.peek();
    switch (token.tag) {
        .@"{" => return self.objectAssignmentPattern(),
        .@"[" => return self.arrayAssignmentPattern(),
        .identifier => {
            const id_token = try self.next();
            const start = token.start;
            const end = token.start + token.len;
            return self.addNode(.{
                .identifier = try self.addToken(id_token),
            }, start, end);
        },
        else => {
            try self.emitDiagnostic(
                token.startCoord(self.source),
                "Expected assignment target, got: {s}",
                .{token.toByteSlice(self.source)},
            );
            return ParseError.InvalidAssignmentTarget;
        },
    }
}

fn isSimpleAssignmentTarget(self: *const Self, node: Node.Index) bool {
    return switch (self.getNode(node).data) {
        .identifier, .member_expr, .computed_member_expr => true,
        else => false,
    };
}

fn assignmentExpression(self: *Self) ParseError!Node.Index {
    var snapshot = try self.saveState(); // to re-parse LHS if its destrucuted.
    defer snapshot.destroy(self.allocator);

    var is_assignment_pattern = false;

    // todo: can I make fewer allocations here?
    var lhs = conditionalExpression(self) catch |err| blk: {
        // If parsing as a conditional expression fails,
        // try to parse as an assignment target.
        // This is necessary for cases like: {a, b = c} = { a: 1, b: 2 }
        // Where the LHS isn't valid assignment expr, but a valid destructuring pattern.
        const parse_diagnostics = try self.allocator.dupe(
            Diagnostic,
            self.diagnostics.items,
        );

        // go back to where the assignment expression started.
        try self.restoreState(&snapshot);
        is_assignment_pattern = true;
        // then, re-parse as a LeftHandSideExpressions
        const lhs_pattern = self.assignmentLhsExpr() catch {
            // If this parse fails too
            // restore the diagnsotics from the original parse attempt (i.e
            // expression, not assignment target).
            self.diagnostics.deinit();
            self.diagnostics = std.ArrayList(Diagnostic).fromOwnedSlice(
                self.allocator,
                parse_diagnostics,
            );
            return err;
        };

        self.allocator.free(parse_diagnostics);
        break :blk lhs_pattern;
    };

    if (!self.peek().isAssignmentOperator()) {
        return lhs;
    }

    // If we see a '=' token, and lhs is not an identifier or member-expression
    // go back and re-parse the LHS as an assignment target.
    // (AssignmentPattern).
    if (!is_assignment_pattern and !self.isSimpleAssignmentTarget(lhs)) {
        try self.restoreState(&snapshot);
        lhs = try self.assignmentLhsExpr();
    }

    const op_token = try self.expect(.@"="); // eat '='

    const rhs = try self.assignmentExpression();
    const start = self.nodes.items[@intFromEnum(rhs)].end;
    const end = self.nodes.items[@intFromEnum(lhs)].start;

    return self.addNode(
        .{
            .assignment_expr = .{
                .lhs = lhs,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            },
        },
        start,
        end,
    );
}

fn coalesceExpression(self: *Self, start_expr: Node.Index) ParseError!Node.Index {
    const start_pos = self.nodes.items[@intFromEnum(start_expr)].start;
    var end_pos = self.nodes.items[@intFromEnum(start_expr)].end;

    var expr: Node.Index = start_expr;
    while (self.isAtToken(.@"??")) {
        const op = try self.next(); // eat '??'
        const rhs = try bOrExpr(self);
        end_pos = self.nodes.items[@intFromEnum(rhs)].end;
        expr = try self.addNode(
            .{
                .binary_expr = .{
                    .lhs = expr,
                    .rhs = rhs,
                    .operator = try self.addToken(op),
                },
            },
            start_pos,
            end_pos,
        );
    }

    return expr;
}

/// ShortCircuitExpression:
///    LogicalOrExpression
///    CoalesceExpression
fn shortCircuitExpresion(self: *Self) ParseError!Node.Index {
    const expr = try lOrExpr(self);
    switch (self.nodes.items[@intFromEnum(expr)].data) {
        .binary_expr => |pl| {
            const op_tag = self.tokens.items[@intFromEnum(pl.operator)].tag;
            if (op_tag == .@"||" or op_tag == .@"&&") {
                return expr;
            }
            return self.coalesceExpression(expr);
        },
        else => {
            return self.coalesceExpression(expr);
        },
    }
}

/// ConditionalExpression:
///     ShortCircuitExpression
///     ShortCircuitExpression '?' AssignmentExpression ':' AssignmentExpression
fn conditionalExpression(self: *Self) ParseError!Node.Index {
    const cond_expr = try self.shortCircuitExpresion();
    if (!self.isAtToken(.@"?")) return cond_expr;

    _ = try self.next(); // eat '?'
    const true_expr = try self.assignmentExpression();
    _ = try self.expect(.@":");
    const false_expr = try self.assignmentExpression();

    const start_pos = self.nodes.items[@intFromEnum(cond_expr)].start;
    const end_pos = self.nodes.items[@intFromEnum(false_expr)].end;

    return try self.addNode(
        .{
            .conditional_expr = .{
                .condition = cond_expr,
                .consequent = true_expr,
                .alternate = false_expr,
            },
        },
        start_pos,
        end_pos,
    );
}

// TODO: check static semantics according to the spec.
fn assignmentPattern(self: *Self) ParseError!Node.Index {
    const lhs = try self.lhsExpression();
    if (!self.isAtToken(.@"=")) return lhs;

    const op_token = try self.next(); // eat '='
    const rhs = try self.assignmentExpression();
    const lhs_start_pos = self.nodes.items[@intFromEnum(lhs)].start;
    const rhs_end_pos = self.nodes.items[@intFromEnum(rhs)].end;
    return try self.addNode(
        .{
            .assignment_pattern = .{
                .lhs = lhs,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            },
        },
        lhs_start_pos,
        rhs_end_pos,
    );
}

/// https://tc39.es/ecma262/#prod-ArrayAssignmentPattern
fn arrayAssignmentPattern(self: *Self) ParseError!Node.Index {
    const lbrac = try self.next(); // eat '['
    std.debug.assert(lbrac.tag == .@"[");

    var items = std.ArrayList(Node.Index).init(self.allocator);
    defer items.deinit();

    var token = self.peek();
    while (token.tag != .@"]") : (token = self.peek()) {
        switch (token.tag) {
            .@"," => {
                _ = try self.next(); // eat ','
                if (self.isAtToken(.@"]")) break;
                try items.append(try self.addNode(
                    .{ .empty_array_item = {} },
                    token.start,
                    token.start + token.len,
                ));
            },

            .@"..." => {
                _ = try self.next(); // eat '...'
                const start_pos = token.start;
                const spread_elem = try self.lhsExpression();
                const end_pos = self.nodes.items[@intFromEnum(spread_elem)].end;
                try items.append(
                    try self.addNode(.{ .spread_element = spread_elem }, start_pos, end_pos),
                );

                if (self.isAtToken(.@",")) {
                    const comma_tok = try self.next();
                    try self.emitDiagnostic(
                        comma_tok.startCoord(self.source),
                        "Comma not permitted after spread element in assignment target",
                        .{},
                    );
                    return ParseError.InvalidAssignmentTarget;
                }

                break;
            },

            else => {
                try items.append(try self.assignmentPattern());
            },
        }
    }

    const rbrac = try self.next(); // eat ']'
    const array_items = try self.addNodeList(items.items);
    return try self.addNode(
        .{ .array_pattern = array_items },
        lbrac.start,
        rbrac.start + rbrac.len,
    );
}

fn completePropertyPatternDef(self: *Self, key: Node.Index) ParseError!Node.Index {
    _ = try self.expect(.@":");

    const value = try self.assignmentPattern();
    const start_pos = self.nodes.items[@intFromEnum(key)].start;
    const end_pos = self.nodes.items[@intFromEnum(value)].end;

    return self.addNode(
        .{ .object_property = .{ .key = key, .value = value } },
        start_pos,
        end_pos,
    );
}

fn destructuredPropertyDefinition(self: *Self) ParseError!Node.Index {
    switch (self.peek().tag) {
        .string_literal, .numeric_literal => {
            const key_token = try self.next();
            const key = try self.addNode(
                .{ .literal = try self.addToken(key_token) },
                key_token.start,
                key_token.start + key_token.len,
            );
            return self.completePropertyPatternDef(key);
        },
        .identifier => {
            const key_token = try self.next();
            const key = try self.addNode(
                .{ .identifier = try self.addToken(key_token) },
                key_token.start,
                key_token.start + key_token.len,
            );

            const lookahead = self.peek();
            if (lookahead.tag == .@"=") {
                const eq_token = try self.next(); // eat '='
                const rhs = try self.assignmentExpression();
                const end_pos = self.getNode(rhs).end;

                const assign_pattern = try self.addNode(
                    .{ .assignment_pattern = .{
                        .lhs = key,
                        .rhs = rhs,
                        .operator = try self.addToken(eq_token),
                    } },
                    key_token.start,
                    end_pos,
                );

                return self.addNode(
                    .{ .object_property = .{ .key = key, .value = assign_pattern } },
                    key_token.start,
                    end_pos,
                );
            }

            if (lookahead.tag == .@":") {
                return self.completePropertyPatternDef(key);
            }

            return self.addNode(
                .{ .object_property = ast.PropertyDefinition{ .key = key, .value = key } },
                key_token.start,
                key_token.start + key_token.len,
            );
        },
        .@"[" => {
            _ = try self.next(); // eat '['
            const key = try self.assignmentExpression();
            _ = try self.expect(.@"]");
            return self.completePropertyPatternDef(key);
        },
        else => {
            return try self.assignmentPattern();
        },
    }
}

/// https://tc39.es/ecma262/#prod-ObjectAssignmentPattern
fn objectAssignmentPattern(self: *Self) ParseError!Node.Index {
    const lbrace = try self.next(); // eat '{'
    std.debug.assert(lbrace.tag == .@"{");

    var props = std.ArrayList(Node.Index).init(self.allocator);
    defer props.deinit();

    var end_pos = lbrace.start + lbrace.len;

    var lookahead = self.peek();
    while (lookahead.tag != .@"}") : (lookahead = self.peek()) {
        switch (lookahead.tag) {
            .@"..." => {
                _ = try self.next(); // eat '...'
                const start_pos = lookahead.start;
                const expr = try self.lhsExpression();
                const end = self.nodes.items[@intFromEnum(expr)].end;
                const spread_expr = try self.addNode(.{ .spread_element = expr }, start_pos, end);
                try props.append(spread_expr);
                break; // spread element must be the last element
            },

            .identifier, .string_literal, .numeric_literal, .@"[" => {
                const prop = try self.destructuredPropertyDefinition();
                try props.append(prop);
            },

            else => {
                try self.emitDiagnostic(
                    lookahead.startCoord(self.source),
                    "Unexpected '{s}' while parsing destructured object pattern",
                    .{lookahead.toByteSlice(self.source)},
                );

                return ParseError.InvalidAssignmentTarget;
            },
        }

        const comma_or_rbrace = try self.expect2(.@"}", .@",");
        if (comma_or_rbrace.tag == .@"}") {
            end_pos = comma_or_rbrace.start + comma_or_rbrace.len;
            break;
        }
    }

    const destructured_props = try self.addNodeList(props.items);
    return self.addNode(
        .{ .object_pattern = destructured_props },
        lbrace.start,
        end_pos,
    );
}

fn unaryExpression(self: *Self) ParseError!Node.Index {
    const token = self.peek();
    switch (token.tag) {
        .kw_delete,
        .kw_typeof,
        .kw_void,
        .@"-",
        .@"+",
        .@"~",
        .@"!",
        => {
            const op_token = try self.next();
            const expr = try self.unaryExpression();
            const expr_end_pos = self.nodes.items[@intFromEnum(expr)].end;
            return try self.addNode(.{
                .unary_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = try self.addToken(op_token),
                },
            }, op_token.start, expr_end_pos);
        },
        else => return self.updateExpression(),
    }
}

/// The ECMASCript262 standard describes a syntax directed operation
/// called `AssignmentTargetType`, which determines if a given expression
/// is "SIMPLE", a.k.a, valid in contexts like the operand of `<expr>++`.
fn isExprSimple(self: *Self) ParseError!Node.Index {
    _ = self;
}

fn updateExpression(self: *Self) ParseError!Node.Index {
    const token = self.peek();
    if (token.tag == .@"++" or token.tag == .@"--") {
        const op_token = try self.next();
        const expr = try self.unaryExpression();
        const expr_end_pos = self.nodes.items[@intFromEnum(expr)].end;
        return self.addNode(.{
            .update_expr = ast.UnaryPayload{
                .operand = expr,
                .operator = try self.addToken(op_token),
            },
        }, op_token.start, expr_end_pos);
    }

    // post increment / decrement
    const expr_start_line = self.peek().line;
    const expr = try self.lhsExpression();
    const lookahead = self.peek();
    if ((lookahead.tag == .@"++" or lookahead.tag == .@"--") and
        lookahead.line == expr_start_line)
    {
        const op_token = try self.next();
        const expr_end_pos = self.nodes.items[@intFromEnum(expr)].end;
        return self.addNode(.{
            .post_unary_expr = .{
                .operand = expr,
                .operator = try self.addToken(op_token),
            },
        }, op_token.start, expr_end_pos);
    }

    return expr;
}

fn lhsExpression(self: *Self) ParseError!Node.Index {
    if (try self.tryNewExpression()) |expr| return expr;
    if (self.peek().tag == .kw_super) {
        return try self.superExpression();
    }

    var lhs_expr = try self.memberExpression();
    if (try self.tryCallExpression(lhs_expr)) |call_expr| {
        lhs_expr = call_expr;
    }

    if (self.isAtToken(.@"?.")) {
        lhs_expr = try self.optionalExpression(lhs_expr);
    }
    return lhs_expr;
}

fn superExpression(self: *Self) ParseError!Node.Index {
    const super_token = try self.next();
    std.debug.assert(super_token.tag == .kw_super);

    const super_args, const start, const end = try self.parseArgs();

    return self.addNode(.{
        .super_call_expr = super_args,
    }, start, end);
}

fn tryNewExpression(self: *Self) ParseError!?Node.Index {
    if (self.isAtToken(.kw_new)) {
        const new_token = try self.next(); // eat "new"
        const expr = try self.memberExpression();
        const expr_end_pos = self.nodes.items[@intFromEnum(expr)].end;
        return try self.addNode(.{
            .new_expr = .{
                .callee = expr,
                .arguments = if (self.isAtToken(.@"("))
                    try self.args()
                else
                    try self.addNode(.{ .arguments = null }, new_token.start, new_token.start),
            },
        }, new_token.start, expr_end_pos);
    }

    return null;
}

/// Try parsing a call expression. If the input is malformed, return a `ParseError`,
/// If no call expression was found, return `null`,
/// Otherwise, return the index of the call expression node.
/// NOTE: The call expression grammar might seem a little odd, because it
/// also has productions that parse member expressions:
/// https://262.ecma-international.org/15.0/index.html#prod-CallExpression
fn tryCallExpression(self: *Self, callee: Node.Index) ParseError!?Node.Index {
    const token = self.peek();
    if (token.tag != .@"(") return null;

    var call_expr = try self.coverCallAndAsyncArrowHead(callee);
    var lookahead = self.peek();
    while (lookahead.tag != .eof) : (lookahead = self.peek()) {
        switch (lookahead.tag) {
            .@"(" => call_expr = try self.completeCallExpression(call_expr),
            .@"[" => call_expr = try self.completeComputedMemberExpression(call_expr),
            .@"." => call_expr = try self.completeMemberExpression(call_expr),
            else => break,
        }
    }

    return call_expr;
}

fn completeCallExpression(self: *Self, callee: Node.Index) ParseError!Node.Index {
    const start_pos = self.nodes.items[@intFromEnum(callee)].start;
    const call_args = try self.args();
    const call_expr = ast.CallExpr{
        .arguments = call_args,
        .callee = callee,
    };
    const end_pos = self.nodes.items[@intFromEnum(call_args)].end;
    return self.addNode(.{ .call_expr = call_expr }, start_pos, end_pos);
}

// CoverCallAndAsyncArrowHead:  MemberExpression Arguments
fn coverCallAndAsyncArrowHead(self: *Self, callee: Node.Index) ParseError!Node.Index {
    const call_args = try self.args();
    const start_pos = self.nodes.items[@intFromEnum(callee)].start;
    const end_pos = self.nodes.items[@intFromEnum(call_args)].end;

    return self.addNode(.{
        .call_expr = .{
            .callee = callee,
            .arguments = call_args,
        },
    }, start_pos, end_pos);
}

/// https://262.ecma-international.org/15.0/index.html#prod-OptionalExpression
fn optionalExpression(self: *Self, object: Node.Index) ParseError!Node.Index {
    var expr = object;
    var lookahead = self.peek();
    while (lookahead.tag != .eof) : (lookahead = self.peek()) {
        switch (lookahead.tag) {
            .@"?." => expr = try self.completeOptionalChain(expr),
            else => return expr,
        }
    }

    return expr;
}

/// Assuming a `<object>?.<property>` has been consumed already, consume the
/// operators that are chained on top, and return a node which will be put into
/// an `optional_expr` field of `ast.Node`.
/// see: `Self.optionalChain`.
fn completeOptionalChain(self: *Self, prev_expr: Node.Index) ParseError!Node.Index {
    var expr = try self.optionalChain(prev_expr);
    const start_pos = self.nodes.items[@intFromEnum(expr)].start;

    var lookahead = self.peek();
    while (lookahead.tag != .eof) : (lookahead = self.peek()) {
        switch (lookahead.tag) {
            .@"[" => {
                const member_expr = try self.completeComputedMemberExpression(expr);
                const end_pos = self.nodes.items[@intFromEnum(member_expr)].end;
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"." => {
                const member_expr = try self.completeMemberExpression(expr);
                const end_pos = self.nodes.items[@intFromEnum(member_expr)].end;
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"(" => {
                const call_expr = try self.completeCallExpression(expr);
                const end_pos = self.nodes.items[@intFromEnum(call_expr)].end;
                expr = try self.addNode(.{ .optional_expr = call_expr }, start_pos, end_pos);
            },
            else => return expr,
        }
    }

    return expr;
}

/// Parse an OptionalExpression:
/// The expression before the `?.` operator is already parsed and passed as an argument.
///
/// See: https://262.ecma-international.org/15.0/index.html#prod-OptionalExpression
fn optionalChain(self: *Self, object: Node.Index) ParseError!Node.Index {
    const start_pos = self.nodes.items[@intFromEnum(object)].start;

    const chain_op = try self.next();
    std.debug.assert(chain_op.tag == .@"?.");

    const lookahead = self.peek();

    switch (lookahead.tag) {
        .@"(" => {
            const call_args = try self.args();
            const end_pos = self.nodes.items[@intFromEnum(call_args)].end;
            const call_expr = try self.addNode(.{
                .call_expr = .{
                    .arguments = call_args,
                    .callee = object,
                },
            }, start_pos, end_pos);
            return self.addNode(ast.NodeData{ .optional_expr = call_expr }, start_pos, end_pos);
        },
        .@"[" => {
            const expr = try self.completeComputedMemberExpression(object);
            const end_pos = self.nodes.items[@intFromEnum(expr)].end;
            return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
        },
        .identifier, .private_identifier => {
            const property_name_token = try self.next(); // eat the property name
            const end_pos = property_name_token.start + property_name_token.len;
            const expr = try self.addNode(.{ .member_expr = .{
                .object = object,
                .property = try self.addToken(lookahead),
            } }, start_pos, end_pos);
            return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
        },
        else => {
            try self.emitDiagnostic(
                lookahead.startCoord(self.source),
                "Expected a property access or all after ?., but got {s}\n",
                .{lookahead.toByteSlice(self.source)},
            );
            return ParseError.UnexpectedToken;
        },
    }
}

fn memberExpression(self: *Self) ParseError!Node.Index {
    var member_expr = try self.primaryExpression();
    var token = self.peek();
    while (token.tag != .eof) : (token = self.peek()) {
        switch (token.tag) {
            .@"." => member_expr = try self.completeMemberExpression(member_expr),
            .@"[" => member_expr = try self.completeComputedMemberExpression(member_expr),
            else => return member_expr,
        }
    }
    return member_expr;
}

fn completeMemberExpression(self: *Self, object: Node.Index) ParseError!Node.Index {
    const dot = try self.next(); // eat "."
    std.debug.assert(dot.tag == .@".");

    const start_pos = self.nodes.items[@intFromEnum(object)].start;

    const property_token_idx: Token.Index = blk: {
        const tok = try self.next();
        if (tok.tag == .identifier or tok.tag == .private_identifier) {
            break :blk try self.addToken(tok);
        }

        try self.emitDiagnostic(
            tok.startCoord(self.source),
            "Expected to see a property name after '.', got a '{s}' instead",
            .{tok.toByteSlice(self.source)},
        );
        return ParseError.UnexpectedToken;
    };

    const property_access = ast.PropertyAccess{
        .object = object,
        .property = property_token_idx,
    };

    const property_token = self.tokens.items[@intFromEnum(property_token_idx)];
    const end_pos = property_token.start + property_token.len;
    return self.addNode(.{ .member_expr = property_access }, start_pos, end_pos);
}

fn completeComputedMemberExpression(self: *Self, object: Node.Index) ParseError!Node.Index {
    const tok = try self.next(); // eat "["
    std.debug.assert(tok.tag == .@"[");

    const property = try self.expression();
    _ = try self.expect(.@"]");

    const property_access = ast.ComputedPropertyAccess{
        .object = object,
        .property = property,
    };

    const start_pos = self.nodes.items[@intFromEnum(object)].start;
    const end_pos = self.nodes.items[@intFromEnum(property)].end;
    return self.addNode(.{ .computed_member_expr = property_access }, start_pos, end_pos);
}

fn primaryExpression(self: *Self) ParseError!Node.Index {
    const token = try self.next();
    switch (token.tag) {
        .kw_this => return self.addNode(
            .{ .this = try self.addToken(token) },
            token.start,
            token.start + token.len,
        ),
        .identifier => {
            return self.addNode(
                .{ .identifier = try self.addToken(token) },
                token.start,
                token.start + token.len,
            );
        },
        .numeric_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => return self.addNode(
            .{ .literal = try self.addToken(token) },
            token.start,
            token.start + token.len,
        ),
        .@"[" => return self.arrayLiteral(token.start),
        .@"{" => return self.objectLiteral(token.start),
        .@"(" => return self.coverParenExprAndArrowParams(),
        .kw_function => return self.functionExpression(token.start, .{}),
        else => {
            try self.emitDiagnostic(
                token.startCoord(self.source),
                "expected an expression, found '{s}'",
                .{token.toByteSlice(self.source)},
            );
            return ParseError.UnexpectedToken;
        },
    }
}

fn coverParenExprAndArrowParams(self: *Self) ParseError!Node.Index {
    const expr = try self.expression();
    _ = try self.expect(.@")");
    return expr;
}

/// Parse an object literal, assuming the `{` has already been consumed.
/// https://262.ecma-international.org/15.0/index.html#prod-ObjectLiteral
fn objectLiteral(self: *Self, start_pos: u32) ParseError!Node.Index {
    const properties = try self.propertyDefinitionList();
    const closing_brace = try self.expect(.@"}");
    const end_pos = closing_brace.start + closing_brace.len;
    return try self.addNode(.{ .object_literal = properties }, start_pos, end_pos);
}

/// https://tc39.es/ecma262/#prod-PropertyDefinitionList
/// Parse a comma-separated list of properties.
/// Returns `null` if there's 0 properties in the object.
fn propertyDefinitionList(self: *Self) ParseError!?ast.NodeList {
    var property_defs = std.ArrayList(Node.Index).init(self.allocator);
    defer property_defs.deinit();

    const lookahead = self.peek();
    while (lookahead.tag != .eof) {
        switch (lookahead.tag) {
            .identifier => {
                try property_defs.append(try self.identifierProperty());
            },

            .@"[" => {
                _ = try self.next();
                const key = try self.assignmentExpression();
                _ = try self.expect(.@"]");

                const property = try self.completePropertyDef(
                    key,
                    .{ .is_computed = true },
                );
                try property_defs.append(property);
            },

            .numeric_literal, .string_literal => {
                const key_token = try self.next();
                const key = try self.addNode(
                    .{ .literal = try self.addToken(key_token) },
                    key_token.start,
                    key_token.start + key_token.len,
                );

                const property_expr = try self.completePropertyDef(key, .{});
                try property_defs.append(property_expr);
            },

            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();
                const start = ellipsis_tok.start;
                const end = self.nodes.items[@intFromEnum(expr)].end;
                try property_defs.append(try self.addNode(.{ .spread_element = expr }, start, end));
            },
            else => break,
        }

        const maybe_comma = self.peek();
        if (maybe_comma.tag == .@",") {
            _ = try self.next();
        } else {
            break;
        }
    }

    if (property_defs.items.len == 0) return null;
    return try self.addNodeList(property_defs.items);
}

/// Parse an object property that starts with an identifier (that may be "get" or "set").
fn identifierProperty(self: *Self) ParseError!Node.Index {
    const key_token = try self.next();
    std.debug.assert(key_token.tag == .identifier);

    const lookahead = self.peek();
    if (lookahead.tag != .@":" and lookahead.tag != .@"(" and
        lookahead.tag != .@"," and lookahead.tag != .@"}")
    {
        const maybe_getter_or_setter = try self.getterOrSetter(key_token);
        if (maybe_getter_or_setter) |getter_or_setter| {
            return getter_or_setter;
        }

        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "Unexpected '{s}' in property definition",
            .{self.current_token.toByteSlice(self.source)},
        );
        return ParseError.UnexpectedToken;
    }

    const key_end_pos = key_token.start + key_token.len;
    const key = try self.addNode(
        .{ .identifier = try self.addToken(key_token) },
        key_token.start,
        key_end_pos,
    );

    const lookahead_tag = self.peek().tag;
    switch (lookahead_tag) {
        .@":", .@"(" => {
            return self.completePropertyDef(key, .{
                .is_method = lookahead_tag == .@"(",
            });
        },

        else => {
            const kv_node = ast.PropertyDefinition{
                .key = key,
                .value = key,
                .flags = .{ .is_shorthand = true },
            };
            return self.addNode(
                .{ .object_property = kv_node },
                key_token.start,
                key_end_pos,
            );
        },
    }
}

/// Tries to parse a getter or setter, assuming `token` is an identifier.
/// If no getter or setter is found, returns `null`.
/// If there is a parse error, emits a diagnostic and returns the error.
fn getterOrSetter(self: *Self, token: Token) ParseError!?Node.Index {
    const kind: ast.PropertyDefinitionKind = blk: {
        const token_str = token.toByteSlice(self.source);
        if (std.mem.eql(u8, token_str, "get")) {
            break :blk .get;
        }

        if (std.mem.eql(u8, token_str, "set")) {
            break :blk .set;
        }

        return null;
    };

    const key = try self.classElementName();
    return try self.parseMethodBody(
        key,
        .{ .is_method = true, .kind = kind },
    );
}

/// https://tc39.es/ecma262/#prod-ClassElementName
fn classElementName(self: *Self) ParseError!Node.Index {
    const token = try self.next();
    switch (token.tag) {
        .identifier, .private_identifier => {
            return self.addNode(
                .{ .identifier = try self.addToken(token) },
                token.start,
                token.start + token.len,
            );
        },
        .@"[" => {
            const expr = try self.assignmentExpression();
            _ = try self.expect(.@"]");
            return expr;
        },
        else => {
            try self.emitDiagnostic(
                token.startCoord(self.source),
                "Expected property name, got '{s}'",
                .{token.toByteSlice(self.source)},
            );
            return ParseError.UnexpectedToken;
        },
    }
}

/// Parse a method body, assuming we're at the '(' node.
/// Returns an `object_property` Node.
fn parseMethodBody(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
) ParseError!Node.Index {
    std.debug.assert(self.current_token.tag == .@"(" and flags.is_method);

    const start_pos = self.peek().start;
    const func_expr = try self.parseFunctionBody(start_pos, null, .{});
    const end_pos = self.nodeSpan(func_expr).end;

    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = func_expr,
        .flags = flags,
    };

    const key_start = self.nodes.items[@intFromEnum(key)].start;
    return self.addNode(
        .{ .object_property = kv_node },
        key_start,
        end_pos,
    );
}

/// Assuming that the key has been parsed, complete the property definition.
fn completePropertyDef(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
) ParseError!Node.Index {
    if (self.current_token.tag == .@"(") {
        return self.parseMethodBody(key, .{
            .is_method = true,
            .is_computed = flags.is_computed,
            .is_shorthand = flags.is_shorthand,
            .kind = flags.kind,
        });
    }

    _ = try self.expect(.@":");

    const value = try self.assignmentExpression();
    const start_pos = self.nodes.items[@intFromEnum(key)].start;
    const end_pos = self.nodes.items[@intFromEnum(value)].end;
    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = value,
        .flags = flags,
    };
    return self.addNode(.{ .object_property = kv_node }, start_pos, end_pos);
}

/// Parse an ArrayLiteral:
/// https://262.ecma-international.org/15.0/index.html#prod-ArrayLiteral
fn arrayLiteral(self: *Self, start_pos: u32) ParseError!Node.Index {
    var elements = std.ArrayList(Node.Index).init(self.allocator);
    defer elements.deinit();

    var end_pos = start_pos;
    while (true) {
        while (self.isAtToken(.@",")) {
            // elision: https://262.ecma-international.org/15.0/index.html#prod-Elision
            const comma = try self.next();
            try elements.append(try self.addNode(
                .{ .empty_array_item = {} },
                comma.start,
                comma.start + comma.len,
            ));
        }

        if (self.isAtToken(.@"]")) {
            const end_tok = try self.next();
            end_pos = end_tok.start + end_tok.len;
            break;
        }

        switch (self.peek().tag) {
            // Spread element
            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();
                const start = ellipsis_tok.start;
                const end = self.nodes.items[@intFromEnum(expr)].end;
                try elements.append(try self.addNode(.{ .spread_element = expr }, start, end));
            },

            else => {
                const item = try self.assignmentExpression();
                try elements.append(item);
            },
        }

        const next_token = try self.expect2(.@",", .@"]");
        if (next_token.tag == .@"]") {
            end_pos = next_token.start + next_token.len;
            break;
        }
    }

    const nodes = try self.addNodeList(elements.items);
    return self.addNode(.{ .array_literal = nodes }, start_pos, end_pos);
}

/// Assuming the parser is at the `function` keyword,
/// parse a function expression.
fn functionExpression(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    const name_token: ?Token.Index =
        if (self.current_token.tag == .identifier)
        try self.addToken(try self.next())
    else
        null;
    return self.parseFunctionBody(start_pos, name_token, flags);
}

/// parses the arguments and body of a function expression (or declaration),
/// assuming the `function` keyword (and/or the function/method name) has been consumed.
fn parseFunctionBody(
    self: *Self,
    start_pos: u32,
    name_token: ?Token.Index,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    const params = try self.parseFormalParameters();
    if (flags.is_arrow) unreachable; // not supported yet :)

    // Allow return statements inside function
    const ctx = self.context;
    defer self.setContext(ctx);
    self.context.@"return" = true;

    const body = try self.blockStatement();
    const end_pos = self.nodeSpan(body).end;

    const function_data = try self.addExtraData(
        ast.ExtraData{
            .function = .{
                .name = name_token,
                .flags = flags,
            },
        },
    );

    return self.addNode(.{
        .function_expr = .{
            .parameters = params,
            .body = body,
            .info = function_data,
        },
    }, start_pos, end_pos);
}

/// Starting with the '(' token , parse formal parameters of a function.
fn parseFormalParameters(self: *Self) ParseError!Node.Index {
    const lparen = try self.expect(.@"(");
    const start_pos = lparen.start;

    var params = std.ArrayList(Node.Index).init(self.allocator);
    defer params.deinit();

    var lookahead = self.peek();
    while (lookahead.tag != .@")" and lookahead.tag != .eof) : (lookahead = self.peek()) {
        const param = try self.assignmentLhsExpr();
        try params.append(param);
    }

    const rparen = try self.expect(.@")");
    const end_pos = rparen.start + rparen.len;

    const param_list = if (params.items.len > 0)
        try self.addNodeList(params.items)
    else
        null;

    return self.addNode(.{ .parameters = param_list }, start_pos, end_pos);
}

/// Get a pointer to a node by its index.
/// The returned value can be invalidated by any call to `addNode`, `restoreState`, `addNodeList`.
fn getNode(self: *const Self, index: Node.Index) *const ast.Node {
    // TODO: should this return a non-pointer instead?
    return &self.nodes.items[@intFromEnum(index)];
}

pub fn getToken(self: *const Self, index: Token.Index) Token {
    return self.tokens.items[@intFromEnum(index)];
}

fn nodeSpan(self: *const Self, index: Node.Index) types.Span {
    const node = &self.nodes.items[@intFromEnum(index)];
    return .{ .start = node.start, .end = node.end };
}

fn args(self: *Self) ParseError!Node.Index {
    const args_node, const start, const end = try self.parseArgs();
    return self.addNode(.{ .arguments = args_node }, start, end);
}

fn parseArgs(self: *Self) ParseError!struct { ast.NodeList, u32, u32 } {
    const start_pos = (try self.expect(.@"(")).start;

    var arg_list = std.ArrayList(Node.Index).init(self.allocator);
    defer arg_list.deinit();

    while (!self.isAtToken(.@")")) {
        const expr = try self.assignmentExpression();
        try arg_list.append(expr);
        if (!self.isAtToken(.@","))
            break;
        _ = try self.next(); // eat ','
    }

    const close_paren = try self.expect(.@")"); // eat closing ')'
    const end_pos = close_paren.start + close_paren.len;

    return .{ try self.addNodeList(arg_list.items), start_pos, end_pos };
}

/// make a right associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeRightAssoc(
    comptime toktag: Token.Tag,
    comptime l: *const ParseFn,
) *const ParseFn {
    const Parselet = struct {
        fn parseFn(self: *Self) ParseError!Node.Index {
            var node = try l(self);

            var token = self.peek();
            while (true) : (token = self.peek()) {
                if (token.tag != toktag) break;
                _ = try self.next();

                const rhs = try parseFn(self);
                const start_pos = self.nodes.items[@intFromEnum(node)].start;
                const end_pos = self.nodes.items[@intFromEnum(rhs)].end;
                node = try self.addNode(.{
                    .binary_expr = .{
                        .lhs = node,
                        .rhs = rhs,
                        .operator = try self.addToken(token),
                    },
                }, start_pos, end_pos);
            }

            return node;
        }
    };

    return &Parselet.parseFn;
}

/// make a left associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeLeftAssoc(
    comptime tag_min: Token.Tag,
    comptime tag_max: Token.Tag,
    comptime nextFn: *const ParseFn,
) *const ParseFn {
    const min: u32 = @intFromEnum(tag_min);
    const max: u32 = @intFromEnum(tag_max);

    const S = struct {
        fn parseFn(self: *Self) ParseError!Node.Index {
            var node = try nextFn(self);

            var token = self.peek();
            while (true) : (token = self.peek()) {
                if (@intFromEnum(token.tag) >= min and @intFromEnum(token.tag) <= max) {
                    _ = try self.next();
                    const rhs = try nextFn(self);

                    const start_pos = self.nodes.items[@intFromEnum(node)].start;
                    const end_pos = self.nodes.items[@intFromEnum(rhs)].end;
                    node = try self.addNode(.{
                        .binary_expr = .{
                            .lhs = node,
                            .rhs = rhs,
                            .operator = try self.addToken(token),
                        },
                    }, start_pos, end_pos);
                } else {
                    break;
                }
            }

            return node;
        }
    };

    return &S.parseFn;
}

const t = std.testing;

const pretty = @import("./pretty.zig");
fn runTestOnFile(tests_dir: std.fs.Dir, file_path: []const u8) !void {
    const source_code = try tests_dir.readFileAlloc(
        t.allocator,
        file_path,
        std.math.maxInt(u32),
    );
    defer t.allocator.free(source_code);

    var parser = try Self.init(t.allocator, source_code, file_path);
    defer parser.deinit();

    const root_node = try parser.expression();
    const pretty_ast = try pretty.toJsonString(t.allocator, &parser, root_node);
    defer t.allocator.free(pretty_ast);

    // The first line is a comment that has the expected JSON stringified AST.
    const first_line_len = std.mem.indexOfScalar(u8, source_code, '\n') orelse unreachable;
    const expected_json_str = source_code[2..first_line_len];

    try t.expectEqualStrings(expected_json_str, pretty_ast);
}

// -----
// Tests
// -----

test StringHelper {
    _ = StringHelper;
}

test parse {
    var root_dir = std.fs.cwd();
    var tests_dir = try root_dir.openDir("expression-tests", .{});
    defer tests_dir.close();

    var iter = tests_dir.iterate();
    while (try iter.next()) |entry| {
        if (!std.mem.eql(u8, std.fs.path.extension(entry.name), ".js")) {
            continue;
        }

        runTestOnFile(tests_dir, entry.name) catch |err| {
            std.debug.print("Error comparing ASTs for file: {s}\n", .{entry.name});
            return err;
        };
    }
}
