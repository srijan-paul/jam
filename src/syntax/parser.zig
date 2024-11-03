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
    IllegalAwait,
    IllegalFatArrow,
    InvalidAssignmentTarget,
    InvalidArrowParameters,
    InvalidArrowFunction,
    InvalidSetter,
    InvalidGetter,
    InvalidObject,
    InvalidLoopLhs,
    InvalidPropertyName,
    LetInStrictMode,
    UnexpectedPattern,
    ExpectedSemicolon,
} || Tokenizer.Error;
const ParseFn = fn (self: *Self) ParseError!Node.Index;

/// An error or warning raised by the Parser.
pub const Diagnostic = struct {
    /// line/col position where error occurred.
    coord: types.Coordinate,
    /// Message reported by the parser.
    message: []const u8,
};

/// Represents the currently active set of grammatical parameters
/// in the parser state.
/// https://tc39.es/ecma262/#sec-grammatical-parameters
pub const ParseContext = packed struct(u16) {
    /// Is a ReturnStatement allowed in the current context?
    /// Unlike `await`, `return` is always parsed as a keyword, regardles of context.
    @"return": bool = false,
    /// Is a BreakStatement allowed in the current context?
    @"break": bool = false,
    /// Is a ContinueStatement allowed in the current context?
    @"continue": bool = false,
    /// Is an `await` parsed as an identifier or as a keyword?
    is_await_reserved: bool = false,
    /// Is `yield` parsed as an identifier or as a keyword?
    is_yield_reserved: bool = false,
    /// Are we parsing a module? (`false` = parsing a script).
    module: bool = false,
    /// Are we in strict mode?
    strict: bool = false,
    /// Are `in` statements allowed?
    /// Used to parse for-in loops.
    in: bool = true,
    /// Is an assignment pattern allowed in the current context?
    is_pattern_allowed: bool = false,
    _: u7 = 0,
};

// arranged in highest to lowest binding order

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

// this one has to be hand-written to disallow 'in' inside for-loop iterators.
// see: forStatement
fn relationalExpr(self: *Self) ParseError!Node.Index {
    var node = try shiftExpr(self);

    const rel_op_start: u32 = @intFromEnum(Token.Tag.relational_start);
    const rel_op_end: u32 = @intFromEnum(Token.Tag.relational_end);

    var token = self.peek();
    while (true) : (token = self.peek()) {
        const itag: u32 = @intFromEnum(token.tag);
        if (itag > rel_op_start and itag < rel_op_end) {

            // `in` expressions are not allowed when parsing the LHS
            // for `for-loop` iterators.
            if (token.tag == .kw_in and !self.context.in) {
                break;
            }

            self.current_destructure_kind =
                @enumFromInt(self.current_destructure_kind.asU8() |
                DestructureKind.cannot_destruct.asU8());

            const op_token = try self.next();
            const rhs = try shiftExpr(self);
            const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
            const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
            const binary_pl: ast.BinaryPayload = .{
                .lhs = node,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            };

            node = try self.addNode(.{ .binary_expr = binary_pl }, start_pos, end_pos);
        } else {
            break;
        }
    }

    return node;
}

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
nodes: std.MultiArrayList(Node),
/// Extra information about a node, if any.
extra_data: std.ArrayList(ast.ExtraData),
/// List of tokens that are necessary to keep around
/// e.g - identifiers, literals, function names, etc.
tokens: std.ArrayList(Token),
/// Arguments for function calls, new-expressions, etc.
node_lists: std.ArrayList(Node.Index),
/// List of error messages and warnings generated during parsing.
diagnostics: std.ArrayList(Diagnostic),
/// The token that we're currently at.
/// Calling `next()` or `peek()` will return this token.
current_token: Token,
/// The next token that we're going to read.
next_token: Token,
/// Line number of the token that was consumed previously.
/// When being accessed, this property can be thought of as the starting line
/// of the first token in the expression/statement that was last parsed.
/// Useful for Automatic Semicolon Insertion.
prev_token_line: u32 = 0,
/// Helper struct to manage, escape, and compare strings in source code.
strings: StringHelper,
/// The current grammatical context of the parser. See struct `ParseContext`.
context: ParseContext = .{},
/// The kind of destructuring that is allowed for the expression
/// that was just parsed by the parser.
current_destructure_kind: DestructureKind = .can_destruct,

const DestructureKind = enum(u8) {
    can_destruct = 0b00,
    must_destruct = 0b01,
    cannot_destruct = 0b10,
    bad_object_or_pattern = 0b11,

    inline fn asU8(self: DestructureKind) u8 {
        return @intFromEnum(self);
    }
};

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
        .nodes = .{},
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
    self.nodes.deinit(self.allocator);
    self.tokens.deinit();
    self.node_lists.deinit();
    self.extra_data.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit();
    self.strings.deinit();
}

pub fn parse(self: *Self) !Node.Index {
    var statements = std.ArrayList(Node.Index).init(self.allocator);
    defer statements.deinit();

    while (self.current_token.tag != .eof) {
        const stmt = try self.statement();
        try statements.append(stmt);
    }

    const stmt_list = try self.addSubRange(statements.items);
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
    return switch (self.peek().tag) {
        .@"{" => self.blockStatement(),
        .@";" => self.emptyStatement(),
        .kw_if => self.ifStatement(),
        .kw_for => self.forStatement(),
        .kw_while => self.whileStatement(),
        .kw_debugger => {
            const token = try self.next();
            const end_pos = try self.semiColon(token.start + token.len);
            return self.addNode(
                .{ .debugger_statement = {} },
                token.start,
                end_pos,
            );
        },
        .kw_async => {
            if (self.next_token.tag == .kw_function) {
                const async_token = try self.next(); // eat 'async'
                _ = try self.next(); // eat 'function'
                return self.functionDeclaration(async_token.start, .{ .is_async = true });
            }
            return self.expressionStatement();
        },
        .kw_function => {
            const fn_token = try self.next();
            return self.functionDeclaration(fn_token.start, .{});
        },
        .kw_return => self.returnStatement(),
        .kw_break => self.breakStatement(),
        .kw_let => self.letStatement(),
        .kw_var, .kw_const => self.variableStatement(try self.next()),
        // TODO: right now, if expression parsing fails, the error message we get is:
        // "Expected an expression, found '<token>'."
        // This error message should be improved.
        else => self.expressionStatement(),
    };
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

const ForLoopKind = enum {
    for_in,
    for_of,
    basic,
};

/// Parse a for loop. Returns an index of:
/// ForOfStatement / ForInStatement / ForStatement
fn forStatement(self: *Self) ParseError!Node.Index {
    const for_kw = try self.next();
    std.debug.assert(for_kw.tag == .kw_for);

    const loop_kind, const iterator = try self.forLoopIterator();

    const saved_context = self.context;
    defer self.setContext(saved_context);

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.statement();
    const start_pos = for_kw.start;
    const end_pos = self.nodes.items(.end)[@intFromEnum(body)];

    const for_payload = ast.ForStatement{
        .body = body,
        .iterator = iterator,
    };

    const node_data: ast.NodeData = switch (loop_kind) {
        .basic => .{ .for_statement = for_payload },
        .for_of => .{ .for_of_statement = for_payload },
        .for_in => .{ .for_in_statement = for_payload },
    };

    return self.addNode(node_data, start_pos, end_pos);
}

/// Once a 'for' keyword has been consumed, this parses the part inside '()', including the parentheses.
/// Returns a tuple where the first item is the kind of for loop (for-in, for-of, or basic),
/// and the second item is the iterator (index of an `ast.ExtraData`).
fn forLoopIterator(self: *Self) ParseError!struct { ForLoopKind, ast.ExtraData.Index } {
    _ = try self.expect(.@"(");

    // Forbid for (let let of x) {} / for (let let = ...)
    if (self.current_token.tag == .kw_let and self.next_token.tag == .kw_let) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "'let' cannot be used as a name in 'let' or 'const' declarations",
            .{},
        );
        return ParseError.UnexpectedToken;
    }

    // Check for productions that start with a variable declarator.
    // for (var/let/const x = 0; x < 10; x++)
    // for (let/let/const x in y)
    // for (let/let/const x of y)
    const curr = self.current_token.tag;
    if ((curr == .kw_let and self.isDeclaratorStart(self.next_token.tag)) or
        curr == .kw_var or
        curr == .kw_const)
    {
        var loop_kind = ForLoopKind.basic;
        const iterator = try self.completeLoopIterator(&loop_kind);
        return .{ loop_kind, iterator };
    }

    // 'for (;' indicates a basic 3-part for loop with empty initializer.
    if (curr == .@";") {
        const iterator = try self.completeBasicLoopIterator(Node.Index.empty);
        return .{ ForLoopKind.basic, iterator };
    }

    const context = self.context;
    defer self.setContext(context);

    // disallow `in` expressions.
    // The next `in` token is instead the iterator of the for loop.
    self.context.in = false;

    // for (<LeftHandSideExpression> <in/of> <Expression>)
    // for (<Expression> <in/of> <Expression>)
    // for (<Expression>;<Expression>;<Expression>);
    const expr = try self.forLoopStartExpression();

    switch (self.current_token.tag) {
        .@";" => {
            // parse <Expression> ; <Expression> ; <Expression>
            if (self.current_destructure_kind == .must_destruct) {
                const expr_start = self.nodes.items(.start)[@intFromEnum(expr)];
                try self.emitDiagnostic(
                    util.offsets.byteIndexToCoordinate(self.source, expr_start),
                    "Unexpected pattern",
                    .{},
                );

                return ParseError.UnexpectedPattern;
            }

            const iterator = try self.completeBasicLoopIterator(expr);
            return .{ ForLoopKind.basic, iterator };
        },

        .kw_of, .kw_in => {
            // parse <Expression> <in/of> <Expression>
            const cannot_destruct = self.current_destructure_kind.asU8() &
                DestructureKind.cannot_destruct.asU8();

            if (cannot_destruct == 1) {
                try self.emitDiagnostic(
                    // TODO: use the expression's start coordinate instead, use util.offsets
                    self.current_token.startCoord(self.source),
                    "left hand side of for-loop must be a name or assignment pattern",
                    .{},
                );
                return ParseError.InvalidLoopLhs;
            }

            const loop_kind = if (self.isAtToken(.kw_of))
                ForLoopKind.for_of
            else
                ForLoopKind.for_in;

            self.context.in = true;

            _ = try self.next();
            const rhs = try self.expression();
            _ = try self.expect(.@")");

            const iterator = try self.addExtraData(.{
                .for_in_of_iterator = .{
                    .left = expr,
                    .right = rhs,
                },
            });
            return .{ loop_kind, iterator };
        },

        else => {
            try self.emitBadTokenDiagnostic("'of', 'in' or ';'", &self.current_token);
            return ParseError.UnexpectedToken;
        },
    }
}

/// Parse the first expression that can appear in a for-loop iterator after
/// the opening parenthesis.
/// This expression may be followed a ';' token (indicating a 3-step for loop),
/// or by 'in' or 'of' (indicating a for-in or for-of loop).
fn forLoopStartExpression(self: *Self) ParseError!Node.Index {
    const first_expr = try self.assignmentExpression();

    const must_destruct = self.current_destructure_kind.asU8() &
        DestructureKind.must_destruct.asU8() == 1;
    if (self.isAtToken(.@",") and !must_destruct) {
        return self.completeSequenceExpr(first_expr);
    }

    return first_expr;
}

fn completeLoopIterator(self: *Self, loop_kind: *ForLoopKind) ParseError!ast.ExtraData.Index {
    const decl_kw = try self.next(); // eat 'let', 'var' or 'const'
    std.debug.assert(decl_kw.tag == .kw_let or
        decl_kw.tag == .kw_var or
        decl_kw.tag == .kw_const);

    const lhs = try self.assignmentLhsExpr();
    const lhs_span = self.nodeSpan(lhs);
    switch (self.current_token.tag) {
        .kw_in, .kw_of => {
            loop_kind.* = if (self.current_token.tag == .kw_in)
                // TODO: handle the [In] grammar attribute
                ForLoopKind.for_in
            else
                ForLoopKind.for_of;

            const declarator = try self.addNode(
                .{ .variable_declarator = .{ .lhs = lhs, .init = null } },
                lhs_span.start,
                lhs_span.end,
            );

            const decl_range = try self.addSubRange(&[_]Node.Index{declarator});
            const declaration = try self.addNode(
                .{ .variable_declaration = .{ .kind = varDeclKind(decl_kw.tag), .declarators = decl_range } },
                decl_kw.start,
                lhs_span.end,
            );

            _ = try self.next(); // eat 'in' or 'of'
            const rhs = try self.expression();
            _ = try self.expect(.@")");
            return self.addExtraData(.{
                .for_in_of_iterator = .{ .left = declaration, .right = rhs },
            });
        },
        else => {
            var end_pos = lhs_span.end;
            var rhs: ?Node.Index = null;
            if (self.isAtToken(.@"=")) {
                _ = try self.next();
                const init_expr = try self.assignmentExpression();

                if (self.current_destructure_kind == .must_destruct) {
                    try self.emitBadDestructureDiagnostic(init_expr);
                    return ParseError.UnexpectedPattern;
                }

                end_pos = self.nodeSpan(init_expr).end;
                rhs = init_expr;
            }

            const first_decl = try self.addNode(
                .{ .variable_declarator = .{ .lhs = lhs, .init = rhs } },
                decl_kw.start,
                end_pos,
            );

            const for_init = try self.completeLoopInitializer(decl_kw, first_decl);
            return self.completeBasicLoopIterator(for_init);
        },
    }
}

fn completeBasicLoopIterator(self: *Self, for_init: Node.Index) ParseError!ast.ExtraData.Index {
    _ = try self.expect(.@";");

    const for_cond = switch (self.current_token.tag) {
        .@";" => Node.Index.empty,
        else => try self.expression(),
    };

    _ = try self.expect(.@";");

    const for_update = switch (self.current_token.tag) {
        .@")" => Node.Index.empty,
        else => try self.expression(),
    };

    _ = try self.expect(.@")");

    return self.addExtraData(ast.ExtraData{
        .for_iterator = ast.ForIterator{
            .init = for_init,
            .condition = for_cond,
            .update = for_update,
        },
    });
}

/// Parse the rest of the for loop initializer.
/// Example:
/// ```js
/// for (let i = 0, j = 0; i < 10; i++,j++);
/// //           ^
/// // already parsed upto here, this function parses the rest
/// ```
/// Given the 'var'/'let'/'const' keyword,
/// and the first declarator, parse the rest of the declarators (if any)
/// and return a var decl node that represents a loop initializer.
fn completeLoopInitializer(self: *Self, kw: Token, first_decl: Node.Index) ParseError!Node.Index {
    var decl_nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer decl_nodes.deinit();

    try decl_nodes.append(first_decl);

    while (self.current_token.tag == .@",") {
        _ = try self.next();
        const decl = try self.variableDeclarator();
        try decl_nodes.append(decl);
    }

    const decls = try self.addSubRange(decl_nodes.items);

    const last_decl = decl_nodes.items[decl_nodes.items.len - 1];
    const end_pos = self.nodes.items(.end)[@intFromEnum(last_decl)];

    return self.addNode(
        .{
            .variable_declaration = .{
                .kind = varDeclKind(kw.tag),
                .declarators = decls,
            },
        },
        kw.start,
        end_pos,
    );
}

fn whileStatement(self: *Self) ParseError!Node.Index {
    const while_kw = try self.next();
    std.debug.assert(while_kw.tag == .kw_while);

    _ = try self.expect(.@"(");
    const cond = try self.expression();
    _ = try self.expect(.@")");

    const saved_context = self.context;
    defer self.setContext(saved_context);

    self.context.@"break" = true;
    self.context.@"continue" = true;

    // todo: perform a labelled statement check here.
    const body = try self.statement();
    const end_pos = self.nodeSpan(body).end;

    return self.addNode(
        .{ .while_statement = .{ .condition = cond, .body = body } },
        while_kw.start,
        end_pos,
    );
}

/// parse a VariableStatement, where `kw` is the keyword used to declare the variable (let, var, or const).
fn variableStatement(self: *Self, kw: Token) ParseError!Node.Index {
    std.debug.assert(kw.tag == .kw_let or kw.tag == .kw_var or kw.tag == .kw_const);

    const decls = try self.variableDeclaratorList();
    const last_decl = decls.asSlice(self)[0];

    var end_pos: u32 = self.nodes.items(.end)[@intFromEnum(last_decl)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(
        .{
            .variable_declaration = .{
                .kind = varDeclKind(kw.tag),
                .declarators = decls,
            },
        },
        kw.start,
        end_pos,
    );
}

fn variableDeclaratorList(self: *Self) ParseError!ast.SubRange {
    var declarators = std.ArrayList(Node.Index).init(self.allocator);
    defer declarators.deinit();

    while (true) {
        const decl = try self.variableDeclarator();
        try declarators.append(decl);
        if (self.current_token.tag == .@",") {
            _ = try self.next();
        } else {
            break;
        }
    }

    const decls = try self.addSubRange(declarators.items);
    return decls;
}

fn varDeclKind(tag: Token.Tag) ast.VarDeclKind {
    return switch (tag) {
        .kw_let => .let,
        .kw_var => .@"var",
        .kw_const => .@"const",
        else => unreachable,
    };
}

/// Parse a statement that starts with the `let` keyword.
/// This may not necessarily be a variable declaration, as `let`
/// is also a valid identifier.
fn letStatement(self: *Self) ParseError!Node.Index {
    std.debug.assert(self.current_token.tag == .kw_let);
    if (self.isDeclaratorStart(self.next_token.tag)) {
        const let_kw = try self.next();
        return self.variableStatement(let_kw);
    }

    if (self.isInStrictMode()) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "'let' can only be used to declare variables in strict mode",
            .{},
        );
        return ParseError.LetInStrictMode;
    }

    // TODO: also support labelled statements where the label is `let`
    return self.expressionStatement();
}

fn variableDeclarator(self: *Self) ParseError!Node.Index {
    const lhs = try self.assignmentLhsExpr();

    const start_pos = self.nodeSpan(lhs).start;
    var end_pos = self.nodeSpan(lhs).end;

    var rhs: ?Node.Index = null;
    if (self.isAtToken(.@"=")) {
        _ = try self.next();
        const init_expr = try self.assignmentExpression();

        if (self.current_destructure_kind == .must_destruct) {
            try self.emitBadDestructureDiagnostic(init_expr);
            return ParseError.UnexpectedPattern;
        }

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
    const expr_node = self.getNode(expr);

    const end_pos = try self.semiColon(expr_node.end);

    return addNode(
        self,
        .{ .expression_statement = expr },
        expr_node.start,
        end_pos,
    );
}

/// BlockStatement:
///   '{' StatementList? '}'
fn blockStatement(self: *Self) ParseError!Node.Index {
    const lbrac = try self.expect(.@"{");
    const start_pos = lbrac.start;

    var statements = std.ArrayList(Node.Index).init(self.allocator);
    defer statements.deinit();

    while (self.peek().tag != .@"}" and self.peek().tag != .eof) {
        const stmt = try self.statement();
        try statements.append(stmt);
    }

    const rbrace = try self.expect(.@"}");
    const end_pos = rbrace.start + rbrace.len;

    if (statements.items.len == 0) {
        return self.addNode(.{ .block_statement = null }, start_pos, end_pos);
    }

    const stmt_list_node = try self.addSubRange(statements.items);
    const block_node = ast.NodeData{ .block_statement = stmt_list_node };
    return self.addNode(block_node, start_pos, end_pos);
}

/// Assuming the parser is at the `function` keyword,
/// parse a function declaration statement.
fn functionDeclaration(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.next();
        fn_flags.is_generator = true;
    }

    const name_token = blk: {
        const token = try self.next();
        if (token.tag == .identifier or self.isKeywordIdentifier(token.tag)) {
            break :blk try self.addToken(token);
        }

        try self.emitBadTokenDiagnostic("function name", &token);
        return ParseError.UnexpectedToken;
    };

    return self.parseFunctionBody(start_pos, name_token, fn_flags, true);
}

/// ReturnStatement:
///    'return' ';'
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

    if (self.current_token.line != return_kw.line or
        self.current_token.tag == .@";" or
        self.current_token.tag == .@"}")
    {
        const end_pos = try self.semiColon(return_kw.start + return_kw.len);
        return self.addNode(.{ .return_statement = null }, return_kw.start, end_pos);
    }

    const operand = try self.expression();
    const end_pos = try self.semiColon(self.nodeSpan(operand).end);
    return self.addNode(.{ .return_statement = operand }, return_kw.start, end_pos);
}

fn breakStatement(self: *Self) ParseError!Node.Index {
    const break_kw = try self.next();
    std.debug.assert(break_kw.tag == .kw_break);

    if (!self.context.@"break") {
        try self.emitDiagnostic(
            break_kw.startCoord(self.source),
            "'break' is not allowed outside loops and switch statements",
            .{},
        );
        return ParseError.IllegalReturn;
    }

    const start_pos = break_kw.start;
    const end_pos = try self.semiColon(break_kw.start + break_kw.len);
    return self.addNode(.{ .break_statement = {} }, start_pos, end_pos);
}

fn continueStatement(self: *Self) ParseError!Node.Index {
    const continue_kw = try self.next();
    std.debug.assert(continue_kw.tag == .kw_continue);

    if (!self.context.@"continue") {
        try self.emitDiagnostic(
            continue_kw.startCoord(self.source),
            "'continue' is not allowed outside loops",
            .{},
        );
        return ParseError.IllegalReturn;
    }

    const start_pos = continue_kw.start;
    const end_pos = try self.semiColon(continue_kw.start + continue_kw.len);
    return self.addNode(.{ .continue_statement = {} }, start_pos, end_pos);
}

// ------------------------
// Common helper functions
// ------------------------

/// Helper function to finish a statement with a semi-colon.
/// `end_pos` is the end-position of the statement node,
/// if a semi-colon is found, returns the end position of the semi-colon,
/// otherwise returns `end_pos`.
/// If the ASI rules cannot be applied, returns a parse error.
fn semiColon(self: *Self, end_pos: u32) ParseError!u32 {
    return if (try self.eatSemiAsi()) |semi|
        semi.end
    else
        end_pos;
}

/// Eat a semicolon token and return its span.
/// If there if there is no semi-colon token, it will check if we're allowed to assume an implicit ';' exists
/// https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-automatic-semicolon-insertion
/// If no semi-colon can be inserted, a parse error is returned instead.
fn eatSemiAsi(self: *Self) ParseError!?types.Span {
    if (self.current_token.tag == .@";") {
        const semicolon = try self.next();
        return .{
            .start = semicolon.start,
            .end = semicolon.start + semicolon.len,
        };
    }

    if (self.current_token.line != self.prev_token_line or
        self.current_token.tag == .@"}" or
        self.current_token.tag == .eof)
        return null;

    try self.emitBadTokenDiagnostic("a ';' or a newline", &self.current_token);
    return ParseError.ExpectedSemicolon;
}

/// Returns whether a token with the tag `tag` can start a variable declarator
/// ({, or [, or an identifier).
fn isDeclaratorStart(self: *Self, tag: Token.Tag) bool {
    return tag == .@"[" or tag == .@"{" or
        tag == .identifier or self.isKeywordIdentifier(tag);
}

fn setContext(self: *Self, context: ParseContext) void {
    self.context = context;
    self.tokenizer.context = context;
}

/// Returns whether the parser is currently parsing strict mode code.
fn isInStrictMode(self: *const Self) bool {
    return self.context.strict or self.context.module;
}

/// Returns `true` of `tag` is a keyword that can be allowed as an identifier
/// in the current context.
fn isKeywordIdentifier(self: *const Self, tag: Token.Tag) bool {
    switch (tag) {
        .kw_await => return !self.context.is_await_reserved,
        .kw_yield => return !self.context.is_yield_reserved,
        else => {
            if (tag.isContextualKeyword()) return true;
            if (self.isInStrictMode()) return false;
            // check if tag is a keyword, but that keyword can be used as an identifier in non-strict mode.
            return tag.isStrictModeKeyword() or tag == .kw_let;
        },
    }
}

/// Reserve a slot for node, then return the index of the reserved slot.
fn reserveSlot(self: *Self) Node.Index {
    try self.nodes.append(self.allocator, undefined);
    return @enumFromInt(self.nodes.len - 1);
}

fn addSubRange(self: *Self, nodes: []const Node.Index) error{OutOfMemory}!ast.SubRange {
    const from: ast.SubRange.Index = @enumFromInt(self.node_lists.items.len);
    try self.node_lists.appendSlice(nodes);
    const to: ast.SubRange.Index = @enumFromInt(self.node_lists.items.len);
    return ast.SubRange{ .from = from, .to = to };
}

fn addToken(self: *Self, token: Token) error{OutOfMemory}!Token.Index {
    try self.tokens.append(token);
    return @enumFromInt(self.tokens.items.len - 1);
}

/// Append a node to the flat node list.
fn addNode(self: *Self, node: NodeData, start: u32, end: u32) error{OutOfMemory}!Node.Index {
    try self.nodes.append(
        self.allocator,
        .{ .data = node, .start = start, .end = end },
    );
    return @enumFromInt(self.nodes.len - 1);
}

/// Append an ExtraData item to the list, and return its index.
fn addExtraData(self: *Self, data: ast.ExtraData) error{OutOfMemory}!ast.ExtraData.Index {
    try self.extra_data.append(data);
    return @enumFromInt(self.extra_data.items.len - 1);
}

fn emitBadDestructureDiagnostic(self: *Self, expr: Node.Index) error{OutOfMemory}!void {
    const start_byte_index = self.nodes.items(.start)[@intFromEnum(expr)];
    const start_coord = util.offsets.byteIndexToCoordinate(self.source, start_byte_index);
    return self.emitDiagnostic(start_coord, "Unexpected destructuring pattern", .{});
}

/// Emit a diagnostic about an un-expected token.
fn emitBadTokenDiagnostic(
    self: *Self,
    comptime expected: []const u8,
    got: *const Token,
) error{OutOfMemory}!void {
    try self.emitDiagnostic(
        got.startCoord(self.source),
        "Expected " ++ expected ++ ", got {s}",
        .{got.toByteSlice(self.source)},
    );
}

/// Push an error essage to the list of diagnostics.
fn emitDiagnostic(
    self: *Self,
    coord: types.Coordinate,
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostics.append(Diagnostic{
        .coord = coord,
        .message = message,
    });
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
    self.prev_token_line = ret_token.line;
    return ret_token;
}

inline fn peek(self: *Self) Token {
    return self.current_token;
}

fn isAtToken(self: *Self, tag: Token.Tag) bool {
    return self.current_token.tag == tag;
}

// --------------------------------------------------------------------------------------
// Expression parsing.
// https://262.ecma-international.org/15.0/index.html#sec-ecmascript-language-expressions
// --------------------------------------------------------------------------------------

// Expression : AssignmentExpression
//            | Expression, AssignmentExpression
fn expression(self: *Self) ParseError!Node.Index {
    const expr = try self.assignmentExpression();
    if (self.current_destructure_kind == .must_destruct) {
        try self.emitBadDestructureDiagnostic(expr);
        return ParseError.UnexpectedPattern;
    }

    if (self.isAtToken(.@",")) {
        return self.completeSequenceExpr(expr);
    }

    return expr;
}

/// Parse a comma-separated sequence expression, where the first-expression is already parsed.
fn completeSequenceExpr(self: *Self, first_expr: Node.Index) ParseError!Node.Index {
    if (!self.isAtToken(.@",")) return first_expr;

    var nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer nodes.deinit();

    _ = try nodes.append(first_expr);

    const start_pos = self.nodes.items(.start)[@intFromEnum(first_expr)];
    var end_pos = self.nodes.items(.start)[@intFromEnum(first_expr)];

    while (self.isAtToken(.@",")) {
        _ = try self.next(); // eat ','
        const rhs = try self.assignmentExpression();
        end_pos = self.nodes.items(.start)[@intFromEnum(rhs)];
        try nodes.append(rhs);
    }

    const expr_list = try self.addSubRange(nodes.items);
    return try self.addNode(ast.NodeData{
        .sequence_expr = expr_list,
    }, start_pos, end_pos);
}

fn assignmentLhsExpr(self: *Self) ParseError!Node.Index {
    const token: *const Token = &self.current_token;
    switch (token.tag) {
        .@"{" => return self.objectAssignmentPattern(),
        .@"[" => return self.arrayAssignmentPattern(),
        else => {
            if (token.tag == .identifier or self.isKeywordIdentifier(token.tag)) {
                return self.identifier(try self.next());
            }

            try self.emitBadTokenDiagnostic("assignment target", token);
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

/// Mutate existing nodes to convert a PrimaryExpression to an
/// AssignmentPattern (or Identifier) (e.g, .object_literal => .object_pattern)
fn reinterpretAsPattern(self: *Self, node_id: Node.Index) void {
    const node: *ast.NodeData = &self.nodes.items(.data)[@intFromEnum(node_id)];
    switch (node.*) {
        .identifier,
        .member_expr,
        .assignment_pattern,
        .empty_array_item,
        .computed_member_expr,
        => return,
        .object_literal => |object_pl| {
            node.* = .{ .object_pattern = object_pl };
            const elems_idx = object_pl orelse return;
            const elems = elems_idx.asSlice(self);
            for (elems) |elem_id| {
                self.reinterpretAsPattern(elem_id);
            }
        },
        .array_literal => |array_pl| {
            node.* = .{ .array_pattern = array_pl };
            const elems_idx = array_pl orelse return;
            const elems = elems_idx.asSlice(self);
            for (elems) |elem_id| {
                self.reinterpretAsPattern(elem_id);
            }
        },
        .object_property => |property_definiton| self.reinterpretAsPattern(property_definiton.value),
        .assignment_expr => |assign_pl| node.* = .{ .assignment_pattern = assign_pl },
        .spread_element => |spread_pl| {
            self.reinterpretAsPattern(spread_pl);
            node.* = .{ .rest_element = spread_pl };
        },
        else => unreachable,
    }
}

fn assignmentExpression(self: *Self) ParseError!Node.Index {
    // Start with the assumption that we're parsing a valid destructurable expression.
    // Subsequent parsing functions will update this to refect the actual destructure-kind
    // of `lhs`.
    self.current_destructure_kind = .can_destruct;
    const lhs = try yieldOrConditionalExpression(self);

    if (!self.current_token.isAssignmentOperator()) {
        return lhs;
    }

    if (self.current_destructure_kind == .cannot_destruct) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "Invalid assignment target",
            .{},
        );
        return ParseError.InvalidAssignmentTarget;
    }

    self.reinterpretAsPattern(lhs);

    const op_token = try self.next(); // eat assignment operator

    self.current_destructure_kind = .can_destruct;

    const rhs = try self.assignmentExpression();
    const start = self.nodes.items(.start)[@intFromEnum(lhs)];
    const end = self.nodes.items(.end)[@intFromEnum(rhs)];

    // An assignment expression is a valid destructuring target.
    // `[x = 1] = [2]` is valid.
    // assignment patterns inside object literals are handled separately
    // in `Parser.identifierProperty`
    self.current_destructure_kind = .can_destruct;

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
    const start_expr_span = self.nodeSpan(start_expr);
    const start_pos = start_expr_span.start;
    var end_pos = start_expr_span.end;

    var expr: Node.Index = start_expr;
    while (self.isAtToken(.@"??")) {
        const op = try self.next(); // eat '??'
        const rhs = try bOrExpr(self);
        end_pos = self.nodeSpan(rhs).end;

        self.current_destructure_kind = .cannot_destruct;

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
    switch (self.nodes.items(.data)[@intFromEnum(expr)]) {
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

fn yieldOrConditionalExpression(self: *Self) ParseError!Node.Index {
    if (!(self.current_token.tag == .kw_yield and self.context.is_yield_reserved)) {
        return self.conditionalExpression();
    }

    self.current_destructure_kind = .cannot_destruct;
    return self.yieldExpression();
}

fn yieldExpression(self: *Self) ParseError!Node.Index {
    const yield_kw = try self.next();
    std.debug.assert(yield_kw.tag == .kw_yield and self.context.is_yield_reserved);

    const has_operand = self.current_token.line == yield_kw.line;
    const is_delegated = self.isAtToken(.@"*") and has_operand;
    // TODO: the error message here can be improved, when yield* is followed by a non-expression.
    if (is_delegated) {
        _ = try self.next(); // eat '*'
    }

    var operand: ?Node.Index = null;
    var end_pos = yield_kw.start + yield_kw.len;
    if (has_operand) {
        const operand_expr = try self.assignmentExpression();
        end_pos = self.nodes.items(.end)[@intFromEnum(operand_expr)];
        operand = operand_expr;
    }

    return self.addNode(.{ .yield_expr = .{
        .value = operand,
        .is_delegated = is_delegated,
    } }, yield_kw.start, end_pos);
}

/// ConditionalExpression:
///     ShortCircuitExpression
///     ShortCircuitExpression '?' AssignmentExpression ':' AssignmentExpression
fn conditionalExpression(self: *Self) ParseError!Node.Index {
    const cond_expr = try self.shortCircuitExpresion();
    if (!self.isAtToken(.@"?")) return cond_expr;

    self.current_destructure_kind = .cannot_destruct;

    _ = try self.next(); // eat '?'
    const true_expr = try self.assignmentExpression();
    _ = try self.expect(.@":");
    const false_expr = try self.assignmentExpression();

    const start_pos = self.nodes.items(.start)[@intFromEnum(cond_expr)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(false_expr)];

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
    const lhs_start_pos = self.nodes.items(.start)[@intFromEnum(lhs)];
    const rhs_end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
    return self.addNode(
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
    while (true) : (token = self.peek()) {
        switch (token.tag) {
            .@"," => {
                const comma = try self.next(); // eat ','
                if (self.isAtToken(.@"]")) break;
                try items.append(try self.addNode(
                    .{ .empty_array_item = {} },
                    comma.start,
                    comma.start + 1,
                ));
            },

            .@"..." => {
                try items.append(try self.restElement());
                if (self.isAtToken(.@",")) {
                    const comma_tok = try self.next();
                    try self.emitDiagnostic(
                        comma_tok.startCoord(self.source),
                        "Comma not permitted after spread element in array pattern",
                        .{},
                    );
                    return ParseError.InvalidAssignmentTarget;
                }

                break;
            },

            .@"]" => break,

            else => {
                try items.append(try self.assignmentPattern());
            },
        }

        if (self.current_token.tag == .@",") {
            _ = try self.next();
        } else if (self.current_token.tag == .@"]") {
            break;
        }
    }

    const rbrac = try self.expect(.@"]"); // eat ']'
    const array_items = try self.addSubRange(items.items);
    return try self.addNode(
        .{ .array_pattern = array_items },
        lbrac.start,
        rbrac.start + rbrac.len,
    );
}

fn completePropertyPatternDef(self: *Self, key: Node.Index) ParseError!Node.Index {
    _ = try self.expect(.@":");

    const value = try self.assignmentPattern();
    const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(value)];

    return self.addNode(
        .{ .object_property = .{ .key = key, .value = value } },
        start_pos,
        end_pos,
    );
}

fn destructuredPropertyDefinition(self: *Self) ParseError!Node.Index {
    switch (self.current_token.tag) {
        .string_literal, .numeric_literal => {
            const key_token = try self.next();
            const key = try self.addNode(
                .{ .literal = try self.addToken(key_token) },
                key_token.start,
                key_token.start + key_token.len,
            );
            return self.completePropertyPatternDef(key);
        },
        .@"[" => {
            _ = try self.next(); // eat '['
            const key = try self.assignmentExpression();
            _ = try self.expect(.@"]");
            return self.completePropertyPatternDef(key);
        },
        else => {
            if (self.current_token.tag == .identifier or
                self.isKeywordIdentifier(self.current_token.tag))
            {
                return self.destructuredIdentifierProperty();
            }

            return try self.assignmentPattern();
        },
    }
}

/// Parse a destructured objectp proeprty starting with an identifier.
/// Assumes that self.current_token is the .identifier.
fn destructuredIdentifierProperty(self: *Self) ParseError!Node.Index {
    const key_token = try self.next();
    const key = try self.addNode(
        .{ .identifier = try self.addToken(key_token) },
        key_token.start,
        key_token.start + key_token.len,
    );

    const cur_token = self.peek();
    if (cur_token.tag == .@"=") {
        const eq_token = try self.next(); // eat '='
        const rhs = try self.assignmentExpression();
        const end_pos = self.getNode(rhs).end;

        return self.addNode(
            .{
                .assignment_pattern = .{
                    .lhs = key,
                    .rhs = rhs,
                    .operator = try self.addToken(eq_token),
                },
            },
            key_token.start,
            end_pos,
        );
    }

    if (cur_token.tag == .@":") {
        return self.completePropertyPatternDef(key);
    }

    return self.addNode(
        .{ .object_property = ast.PropertyDefinition{ .key = key, .value = key } },
        key_token.start,
        key_token.start + key_token.len,
    );
}

/// https://tc39.es/ecma262/#prod-ObjectAssignmentPattern
fn objectAssignmentPattern(self: *Self) ParseError!Node.Index {
    const lbrace = try self.next(); // eat '{'
    std.debug.assert(lbrace.tag == .@"{");

    var props = std.ArrayList(Node.Index).init(self.allocator);
    defer props.deinit();

    var end_pos = lbrace.start + lbrace.len;

    var cur_token = self.current_token;
    while (cur_token.tag != .@"}") : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"..." => {
                try props.append(try self.restElement());
                break; // spread element must be the last element
            },

            .identifier, .string_literal, .numeric_literal, .@"[" => {
                const prop = try self.destructuredPropertyDefinition();
                try props.append(prop);
            },

            else => {
                if (self.isKeywordIdentifier(cur_token.tag)) {
                    const prop = try self.destructuredPropertyDefinition();
                    try props.append(prop);
                } else {
                    try self.emitDiagnostic(
                        cur_token.startCoord(self.source),
                        "Unexpected '{s}' while parsing destructured object pattern",
                        .{cur_token.toByteSlice(self.source)},
                    );

                    return ParseError.InvalidAssignmentTarget;
                }
            },
        }

        const comma_or_rbrace = try self.expect2(.@"}", .@",");
        if (comma_or_rbrace.tag == .@"}") {
            end_pos = comma_or_rbrace.start + comma_or_rbrace.len;
            break;
        }
    }

    const destructured_props = try self.addSubRange(props.items);
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
            const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
            self.current_destructure_kind = .cannot_destruct;
            return try self.addNode(.{
                .unary_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = try self.addToken(op_token),
                },
            }, op_token.start, expr_end_pos);
        },
        else => {
            if (self.isAtToken(.kw_await) and self.context.is_await_reserved) {
                return self.awaitExpression();
            }

            return self.updateExpression();
        },
    }
}

fn awaitExpression(self: *Self) ParseError!Node.Index {
    const await_token = try self.next();
    std.debug.assert(await_token.tag == .kw_await);

    if (!self.context.is_await_reserved) {
        try self.emitDiagnostic(
            await_token.startCoord(self.source),
            "'await' expressions are only permitted inside async functions",
            .{},
        );
        return ParseError.IllegalAwait;
    }

    const operand = try self.unaryExpression();
    const end_pos = self.nodeSpan(operand).end;

    self.current_destructure_kind = .cannot_destruct;
    return self.addNode(.{
        .await_expr = ast.UnaryPayload{
            .operator = try self.addToken(await_token),
            .operand = operand,
        },
    }, await_token.start, end_pos);
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
        const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
        self.current_destructure_kind = .cannot_destruct;
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
    const cur_token = self.peek();
    if ((cur_token.tag == .@"++" or cur_token.tag == .@"--") and
        cur_token.line == expr_start_line)
    {
        self.current_destructure_kind = .cannot_destruct;
        const op_token = try self.next();
        const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
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
        const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
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
    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"(" => call_expr = try self.completeCallExpression(call_expr),
            .@"[" => call_expr = try self.completeComputedMemberExpression(call_expr),
            .@"." => call_expr = try self.completeMemberExpression(call_expr),
            else => break,
        }
    }

    return call_expr;
}

fn completeCallExpression(self: *Self, callee: Node.Index) ParseError!Node.Index {
    const start_pos = self.nodes.items(.start)[@intFromEnum(callee)];
    const call_args = try self.args();
    const call_expr = ast.CallExpr{
        .arguments = call_args,
        .callee = callee,
    };
    const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];
    self.current_destructure_kind = .cannot_destruct;
    return self.addNode(.{ .call_expr = call_expr }, start_pos, end_pos);
}

// CoverCallAndAsyncArrowHead:  MemberExpression Arguments
fn coverCallAndAsyncArrowHead(self: *Self, callee: Node.Index) ParseError!Node.Index {
    const call_args = try self.args();
    const start_pos = self.nodes.items(.start)[@intFromEnum(callee)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];

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
    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"?." => expr = try self.completeOptionalChain(expr),
            else => return expr,
        }
    }

    self.current_destructure_kind = .cannot_destruct;
    return expr;
}

/// Assuming a `<object>?.<property>` has been consumed already, consume the
/// operators that are chained on top, and return a node which will be put into
/// an `optional_expr` field of `ast.Node`.
/// see: `Self.optionalChain`.
fn completeOptionalChain(self: *Self, prev_expr: Node.Index) ParseError!Node.Index {
    var expr = try self.optionalChain(prev_expr);
    const start_pos = self.nodes.items(.start)[@intFromEnum(expr)];

    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"[" => {
                const member_expr = try self.completeComputedMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"." => {
                const member_expr = try self.completeMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"(" => {
                const call_expr = try self.completeCallExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(call_expr)];
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
    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];

    const chain_op = try self.next();
    std.debug.assert(chain_op.tag == .@"?.");

    const cur_token = self.peek();

    switch (cur_token.tag) {
        .@"(" => {
            const call_args = try self.args();
            const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];
            const call_expr = try self.addNode(.{
                .call_expr = .{
                    .arguments = call_args,
                    .callee = object,
                },
            }, start_pos, end_pos);
            return self.addNode(.{ .optional_expr = call_expr }, start_pos, end_pos);
        },
        .@"[" => {
            const expr = try self.completeComputedMemberExpression(object);
            const end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
            return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
        },
        else => {
            if (self.current_token.tag == .identifier or self.current_token.tag.isKeyword()) {
                const property_name_token = try self.next(); // eat the property name
                const end_pos = property_name_token.start + property_name_token.len;
                const expr = try self.addNode(.{ .member_expr = .{
                    .object = object,
                    .property = try self.addToken(property_name_token),
                } }, start_pos, end_pos);
                return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
            }

            try self.emitDiagnostic(
                cur_token.startCoord(self.source),
                "Expected property access or function call after ?., but got {s}",
                .{cur_token.toByteSlice(self.source)},
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

    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];

    const property_token_idx: Token.Index = blk: {
        const tok = try self.next();
        // Yes, keywords are valid property names...
        if (tok.tag == .identifier or tok.tag.isKeyword()) {
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
    self.current_destructure_kind = .can_destruct;
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

    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(property)];
    self.current_destructure_kind = .can_destruct;
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
        .identifier => return self.identifier(token),
        .numeric_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => {
            self.current_destructure_kind = .cannot_destruct;
            return self.addNode(
                .{ .literal = try self.addToken(token) },
                token.start,
                token.start + token.len,
            );
        },
        .@"[" => return self.arrayLiteral(token.start),
        .@"{" => return self.objectLiteral(token.start),
        .@"(" => return self.groupingExprOrArrowFunction(&token),
        .kw_async => {
            if (self.current_token.tag == .kw_function) {
                _ = try self.next(); // eat 'function'
                return self.functionExpression(token.start, .{ .is_async = true });
            }
            // fall through to the default case.
        },
        .kw_function => return self.functionExpression(token.start, .{}),
        else => {},
    }

    if (self.isKeywordIdentifier(token.tag)) {
        return self.identifier(token);
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Unexpected '{s}'",
        .{token.toByteSlice(self.source)},
    );
    return ParseError.UnexpectedToken;
}

fn identifier(self: *Self, token: Token) ParseError!Node.Index {
    return self.addNode(
        .{ .identifier = try self.addToken(token) },
        token.start,
        token.start + token.len,
    );
}

/// Assuming the parameter list has been consumed, parse the body of
/// an arrow function and return the complete arrow function AST node id.
fn completeArrowFunction(
    self: *Self,
    lparen: *const Token,
    rparen: *const Token,
    params: Node.Index,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    if (!self.isAtToken(.@"=>")) {
        try self.emitDiagnostic(
            lparen.startCoord(self.source),
            "'()' is not a valid expression. Arrow functions start with '() => '",
            .{},
        );
        return ParseError.InvalidArrowFunction;
    }

    const fat_arrow = try self.next();
    if (rparen.line != fat_arrow.line) {
        try self.emitDiagnostic(
            fat_arrow.startCoord(self.source),
            "'=>' must be on the same line as the arrow function parameters",
            .{},
        );
        return ParseError.IllegalFatArrow;
    }

    const body = blk: {
        const token = self.peek();
        if (token.tag == .@"{") {
            const context = self.context;
            defer self.setContext(context);
            self.context.@"return" = true;
            break :blk try self.blockStatement();
        }

        const assignment = try self.assignmentExpression();
        if (self.current_destructure_kind == .must_destruct) {
            try self.emitDiagnostic(
                token.startCoord(self.source),
                "Invalid arrow function body",
                .{},
            );
            return ParseError.InvalidArrowFunction;
        }

        break :blk assignment;
    };

    const end_pos = self.nodes.items(.end)[@intFromEnum(body)];
    return self.addNode(ast.NodeData{
        .function_expr = ast.Function{
            .parameters = params,
            .body = body,
            .info = try self.addExtraData(.{
                .function = .{
                    .name = null,
                    .flags = flags,
                },
            }),
        },
    }, lparen.start, end_pos);
}

/// Parse a RestElement, assuming we're at the `...` token
fn restElement(self: *Self) ParseError!Node.Index {
    const dotdotdot = try self.next();
    const rest_arg = try self.assignmentLhsExpr();
    const end_pos = self.nodes.items(.end)[@intFromEnum(rest_arg)];
    return self.addNode(.{ .rest_element = rest_arg }, dotdotdot.start, end_pos);
}

/// Once a '(' token has been eaten, parse the either an arrow function or a parenthesized expression.
fn arrowParamsOrExpression(self: *Self, lparen: *const Token) ParseError!Node.Index {
    const first_expr = try self.assignmentExpression();
    if (self.current_destructure_kind == .cannot_destruct) {
        const expr = try self.completeSequenceExpr(first_expr);
        _ = try self.expect(.@")");
        return expr;
    }

    var nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer nodes.deinit();

    _ = try nodes.append(first_expr);

    var has_trailing_comma = false;

    var destructure_kind: u8 = self.current_destructure_kind.asU8();
    while (self.isAtToken(.@",")) {
        const comma_token = try self.next(); // eat ','
        // A ')' after comma is allowed in arrow function parameters,
        // but not in regular comma-separated expressions.
        if (self.isAtToken(.@")") and
            destructure_kind & DestructureKind.cannot_destruct.asU8() != 1)
        {
            has_trailing_comma = true;
            break;
        }

        const rhs = try self.assignmentExpression();
        destructure_kind |= self.current_destructure_kind.asU8();

        if (destructure_kind == DestructureKind.bad_object_or_pattern.asU8()) {
            // TODO: better location for the diagnostic.
            try self.emitDiagnostic(
                comma_token.startCoord(self.source),
                "Invalid object or destructuring pattern",
                .{},
            );
            return ParseError.InvalidObject;
        }
        try nodes.append(rhs);
    }

    const rparen = try self.expect(.@")");

    if (self.isAtToken(.@"=>")) {
        if (destructure_kind != DestructureKind.can_destruct.asU8()) {
            // TODO: improve the location of the diagnostic.
            // Which part exactly is invalid?
            try self.emitDiagnostic(
                lparen.startCoord(self.source),
                "Invalid arrow function parameters",
                .{},
            );
            return ParseError.InvalidArrowParameters;
        }

        for (nodes.items) |node| {
            self.reinterpretAsPattern(node);
        }

        const params_range = try self.addSubRange(nodes.items);
        const parameters = try self.addNode(
            .{ .parameters = params_range },
            lparen.start,
            rparen.start + 1,
        );

        return self.completeArrowFunction(
            lparen,
            &rparen,
            parameters,
            .{ .is_arrow = true },
        );
    }

    if (destructure_kind == DestructureKind.must_destruct.asU8()) {
        // TODO: improve the location of the diagnostic.
        // Which part exaclty forces a destructuring pattern?
        try self.emitDiagnostic(
            lparen.startCoord(self.source),
            "Grouping expression contains a destructuring pattern",
            .{},
        );
        return ParseError.UnexpectedPattern;
    }

    if (has_trailing_comma) {
        try self.emitDiagnostic(
            rparen.startCoord(self.source),
            "Trailing comma is not permitted in parenthesied expressions",
            .{},
        );
        return ParseError.UnexpectedToken;
    }

    if (nodes.items.len == 1) {
        return nodes.items[0];
    }

    const sequence_expr = try self.addSubRange(nodes.items);
    return self.addNode(.{ .sequence_expr = sequence_expr }, lparen.start, rparen.start + 1);
}

/// Parses either an arrow function or a parenthesized expression.
fn groupingExprOrArrowFunction(self: *Self, lparen: *const Token) ParseError!Node.Index {
    if (self.isAtToken(.@")")) {
        const rparen = try self.next();
        const end_pos = rparen.start + rparen.len;
        const params = try self.addNode(.{ .parameters = null }, lparen.start, end_pos);
        return self.completeArrowFunction(lparen, &rparen, params, .{ .is_arrow = true });
    }

    if (self.isAtToken(.@"...")) {
        const rest_elem = try self.restElement();
        const rparen = try self.expect(.@")");
        const params = try self.addSubRange(&[_]Node.Index{rest_elem});

        const parameters = try self.addNode(
            .{ .parameters = params },
            lparen.start,
            rparen.start + rparen.len,
        );
        return self.completeArrowFunction(lparen, &rparen, parameters, .{ .is_arrow = true });
    }

    return self.arrowParamsOrExpression(lparen);
}

/// Parse an object literal, assuming the `{` has already been consumed.
/// https://262.ecma-international.org/15.0/index.html#prod-ObjectLiteral
fn objectLiteral(self: *Self, start_pos: u32) ParseError!Node.Index {
    const properties = try self.propertyDefinitionList();
    const closing_brace = try self.expect(.@"}");
    const end_pos = closing_brace.start + closing_brace.len;
    return self.addNode(.{ .object_literal = properties }, start_pos, end_pos);
}

/// https://tc39.es/ecma262/#prod-PropertyDefinitionList
/// Parse a comma-separated list of properties.
/// Returns `null` if there's 0 properties in the object.
fn propertyDefinitionList(self: *Self) ParseError!?ast.SubRange {
    var property_defs = std.ArrayList(Node.Index).init(self.allocator);
    defer property_defs.deinit();

    var destructure_kind: u8 = @intFromEnum(self.current_destructure_kind);

    const cur_token = self.peek();
    while (cur_token.tag != .eof) {
        switch (cur_token.tag) {
            .identifier => {
                try property_defs.append(try self.identifierProperty());
                destructure_kind |= self.current_destructure_kind.asU8();
            },

            .@"[" => {
                _ = try self.next();
                const key = try self.assignmentExpression();
                _ = try self.expect(.@"]");

                const property = try self.completePropertyDef(
                    key,
                    .{ .is_computed = true },
                );
                destructure_kind |= self.current_destructure_kind.asU8();
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
                destructure_kind |= self.current_destructure_kind.asU8();
                try property_defs.append(property_expr);
            },

            // generator method
            .@"*" => {
                _ = try self.next(); // eat '*'
                const key = try self.propertyName();
                const generator_method = try self.parseMethodBody(
                    key,
                    .{ .is_method = true },
                    .{ .is_generator = true },
                );
                destructure_kind |= DestructureKind.cannot_destruct.asU8();
                try property_defs.append(generator_method);
            },

            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();

                destructure_kind |= self.current_destructure_kind.asU8();

                const start = ellipsis_tok.start;
                const end = self.nodes.items(.end)[@intFromEnum(expr)];
                try property_defs.append(try self.addNode(.{ .spread_element = expr }, start, end));

                if (self.isAtToken(.@",")) {
                    // comma is not allowed after rest element in object patterns
                    destructure_kind |= DestructureKind.cannot_destruct.asU8();
                }
            },
            else => {
                if (self.current_token.tag.isKeyword()) {
                    const id_property = try self.identifierProperty();
                    try property_defs.append(id_property);
                    destructure_kind |= self.current_destructure_kind.asU8();
                } else {
                    break;
                }
            },
        }

        const maybe_comma = self.peek();
        if (maybe_comma.tag == .@",") {
            _ = try self.next();
        } else {
            break;
        }
    }

    if (destructure_kind == @intFromEnum(DestructureKind.bad_object_or_pattern)) {
        // TODO: emit a diagnostic.
        return ParseError.InvalidObject;
    }

    self.current_destructure_kind = @enumFromInt(destructure_kind);

    if (property_defs.items.len == 0) return null;
    return try self.addSubRange(property_defs.items);
}

/// Parse an object property name, which can be an identifier or a keyword.
fn propertyName(self: *Self) ParseError!Node.Index {
    if (self.current_token.tag == .identifier or self.current_token.tag.isKeyword()) {
        self.current_destructure_kind = .can_destruct;
        return self.identifier(try self.next());
    }

    try self.emitDiagnostic(
        self.current_token.startCoord(self.source),
        "Expected a property name, got '{s}'",
        .{self.current_token.toByteSlice(self.source)},
    );

    return ParseError.InvalidPropertyName;
}

/// Parse an the property of an object literal or object pattern that starts with an identifier.
fn identifierProperty(self: *Self) ParseError!Node.Index {
    const key_token = try self.next();
    std.debug.assert(key_token.tag == .identifier or key_token.tag.isKeyword());

    const cur_token = self.current_token;
    if (cur_token.tag != .@":" and cur_token.tag != .@"(" and
        cur_token.tag != .@"," and cur_token.tag != .@"}" and
        cur_token.tag != .@"=")
    {
        if (key_token.tag == .kw_async and key_token.tag.isValidPropertyName()) {
            // handle `async f() { ... }`
            const property_key = try self.identifier(try self.next());
            const property_val = try self.parseMethodBody(
                property_key,
                .{ .is_method = true },
                .{ .is_async = true },
            );

            const end_pos = self.nodes.items(.end)[@intFromEnum(property_val)];

            // object literals with async methods are not valid object patterns.
            self.current_destructure_kind = .cannot_destruct;
            return self.addNode(.{
                .object_property = .{
                    .key = property_key,
                    .value = property_val,
                },
            }, key_token.start, end_pos);
        }

        const maybe_getter_or_setter = try self.getterOrSetter(key_token);
        if (maybe_getter_or_setter) |getter_or_setter| {
            // object literals with getters or setters are not valid object patterns.
            self.current_destructure_kind = .cannot_destruct;
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
    const key = try self.identifier(key_token);

    const cur_token_tag = self.current_token.tag;
    switch (cur_token_tag) {
        .@":", .@"(" => {
            return self.completePropertyDef(key, .{
                .is_method = cur_token_tag == .@"(",
            });
        },

        .@"=" => {
            const op_token = try self.next(); // eat '='
            if (key_token.tag != .identifier) {
                try self.emitBadTokenDiagnostic("property name", &key_token);
                return ParseError.InvalidPropertyName;
            }

            const value = try self.assignmentExpression();
            const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
            const end_pos = self.nodes.items(.end)[@intFromEnum(value)];

            const assignment_pattern = ast.NodeData{
                .assignment_pattern = .{
                    .lhs = key,
                    .rhs = value,
                    .operator = try self.addToken(op_token),
                },
            };

            // 'Identifier = AssignmentExpression' is allowed in object patterns but not in object literals
            self.current_destructure_kind = .must_destruct;
            return self.addNode(assignment_pattern, start_pos, end_pos);
        },

        else => {
            const kv_node = ast.PropertyDefinition{
                .key = key,
                .value = key,
                .flags = .{ .is_shorthand = true },
            };

            // { k }
            //   ^-- Is a valid property in a destructuring pattern
            self.current_destructure_kind = .can_destruct;
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
        .{},
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
    fn_flags: ast.FunctionFlags,
) ParseError!Node.Index {
    std.debug.assert(self.current_token.tag == .@"(" and flags.is_method);

    const start_pos = self.peek().start;
    const func_expr = try self.parseFunctionBody(
        start_pos,
        null,
        fn_flags,
        false,
    );
    const end_pos = self.nodeSpan(func_expr).end;

    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = func_expr,
        .flags = flags,
    };

    if (flags.kind == .get or flags.kind == .set) {
        // verify the number of parameters for getters and setters.
        try self.checkGetterOrSetterParams(func_expr, flags.kind);
    }

    const key_start = self.nodes.items(.start)[@intFromEnum(key)];
    return self.addNode(
        .{ .object_property = kv_node },
        key_start,
        end_pos,
    );
}

/// Verify the number of parameters in a getter or setter.
/// Emit a diagnostic, then return an error if the number is invalid.
/// A Getter must have exaclty 0 parameters, and a setter only one.
fn checkGetterOrSetterParams(
    self: *Self,
    func_expr: Node.Index,
    kind: ast.PropertyDefinitionKind,
) ParseError!void {
    const func = &self.getNode(func_expr).data.function_expr;
    const n_params = func.getParameterCount(self);
    if (kind == .get and n_params != 0) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "A 'get' accessor should have no parameter, but got {d}",
            .{n_params},
        );
        return ParseError.InvalidGetter;
    }

    if (kind == .set and n_params != 1) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "A 'set' accessor should have exaclty one parameters, but got {d}",
            .{n_params},
        );
        return ParseError.InvalidSetter;
    }
}

/// Assuming that the key has been parsed, complete the property definition.
fn completePropertyDef(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
) ParseError!Node.Index {
    if (self.current_token.tag == .@"(") {
        self.current_destructure_kind = .cannot_destruct;
        return self.parseMethodBody(key, .{
            .is_method = true,
            .is_computed = flags.is_computed,
            .is_shorthand = flags.is_shorthand,
            .kind = flags.kind,
        }, .{});
    }

    _ = try self.expect(.@":");

    const value = try self.assignmentExpression();
    // the assignmentExpression() will have updated self.current_destructure_kind

    const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(value)];
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

    var destructuring_kind: u8 = @intFromEnum(self.current_destructure_kind);

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
                const end = self.nodes.items(.end)[@intFromEnum(expr)];
                if (self.isAtToken(.@",")) {
                    // "," is not allowed after rest element in array patterns
                    destructuring_kind |= DestructureKind.cannot_destruct.asU8();
                }

                try elements.append(try self.addNode(.{ .spread_element = expr }, start, end));
            },

            else => {
                const item = try self.assignmentExpression();
                destructuring_kind |= self.current_destructure_kind.asU8();
                try elements.append(item);
            },
        }

        const next_token = try self.expect2(.@",", .@"]");
        if (next_token.tag == .@"]") {
            end_pos = next_token.start + next_token.len;
            break;
        }
    }

    const nodes = try self.addSubRange(elements.items);
    self.current_destructure_kind = @enumFromInt(destructuring_kind);
    return self.addNode(.{ .array_literal = nodes }, start_pos, end_pos);
}

/// Assuming the parser is at the `function` keyword,
/// parse a function expression.
fn functionExpression(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) ParseError!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.next(); // eat '*'
        fn_flags.is_generator = true;
    }

    const name_token: ?Token.Index =
        if (self.current_token.tag == .identifier)
        try self.addToken(try self.next())
    else
        null;
    return self.parseFunctionBody(start_pos, name_token, fn_flags, false);
}

/// parses the arguments and body of a function expression (or declaration),
/// assuming the `function` keyword (and/or the function/method name) has been consumed.
fn parseFunctionBody(
    self: *Self,
    start_pos: u32,
    name_token: ?Token.Index,
    flags: ast.FunctionFlags,
    is_decl: bool,
) ParseError!Node.Index {
    const saved_context = self.context;
    defer self.setContext(saved_context);

    if (flags.is_generator)
        self.context.is_yield_reserved = true;
    if (flags.is_async)
        self.context.is_await_reserved = true;

    const params = try self.parseFormalParameters();
    if (flags.is_arrow) unreachable; // not supported yet :)

    // Allow return statements inside function
    const ctx = self.context;
    defer self.setContext(ctx);
    self.context.@"return" = true;

    // TODO: make the body a statement list.
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

    const function = ast.Function{
        .parameters = params,
        .body = body,
        .info = function_data,
    };

    const node_data: ast.NodeData = if (is_decl)
        .{ .function_declaration = function }
    else
        .{ .function_expr = function };
    return self.addNode(node_data, start_pos, end_pos);
}

/// Starting with the '(' token , parse formal parameters of a function.
fn parseFormalParameters(self: *Self) ParseError!Node.Index {
    const lparen = try self.expect(.@"(");
    const start_pos = lparen.start;

    var params = std.ArrayList(Node.Index).init(self.allocator);
    defer params.deinit();

    var cur_token = self.peek();
    while (cur_token.tag != .@")" and cur_token.tag != .eof) : (cur_token = self.peek()) {
        const param = try self.assignmentLhsExpr();
        try params.append(param);
    }

    const rparen = try self.expect(.@")");
    const end_pos = rparen.start + rparen.len;

    const param_list = if (params.items.len > 0)
        try self.addSubRange(params.items)
    else
        null;

    return self.addNode(.{ .parameters = param_list }, start_pos, end_pos);
}

/// Get a pointer to a node by its index.
/// The returned value can be invalidated by any call to `addNode`, `restoreState`, `addSubRange`.
pub fn getNode(self: *const Self, index: Node.Index) ast.Node {
    return self.nodes.get(@intFromEnum(index));
}

pub fn getExtraData(
    self: *const Self,
    index: ast.ExtraData.Index,
) *const ast.ExtraData {
    return &self.extra_data.items[@intFromEnum(index)];
}

/// From a `SubRange` object, get a slice that contains the ID for each node
/// in that list.
pub fn getSubRange(
    self: *const Self,
    from: ast.SubRange.Index,
    to: ast.SubRange.Index,
) []const Node.Index {
    const from_: usize = @intFromEnum(from);
    const to_: usize = @intFromEnum(to);
    return self.node_lists.items[from_..to_];
}

pub fn getToken(self: *const Self, index: Token.Index) Token {
    return self.tokens.items[@intFromEnum(index)];
}

/// Get the start and end byte offset of a node in the source file.
fn nodeSpan(self: *const Self, index: Node.Index) types.Span {
    const start = self.nodes.items(.start)[@intFromEnum(index)];
    const end = self.nodes.items(.end)[@intFromEnum(index)];
    return .{ .start = start, .end = end };
}

/// Parses arguments for a function call, assuming the current_token is '('
fn args(self: *Self) ParseError!Node.Index {
    const args_node, const start, const end = try self.parseArgs();
    return self.addNode(.{ .arguments = args_node }, start, end);
}

fn parseArgs(self: *Self) ParseError!struct { ast.SubRange, u32, u32 } {
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

    return .{ try self.addSubRange(arg_list.items), start_pos, end_pos };
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
                const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
                const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
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
                const itag: u32 = @intFromEnum(token.tag);
                if (itag >= min and itag <= max) {
                    self.current_destructure_kind =
                        @enumFromInt(self.current_destructure_kind.asU8() |
                        DestructureKind.cannot_destruct.asU8());

                    _ = try self.next();
                    const rhs = try nextFn(self);
                    const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
                    const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
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

// -----
// Tests
// -----

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

    const root_node = parser.parse() catch |err| {
        for (parser.diagnostics.items) |diagnostic| {
            std.log.err("({d}:{d}): {s}\n", .{
                diagnostic.coord.line,
                diagnostic.coord.column,
                diagnostic.message,
            });
        }
        return err;
    };

    // 1. prettify the AST as a JSON string
    const ast_json = try pretty.toJsonString(t.allocator, &parser, root_node);
    defer t.allocator.free(ast_json);

    // 2. For every `<filename>.js`, read the corresponding `<filename>.json` file
    const json_file_path = try std.mem.concat(
        t.allocator,
        u8,
        &.{ file_path[0 .. file_path.len - 3], ".json" },
    );
    defer t.allocator.free(json_file_path);

    const expected_ast_json = tests_dir.readFileAlloc(
        t.allocator,
        json_file_path,
        std.math.maxInt(u32),
    ) catch |err| {
        std.debug.print("failed to read file: {s}\n", .{json_file_path});
        return err;
    };
    defer t.allocator.free(expected_ast_json);

    const expected_ast_json_trimmed = std.mem.trim(u8, expected_ast_json, "\n\t ");

    // 3. ensure the AST JSON is equal to the expected JSON
    try t.expectEqualStrings(expected_ast_json_trimmed, ast_json);
}

test StringHelper {
    _ = StringHelper;
}

test parse {
    var root_dir = std.fs.cwd();
    var tests_dir = try root_dir.openDir("parser-tests", .{ .iterate = true });
    defer tests_dir.close();

    var iter = tests_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .directory) {
            continue;
        }

        var dir = try tests_dir.openDir(entry.name, .{ .iterate = true });
        defer dir.close();

        var d_iter = dir.iterate();
        while (try d_iter.next()) |test_case_entry| {
            std.debug.assert(test_case_entry.kind == .file);
            if (!std.mem.endsWith(u8, test_case_entry.name, ".js")) continue;
            runTestOnFile(dir, test_case_entry.name) catch |err| {
                std.log.err("Error comparing ASTs for file: {s}\n", .{test_case_entry.name});
                return err;
            };
        }
    }
}
