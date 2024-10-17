const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig").Tokenizer;
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");

const util = @import("util");

const types = util.types;
const offsets = util.offsets;

const Node = ast.Node;

const ParseError = error{ UnexpectedToken, OutOfMemory, NotSupported } || Tokenizer.Error;
const ParseFn = fn (self: *Self) ParseError!Node.Index;

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

source: []const u8,
file_name: []const u8,
tokenizer: Tokenizer,

/// All AST nodes are stored in this flat list and reference
/// each other using their indices.
nodes: std.ArrayList(Node),
/// List of tokens that are necessary to keep around
/// e.g - identifiers, literals, node start and end nodes, etc.
tokens: std.ArrayList(Token),
/// Arguments for function calls, new-expressions, etc.
node_lists: std.ArrayList(Node.Index),
diagnostics: std.ArrayList(types.Diagnostic),

/// The token that we're currently at.
/// Calling `next()` or `peek()` will return this token.
current_token: Token,
/// The next token that we're going to read.
next_token: Token,

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
        .nodes = std.ArrayList(Node).init(allocator),
        .tokens = std.ArrayList(Token).init(allocator),
        .diagnostics = std.ArrayList(types.Diagnostic).init(allocator),
        .node_lists = std.ArrayList(Node.Index).init(allocator),
    };

    // this call will initialize `current_token` and `next_token`.
    _ = try self.next();
    _ = try self.next();
    return self;
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit();
    self.tokens.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit();
    self.node_lists.deinit();
}

pub fn parse(self: *Self) !Node.Index {
    return try self.expression();
}

// Expression : AssignmentExpression
//            | Expression, AssignmentExpression
fn expression(self: *Self) !Node.Index {
    // TODO: comma separated expressions
    const assignment_expr = self.assignmentExpression();
    return assignment_expr;
}

fn assignmentExpression(self: *Self) !Node.Index {
    // TODO: formally, this should be ConditionalExpression parslet.
    var node = try lOrExpr(self);
    // TODO: check if `node` is valid LHS

    var token = self.peek();
    while (true) : (token = self.peek()) {
        if (!token.isAssignmentOperator()) break;
        _ = try self.next();

        const rhs = try self.assignmentExpression();
        node = try self.addNode(.{
            .assignment_expr = .{
                .lhs = node,
                .rhs = rhs,
                .operator = try self.addToken(token),
            },
        });
    }

    return node;
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
            _ = try self.next();
            const expr = try self.unaryExpression();
            return try self.addNode(ast.Node{
                .unary_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = try self.addToken(token),
                },
            });
        },
        else => return self.updateExpression(),
    }
}

fn updateExpression(self: *Self) ParseError!Node.Index {
    const token = self.peek();
    if (token.tag == .@"++" or token.tag == .@"--") {
        _ = try self.next();
        const expr = try self.unaryExpression();
        return self.addNode(ast.Node{
            .update_expr = ast.UnaryPayload{
                .operand = expr,
                .operator = try self.addToken(token),
            },
        });
    }

    // post increment / decrement
    const expr = try self.lhsExpression();
    const lookahead = self.peek();
    if (lookahead.tag == .@"++" or lookahead.tag == .@"--") {
        _ = try self.next();
        return self.addNode(ast.Node{
            .post_unary_expr = .{
                .operand = expr,
                .operator = try self.addToken(lookahead),
            },
        });
    }

    return expr;
}

fn lhsExpression(self: *Self) ParseError!Node.Index {
    if (try self.tryNewExpression()) |expr| return expr;
    var lhs_expr = try self.memberExpression();
    if (try self.tryCallExpression(lhs_expr)) |call_expr| {
        lhs_expr = call_expr;
    }

    if (self.isAtToken(.@"?.")) {
        lhs_expr = try self.optionalExpression(lhs_expr);
    }
    return lhs_expr;
}

fn tryNewExpression(self: *Self) ParseError!?Node.Index {
    if (self.isAtToken(.kw_new)) {
        _ = try self.next(); // eat "new"
        const expr = try self.memberExpression();
        return try self.addNode(.{
            .new_expr = .{
                .callee = expr,
                .arguments = if (self.isAtToken(.@"("))
                    try self.args()
                else
                    try self.addNode(.{ .arguments = null }),
            },
        });
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
    if (token.tag == .kw_super) {
        _ = try self.next();
        return try self.addNode(.{
            .super_call_expr = try self.parseArgs(),
        });
    }

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
    const call_expr = ast.CallExpr{
        .arguments = try self.args(),
        .callee = callee,
    };

    return self.addNode(.{ .call_expr = call_expr });
}

// CoverCallAndAsyncArrowHead:  MemberExpression Arguments
fn coverCallAndAsyncArrowHead(self: *Self, callee: Node.Index) ParseError!Node.Index {
    return self.addNode(.{
        .call_expr = .{
            .callee = callee,
            .arguments = try self.args(),
        },
    });
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
    var lookahead = self.peek();
    while (lookahead.tag != .eof) : (lookahead = self.peek()) {
        switch (lookahead.tag) {
            .@"[" => {
                const member_expr = try self.completeComputedMemberExpression(expr);
                expr = try self.addNode(.{ .optional_expr = member_expr });
            },
            .@"." => {
                const member_expr = try self.completeMemberExpression(expr);
                expr = try self.addNode(.{ .optional_expr = member_expr });
            },
            .@"(" => {
                const call_expr = try self.completeCallExpression(expr);
                expr = try self.addNode(.{ .optional_expr = call_expr });
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
    const chain_op = try self.next();
    std.debug.assert(chain_op.tag == .@"?.");

    const lookahead = self.peek();

    switch (lookahead.tag) {
        .@"(" => {
            const expr = try self.addNode(ast.Node{
                .call_expr = .{
                    .arguments = try self.args(),
                    .callee = object,
                },
            });
            return self.addNode(ast.Node{ .optional_expr = expr });
        },
        .@"[" => {
            const expr = try self.completeComputedMemberExpression(object);
            return self.addNode(ast.Node{ .optional_expr = expr });
        },
        .identifier, .private_identifier => {
            _ = try self.next(); // eat the property name
            const expr = try self.addNode(ast.Node{ .member_expr = .{
                .object = object,
                .property = try self.addToken(lookahead),
            } });
            return self.addNode(ast.Node{ .optional_expr = expr });
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

    const property_token: Token.Index = blk: {
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
        .property = property_token,
    };
    return self.addNode(.{ .member_expr = property_access });
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

    return self.addNode(.{ .computed_member_expr = property_access });
}

fn primaryExpression(self: *Self) ParseError!Node.Index {
    const token = try self.next();
    switch (token.tag) {
        .kw_this => return self.addNode(.{ .this = try self.addToken(token) }),
        .identifier => return self.addNode(.{ .identifier = try self.addToken(token) }),
        .numeric_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => return self.addNode(ast.Node{ .literal = try self.addToken(token) }),
        .@"[" => return self.arrayLiteral(),
        .@"{" => return self.objectLiteral(),
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

/// Parse an object literal, assuming the `{` has already been consumed.
/// https://262.ecma-international.org/15.0/index.html#prod-ObjectLiteral
fn objectLiteral(self: *Self) ParseError!Node.Index {
    const properties = try self.propertyDefinitionList();
    _ = try self.expect(.@"}");

    return try self.addNode(.{ .object_literal = properties });
}

fn propertyDefinitionList(self: *Self) ParseError!?ast.NodeList {
    var property_defs = std.ArrayList(Node.Index).init(self.allocator);
    defer property_defs.deinit();

    const lookahead = self.peek();
    while (lookahead.tag != .eof) {
        switch (lookahead.tag) {
            .identifier => {
                const key_token = try self.next();
                const key = try self.addNode(.{ .identifier = try self.addToken(key_token) });

                const maybe_colon = self.peek();
                if (maybe_colon.tag != .@":") {
                    const kv_node = ast.ObjectProperty{ .key = key, .value = key };
                    try property_defs.append(try self.addNode(.{ .object_property = kv_node }));
                } else {
                    _ = try self.next();
                    try property_defs.append(try self.completePropertyDef(key));
                }
            },

            .@"[" => {
                _ = try self.next();
                const key = try self.assignmentExpression();
                _ = try self.expect(.@"]");
                _ = try self.expect(.@":");
                try property_defs.append(try self.completePropertyDef(key));
            },

            .numeric_literal, .string_literal => {
                const key_token = try self.next();
                const key = try self.addNode(.{ .literal = try self.addToken(key_token) });
                _ = try self.expect(.@":");
                try property_defs.append(try self.completePropertyDef(key));
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

fn completePropertyDef(self: *Self, key: Node.Index) ParseError!Node.Index {
    const value = try self.assignmentExpression();
    const kv_node = ast.ObjectProperty{
        .key = key,
        .value = value,
    };
    return self.addNode(.{ .object_property = kv_node });
}

/// Parse an ArrayLiteral:
/// https://262.ecma-international.org/15.0/index.html#prod-ArrayLiteral
fn arrayLiteral(self: *Self) ParseError!Node.Index {
    var elements = std.ArrayList(Node.Index).init(self.allocator);
    defer elements.deinit();

    while (true) {
        while (self.isAtToken(.@",")) {
            // elision: https://262.ecma-international.org/15.0/index.html#prod-Elision
            _ = try self.next();
            try elements.append(try self.addNode(.{ .empty_array_item = {} }));
        }

        if (self.isAtToken(.@"]")) {
            _ = try self.next();
            break;
        }

        const item = try self.assignmentExpression();
        try elements.append(item);

        if ((try self.expect2(.@",", .@"]")).tag == .@"]") {
            break;
        }
    }

    const nodes = try self.addNodeList(elements.items);
    return self.addNode(.{ .array_literal = nodes });
}

fn getNode(self: *const Self, index: Node.Index) *const ast.Node {
    return &self.nodes.items[@intFromEnum(index)];
}

fn args(self: *Self) ParseError!Node.Index {
    return self.addNode(.{ .arguments = try self.parseArgs() });
}

fn parseArgs(self: *Self) ParseError!ast.NodeList {
    _ = try self.expect(.@"(");

    var arg_list = std.ArrayList(Node.Index).init(self.allocator);
    defer arg_list.deinit();

    while (!self.isAtToken(.@")")) {
        const expr = try self.assignmentExpression();
        try arg_list.append(expr);
        if (!self.isAtToken(.@","))
            break;
        _ = try self.next(); // eat ','
    }

    _ = try self.expect(.@")"); // eat closing ')'

    return self.addNodeList(arg_list.items);
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

fn addNode(self: *Self, node: Node) error{OutOfMemory}!Node.Index {
    try self.nodes.append(node);
    return @enumFromInt(self.nodes.items.len - 1);
}

/// Push an error essage to the list of diagnostics.
fn emitDiagnostic(
    self: *Self,
    coord: types.Coordinate,
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostics.append(types.Diagnostic{
        .coord = coord,
        .message = message,
    });
}

fn expect(self: *Self, tag: Token.Tag) ParseError!Token {
    const token = try self.next();
    if (token.tag == tag) {
        return token;
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Expected a '{s}'', but found a '{s}'",
        .{ @tagName(tag), token.toByteSlice(self.source) },
    );
    return ParseError.UnexpectedToken;
}

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
                node = try self.addNode(.{
                    .binary_expr = .{
                        .lhs = node,
                        .rhs = rhs,
                        .operator = try self.addToken(token),
                    },
                });
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
                    node = try self.addNode(.{
                        .binary_expr = .{
                            .lhs = node,
                            .rhs = rhs,
                            .operator = try self.addToken(token),
                        },
                    });
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
test parse {
    const source = "a /= b = 2 * 3";
    var parser = try Self.init(t.allocator, source, "test.js");
    defer parser.deinit();
    _ = parser.parse() catch {
        for (parser.diagnostics.items) |d| {
            std.debug.print("{s}", .{d.message});
        }
        return;
    };
}
