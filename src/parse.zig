const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig").Tokenizer;
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");
const types = @import("./types.zig");
const offsets = @import("./offsets.zig");

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
nodes: std.ArrayList(Node),
tokens: std.ArrayList(Token),
arguments: std.ArrayList(Node.Index),
diagnostics: std.ArrayList(types.Diagnostic),

pub fn init(
    allocator: std.mem.Allocator,
    source: []const u8,
    file_name: []const u8,
) ParseError!Self {
    return Self{
        .allocator = allocator,
        .source = source,
        .file_name = file_name,
        .tokenizer = try Tokenizer.init(source),
        .nodes = std.ArrayList(Node).init(allocator),
        .tokens = std.ArrayList(Token).init(allocator),
        .diagnostics = std.ArrayList(types.Diagnostic).init(allocator),
        .arguments = std.ArrayList(Node.Index).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit();
    self.tokens.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit();
    self.arguments.deinit();
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

    while (self.peek()) |token| {
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
    if (self.peek()) |token| {
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
            else => {},
        }
    }

    return self.updateExpression();
}

fn updateExpression(self: *Self) ParseError!Node.Index {
    if (self.peek()) |token| {
        if (token.tag == .@"++" and token.tag == .@"--") {
            _ = try self.next();
            const expr = try self.unaryExpression();
            return self.addNode(ast.Node{
                .update_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = try self.addToken(token),
                },
            });
        }
    }

    // post increment / decrement
    const expr = try self.lhsExpression();
    const lookahead = self.peek() orelse return expr;
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

fn tryCallExpression(self: *Self, callee: Node.Index) ParseError!?Node.Index {
    const token = self.peek() orelse return ParseError.UnexpectedEof;
    if (token.tag == .kw_super) {
        _ = try self.next();
        return try self.addNode(.{
            .super_call_expr = try self.parseArgs(),
        });
    }

    if (token.tag != .@"(") return null;

    var call_expr = try self.coverCallAndAsyncArrowHead(callee);
    var maybe_token = self.peek();
    while (maybe_token) |lookahead| : (maybe_token = self.peek()) {
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
    while (lookahead) |token| : (lookahead = self.peek()) {
        switch (token.tag) {
            .@"?." => expr = try self.completeOptionalChain(expr),
            else => return expr,
        }
    }

    return expr;
}

fn completeOptionalChain(self: *Self, prev_expr: Node.Index) ParseError!Node.Index {
    var expr = try self.optionalChain(prev_expr);
    var lookahead = self.peek();
    while (lookahead) |token| : (lookahead = self.peek()) {
        switch (token.tag) {
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

fn optionalChain(self: *Self, object: Node.Index) ParseError!Node.Index {
    const chain_op = try self.next();
    std.debug.assert(chain_op.tag == .@"?.");

    const lookahead = self.peek() orelse
        return ParseError.UnexpectedEof;

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
    var maybe_token = self.peek();
    while (maybe_token) |tok| : (maybe_token = self.peek()) {
        switch (tok.tag) {
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
        else => {
            try self.emitDiagnostic(
                token.startCoord(self.source),
                "expected literal, found '{s}'",
                .{token.toByteSlice(self.source)},
            );
            return ParseError.UnexpectedToken;
        },
    }
}

fn getNode(self: *const Self, index: Node.Index) *const ast.Node {
    return &self.nodes.items[@intFromEnum(index)];
}

fn args(self: *Self) ParseError!Node.Index {
    return self.addNode(.{ .arguments = try self.parseArgs() });
}

fn parseArgs(self: *Self) ParseError!ast.Arguments {
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
    const from: ast.Arguments.Index = @enumFromInt(self.arguments.items.len);
    try self.arguments.appendSlice(arg_list.items);
    const to: ast.Arguments.Index = @enumFromInt(self.arguments.items.len);
    return ast.Arguments{ .from = from, .to = to };
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
    const token = try self.tokenizer.next();
    if (token.tag == tag) {
        return token;
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Expected a {s}, got '{s}'",
        .{ @tagName(tag), token.toByteSlice(self.source) },
    );
    return ParseError.UnexpectedToken;
}

fn next(self: *Self) ParseError!Token {
    return self.tokenizer.next();
}

fn peek(self: *Self) ?Token {
    return self.tokenizer.peek();
}

fn isAtToken(self: *Self, tag: Token.Tag) bool {
    if (self.peek()) |token| {
        return token.tag == tag;
    }

    return false;
}

fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

fn checkActiveField(v: anytype, want: []const u8) bool {
    const tag_name = @tagName(v);
    return std.mem.eql(u8, tag_name, want);
}

pub fn toPretty(
    self: *const Self,
    allocator: std.mem.Allocator,
    node_id: Node.Index,
) !ast.NodePretty {
    const node = self.nodes.items[@intFromEnum(node_id)];
    switch (node) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try copy(allocator, try self.toPretty(allocator, payload.lhs));
            const rhs = try copy(allocator, try self.toPretty(allocator, payload.rhs));
            const token = self.tokens.items[@intFromEnum(payload.operator)];

            if (checkActiveField(node, "binary_expr")) {
                return .{
                    .binary_expression = .{
                        .lhs = lhs,
                        .rhs = rhs,
                        .operator = token.toByteSlice(self.source),
                    },
                };
            } else {
                return .{
                    .assignment_expression = .{
                        .lhs = lhs,
                        .rhs = rhs,
                        .operator = token.toByteSlice(self.source),
                    },
                };
            }
        },

        .identifier,
        .literal,
        => |tok_id| {
            const token = self.tokens.items[@intFromEnum(tok_id)];
            if (checkActiveField(node, "identifier")) {
                return .{ .identifier = token.toByteSlice(self.source) };
            } else {
                return .{ .literal = token.toByteSlice(self.source) };
            }
        },

        .this => return .{ .this = {} },
        .member_expr => |payload| {
            const obj = try copy(allocator, try self.toPretty(allocator, payload.object));
            const member = self.tokens.items[@intFromEnum(payload.property)];
            return .{ .member_expression = .{
                .object = obj,
                .property = member.toByteSlice(self.source),
            } };
        },

        .computed_member_expr => |payload| {
            const obj = try copy(allocator, try self.toPretty(allocator, payload.object));
            const member = try copy(allocator, try self.toPretty(allocator, payload.property));
            return .{
                .computed_member_expression = .{
                    .object = obj,
                    .property = member,
                },
            };
        },

        .update_expr, .post_unary_expr, .unary_expr => |payload| {
            const operand = try copy(
                allocator,
                try self.toPretty(allocator, payload.operand),
            );
            const token = self.tokens.items[@intFromEnum(payload.operator)];
            return if (checkActiveField(node, "post_unary_expr"))
                ast.NodePretty{
                    .post_unary_expression = .{
                        .operand = operand,
                        .operator = token.toByteSlice(self.source),
                    },
                }
            else
                .{
                    .unary_expression = .{
                        .operand = operand,
                        .operator = token.toByteSlice(self.source),
                    },
                };
        },

        .super_call_expr, .arguments => |maybe_args| {
            if (maybe_args) |arguments| {
                var new_args = std.ArrayList(ast.NodePretty).init(allocator);
                const from: usize = @intFromEnum(arguments.from);
                const to: usize = @intFromEnum(arguments.to);
                for (from..to) |i| {
                    const arg_node = self.arguments.items[i];
                    const new_arg = try self.toPretty(allocator, arg_node);
                    try new_args.append(new_arg);
                }
                return .{ .arguments = try new_args.toOwnedSlice() };
            } else {
                return .{ .arguments = try allocator.alloc(ast.NodePretty, 0) };
            }
        },

        .call_expr, .new_expr => |payload| {
            const callee = try copy(allocator, try self.toPretty(allocator, payload.callee));
            const arguments = try copy(allocator, try self.toPretty(allocator, payload.arguments));
            return if (checkActiveField(node, "call_expr")) .{
                .call_expression = .{
                    .callee = callee,
                    .arguments = arguments,
                },
            } else .{
                .new_expression = .{
                    .callee = callee,
                    .arguments = arguments,
                },
            };
        },

        .optional_expr => |payload| {
            const expr = try copy(allocator, try self.toPretty(allocator, payload));
            return .{ .optional_expression = expr };
        },
    }
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

            while (self.peek()) |token| {
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

            while (self.peek()) |token| {
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
