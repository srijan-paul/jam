const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig").Tokenizer;
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");
const types = @import("./types.zig");

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
    };
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit();
    self.tokens.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit();
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
    const token = self.peek() orelse return expr;
    if (token.tag == .@"++" and token.tag == .@"--") {
        _ = try self.next();
        return self.addNode(ast.Node{
            .post_unary_expr = .{
                .operand = expr,
                .operator = try self.addToken(token),
            },
        });
    }

    return expr;
}

fn lhsExpression(self: *Self) ParseError!Node.Index {
    return self.memberExpression();
}

fn memberExpression(self: *Self) ParseError!Node.Index {
    const primary_expression = try self.primaryExpression();
    const token = self.peek() orelse return primary_expression;
    switch (token.tag) {
        .@"." => {
            _ = try self.next(); // eat "."

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

            return self.addNode(ast.Node{
                .member_expr = .{
                    .object = primary_expression,
                    .property = property_token,
                },
            });
        },

        .@"[" => {
            _ = try self.next(); // eat '['
            const expr = try self.expression();
            _ = try self.expect(.@"]");

            return self.addNode(ast.Node{
                .computed_member_expr = ast.ComputedPropertyAccess{
                    .object = primary_expression,
                    .property = expr,
                },
            });
        },

        else => {
            return primary_expression;
        },
    }
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
    args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, args);
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

fn isAtToken(self: *Self, tag: Token.Tag) !Token {
    if (self.peek()) |token| {
        return token.tag == tag;
    }
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
                return ast.NodePretty{
                    .binary_expr = .{
                        .lhs = lhs,
                        .rhs = rhs,
                        .operator = token.toByteSlice(self.source),
                    },
                };
            } else {
                return ast.NodePretty{
                    .assignment_expr = .{
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
            return ast.NodePretty{
                .member_expr = .{
                    .object = obj,
                    .property = member.toByteSlice(self.source),
                },
            };
        },

        .computed_member_expr => |payload| {
            const obj = try copy(allocator, try self.toPretty(allocator, payload.object));
            const member = try copy(allocator, try self.toPretty(allocator, payload.property));
            return ast.NodePretty{
                .computed_member_expr = .{
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
            return ast.NodePretty{
                .unary_expr = .{
                    .operand = operand,
                    .operator = token.toByteSlice(self.source),
                },
            };
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
