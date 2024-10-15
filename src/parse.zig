const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig").Tokenizer;
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");
const types = @import("./types.zig");

const Node = ast.Node;

const ParseError = error{ UnexpectedToken, OutOfMemory } || Tokenizer.Error;
const ParseFn = fn (self: *Self) ParseError!Node.Index;

// arranged in highest to lowest binding
const exponentExpr = makeRightAssoc(.@"**", Self.atomic);

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

fn expression(self: *Self) !Node.Index {
    return self.assignmentExpression();
}

fn assignmentExpression(self: *Self) !Node.Index {
    var node = try lOrExpr(self);
    // todo: check if `node` is valid LHS

    while (self.peek()) |token| {
        if (!token.isAssignmentOperator()) break;
        _ = try self.next();

        const rhs = try self.assignmentExpression();
        node = try self.addNode(.{
            .assignment_expr = .{
                .lhs = node,
                .rhs = rhs,
                .op = try self.addToken(token),
            },
        });
    }

    return node;
}

fn addToken(self: *Self, token: Token) error{OutOfMemory}!Token.Index {
    try self.tokens.append(token);
    return @enumFromInt(self.tokens.items.len - 1);
}

fn atomic(self: *Self) !Node.Index {
    const token = try self.next();
    switch (token.tag) {
        .numeric_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => {
            const i = try self.addToken(token);
            return try self.addNode(ast.Node{ .literal = i });
        },
        .identifier => {
            const i = try self.addToken(token);
            return try self.addNode(ast.Node{ .identifier = i });
        },
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
            const token = self.tokens.items[@intFromEnum(payload.op)];

            if (checkActiveField(node, "binary_expr")) {
                return ast.NodePretty{
                    .binary_expr = .{
                        .lhs = lhs,
                        .rhs = rhs,
                        .op = token.toByteSlice(self.source),
                    },
                };
            } else {
                return ast.NodePretty{
                    .assignment_expr = .{
                        .lhs = lhs,
                        .rhs = rhs,
                        .op = token.toByteSlice(self.source),
                    },
                };
            }
        },

        .literal,
        => |tok_id| {
            const token = self.tokens.items[@intFromEnum(tok_id)];
            return ast.NodePretty{
                .literal = token.toByteSlice(self.source),
            };
        },

        .identifier => |tok_id| {
            const token = self.tokens.items[@intFromEnum(tok_id)];
            return ast.NodePretty{
                .literal = token.toByteSlice(self.source),
            };
        },
    }
}

/// make a right associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeRightAssoc(toktag: Token.Tag, l: *const ParseFn) *const ParseFn {
    const S = struct {
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
                        .op = try self.addToken(token),
                    },
                });
            }

            return node;
        }
    };

    return &S.parseFn;
}

/// make a left associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeLeftAssoc(
    tag_min: Token.Tag,
    tag_max: Token.Tag,
    nextFn: *const ParseFn,
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
                            .op = try self.addToken(token),
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
