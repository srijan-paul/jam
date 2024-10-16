const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");
const Node = ast.Node;

fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

fn checkActiveField(v: anytype, want: []const u8) bool {
    const tag_name = @tagName(v);
    return std.mem.eql(u8, tag_name, want);
}

/// Convert an AST Node to a struct that can be JSON serialized
/// in a human readable form.
fn toPretty(
    self: *const Parser,
    allocator: std.mem.Allocator,
    node_id: Node.Index,
) !ast.NodePretty {
    const node = self.nodes.items[@intFromEnum(node_id)];
    switch (node) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try copy(allocator, try toPretty(self, allocator, payload.lhs));
            const rhs = try copy(allocator, try toPretty(self, allocator, payload.rhs));
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
            const obj = try copy(allocator, try toPretty(self, allocator, payload.object));
            const member = self.tokens.items[@intFromEnum(payload.property)];
            return .{ .member_expression = .{
                .object = obj,
                .property = member.toByteSlice(self.source),
            } };
        },

        .computed_member_expr => |payload| {
            const obj = try copy(allocator, try toPretty(self, allocator, payload.object));
            const member = try copy(allocator, try toPretty(self, allocator, payload.property));
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
                try toPretty(self, allocator, payload.operand),
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
                    const new_arg = try toPretty(self, allocator, arg_node);
                    try new_args.append(new_arg);
                }
                return .{ .arguments = try new_args.toOwnedSlice() };
            } else {
                return .{ .arguments = try allocator.alloc(ast.NodePretty, 0) };
            }
        },

        .call_expr, .new_expr => |payload| {
            const callee = try copy(allocator, try toPretty(self, allocator, payload.callee));
            const arguments = try copy(allocator, try toPretty(self, allocator, payload.arguments));
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
            const expr = try copy(allocator, try toPretty(self, allocator, payload));
            return .{ .optional_expression = expr };
        },
    }
}

pub fn toJsonString(allocator: std.mem.Allocator, parser: *const Parser, node: ast.Node.Index) error{OutOfMemory}![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();
    defer arena.deinit();

    const pretty_node = try toPretty(parser, al, node);
    return try std.json.stringifyAlloc(allocator, pretty_node, .{});
}
