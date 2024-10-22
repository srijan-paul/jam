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

fn prettyNodeList(
    allocator: std.mem.Allocator,
    self: *const Parser,
    maybe_args: ?ast.NodeList,
) error{OutOfMemory}![]ast.NodePretty {
    if (maybe_args) |arguments| {
        var new_args = std.ArrayList(ast.NodePretty).init(allocator);
        const from: usize = @intFromEnum(arguments.from);
        const to: usize = @intFromEnum(arguments.to);
        for (from..to) |i| {
            const arg_node = self.node_lists.items[i];
            const new_arg = try toPretty(self, allocator, arg_node);
            try new_args.append(new_arg);
        }
        return try new_args.toOwnedSlice();
    }
    return try allocator.alloc(ast.NodePretty, 0);
}

/// Convert an AST Node to a struct that can be JSON serialized
/// in a human readable form.
fn toPretty(
    self: *const Parser,
    allocator: std.mem.Allocator,
    node_id: Node.Index,
) !ast.NodePretty {
    const node = self.nodes.items[@intFromEnum(node_id)];
    switch (node.data) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try copy(allocator, try toPretty(self, allocator, payload.lhs));
            const rhs = try copy(allocator, try toPretty(self, allocator, payload.rhs));
            const token = self.tokens.items[@intFromEnum(payload.operator)];

            const operator = token.toByteSlice(self.source);

            const binary = .{
                .binary_expression = .{ .lhs = lhs, .rhs = rhs, .operator = operator },
            };
            const assignment = .{
                .assignment_expression = .{ .lhs = lhs, .rhs = rhs, .operator = operator },
            };

            return if (checkActiveField(node.data, "binary_expr"))
                binary
            else
                assignment;
        },

        .identifier,
        .literal,
        => |tok_id| {
            const token = self.tokens.items[@intFromEnum(tok_id)];
            if (checkActiveField(node.data, "identifier")) {
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
            return if (checkActiveField(node.data, "post_unary_expr"))
                .{
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

        .super_call_expr, .arguments => |maybe_args| return .{
            .arguments = try prettyNodeList(allocator, self, maybe_args),
        },

        .call_expr, .new_expr => |payload| {
            const callee = try copy(allocator, try toPretty(self, allocator, payload.callee));
            const arguments = try copy(allocator, try toPretty(self, allocator, payload.arguments));
            return if (checkActiveField(node.data, "call_expr")) .{
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

        .sequence_expr => |nodes| {
            return .{
                .sequence_expression = try prettyNodeList(allocator, self, nodes),
            };
        },

        .optional_expr => |payload| {
            const expr = try copy(allocator, try toPretty(self, allocator, payload));
            return .{ .optional_expression = expr };
        },

        .empty_array_item => return .{ .empty_array_item = {} },
        .array_literal => |items| return .{
            .array = try prettyNodeList(allocator, self, items),
        },

        .object_literal => |properties| return .{
            .object_literal = try prettyNodeList(allocator, self, properties),
        },

        .object_property => |prop| return ast.NodePretty{
            .object_property = .{
                .key = try copy(allocator, try toPretty(self, allocator, prop.key)),
                .value = try copy(allocator, try toPretty(self, allocator, prop.value)),
            },
        },
        .spread_element => |payload| {
            const expr = try copy(allocator, try toPretty(self, allocator, payload));
            return .{ .spread_element = expr };
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
