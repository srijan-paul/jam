const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");
const Node = ast.Node;
const util = @import("util");

fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

fn checkActiveField(v: anytype, want: []const u8) bool {
    const tag_name = @tagName(v);
    return std.mem.eql(u8, tag_name, want);
}

fn prettySubRange(
    allocator: std.mem.Allocator,
    self: *const Parser,
    maybe_args: ?ast.SubRange,
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

fn escapeUtf8(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 0 };
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    while (iter.i < str.len) {
        if (str[iter.i] == '\\') {
            const parsed_cp = util.parseUnicodeEscape(str[iter.i..]) orelse
                unreachable; // validated during tokenization.
            const cp = parsed_cp.codepoint;
            var cp_slice: [4]u8 = undefined;
            const cp_len = std.unicode.utf8Encode(cp, &cp_slice) catch
                unreachable;
            try buf.appendSlice(cp_slice[0..cp_len]);
            iter.i += parsed_cp.len;
            continue;
        }

        const cp_slice = iter.nextCodepointSlice() orelse
            unreachable; // already validated UTF-8 during tokenization
        try buf.appendSlice(cp_slice);
    }

    return buf.toOwnedSlice();
}

/// Convert an AST Node to a struct that can be JSON serialized
/// in a human readable form.
fn toPretty(
    self: *const Parser,
    al: std.mem.Allocator,
    node_id: Node.Index,
) !ast.NodePretty {
    const node = self.nodes.get(@intFromEnum(node_id));
    switch (node.data) {
        .binary_expr,
        .assignment_expr,
        .assignment_pattern,
        => |payload| {
            const lhs = try copy(al, try toPretty(self, al, payload.lhs));
            const rhs = try copy(al, try toPretty(self, al, payload.rhs));
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
            else if (checkActiveField(node.data, "assignment_expr"))
                assignment
            else
                .{
                    .assignment_pattern = .{ .lhs = lhs, .rhs = rhs, .operator = operator },
                };
        },

        .identifier,
        .literal,
        => |tok_id| {
            const token = self.tokens.items[@intFromEnum(tok_id)];
            if (checkActiveField(node.data, "identifier")) {
                const id = token.toByteSlice(self.source);
                const escaped = try escapeUtf8(al, id);
                return .{ .identifier = escaped };
            } else {
                return .{ .literal = token.toByteSlice(self.source) };
            }
        },

        .this => return .{ .this = {} },
        .member_expr => |payload| {
            const obj = try copy(al, try toPretty(self, al, payload.object));
            const member = self.tokens.items[@intFromEnum(payload.property)];
            return .{ .member_expression = .{
                .object = obj,
                .property = member.toByteSlice(self.source),
            } };
        },

        .computed_member_expr => |payload| {
            const obj = try copy(al, try toPretty(self, al, payload.object));
            const member = try copy(al, try toPretty(self, al, payload.property));
            return .{
                .computed_member_expression = .{
                    .object = obj,
                    .property = member,
                },
            };
        },

        .update_expr, .post_unary_expr, .unary_expr, .await_expr => |payload| {
            const operand = try copy(
                al,
                try toPretty(self, al, payload.operand),
            );
            const token = self.tokens.items[@intFromEnum(payload.operator)];
            const unary_pl = .{
                .operand = operand,
                .operator = token.toByteSlice(self.source),
            };

            return if (checkActiveField(node.data, "post_unary_expr"))
                .{ .post_unary_expression = unary_pl }
            else if (checkActiveField(node.data, "await_expr"))
                .{ .await_expression = unary_pl }
            else
                .{ .unary_expression = unary_pl };
        },

        .yield_expr => |pl| {
            const value = if (pl.value) |v|
                try copy(al, try toPretty(self, al, v))
            else
                null;
            return ast.NodePretty{
                .yield_expression = .{
                    .value = value,
                    .is_delegated = pl.is_delegated,
                },
            };
        },

        .super_call_expr, .arguments => |maybe_args| return .{
            .arguments = try prettySubRange(al, self, maybe_args),
        },

        .call_expr, .new_expr => |payload| {
            const callee = try copy(al, try toPretty(self, al, payload.callee));
            const arguments = try copy(al, try toPretty(self, al, payload.arguments));
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
                .sequence_expression = try prettySubRange(al, self, nodes),
            };
        },

        .optional_expr => |payload| {
            const expr = try copy(al, try toPretty(self, al, payload));
            return .{ .optional_expression = expr };
        },

        .empty_array_item => return .{ .empty_array_item = {} },
        .array_literal => |items| return .{
            .array = try prettySubRange(al, self, items),
        },

        .array_pattern => |items| return .{
            .array_pattern = try prettySubRange(al, self, items),
        },

        .object_literal => |properties| return .{
            .object_literal = try prettySubRange(al, self, properties),
        },

        .object_pattern => |properties| return .{
            .object_pattern = try prettySubRange(al, self, properties),
        },

        .object_property => |prop| return ast.NodePretty{
            .object_property = .{
                .key = try copy(al, try toPretty(self, al, prop.key)),
                .value = try copy(al, try toPretty(self, al, prop.value)),
                .flags = .{
                    .is_computed = prop.flags.is_computed,
                    .is_shorthand = prop.flags.is_shorthand,
                    .is_method = prop.flags.is_method,
                    .kind = prop.flags.kind,
                },
            },
        },
        .spread_element => |payload| {
            const expr = try copy(al, try toPretty(self, al, payload));
            return .{ .spread_element = expr };
        },

        .rest_element => |payload| {
            const expr = try copy(al, try toPretty(self, al, payload));
            return .{ .rest_element = expr };
        },

        .return_statement => |operand| {
            const ret_operand = if (operand) |arg|
                try copy(al, try toPretty(self, al, arg))
            else
                null;
            return .{
                .return_statement = ret_operand,
            };
        },

        .conditional_expr, .if_statement => |cond_expr| {
            const cond = try copy(al, try toPretty(self, al, cond_expr.condition));
            const consequent = try copy(al, try toPretty(self, al, cond_expr.consequent));
            const alternate = try copy(al, try toPretty(self, al, cond_expr.alternate));
            const conditional = .{
                .condition = cond,
                .consequent = consequent,
                .alternate = alternate,
            };

            return if (checkActiveField(node.data, "conditional_expr")) .{
                .conditional_expression = conditional,
            } else .{
                .if_statement = conditional,
            };
        },

        .while_statement => |stmt| {
            const cond = try copy(al, try toPretty(self, al, stmt.condition));
            const body = try copy(al, try toPretty(self, al, stmt.body));

            return .{
                .while_statement = .{
                    .condition = cond,
                    .body = body,
                },
            };
        },

        .for_statement => |stmt| {
            const extra = self.getExtraData(stmt.iterator).for_iterator;
            const init = if (extra.init != Node.Index.empty)
                try copy(al, try toPretty(self, al, extra.init))
            else
                null;

            const condition = if (extra.condition != Node.Index.empty)
                try copy(al, try toPretty(self, al, extra.condition))
            else
                null;

            const update = if (extra.update != Node.Index.empty)
                try copy(al, try toPretty(self, al, extra.update))
            else
                null;

            const body = try copy(al, try toPretty(self, al, stmt.body));
            return .{
                .for_statement = .{
                    .init = init,
                    .condition = condition,
                    .update = update,
                    .body = body,
                },
            };
        },

        .expression_statement => |expr| {
            const expression = try copy(al, try toPretty(self, al, expr));
            return .{ .expression_statement = expression };
        },

        .block_statement => |block| {
            const body = try prettySubRange(al, self, block);
            return .{ .block_statement = body };
        },

        .program => |block| {
            const body = try prettySubRange(al, self, block);
            return .{ .program = body };
        },

        .parameters => |params| {
            const p_list = try prettySubRange(al, self, params);
            return .{ .parameters = p_list };
        },

        .empty_statement => return .{ .empty_statement = {} },
        .debugger_statement => return .{ .debugger_statement = {} },

        .variable_declarator => |decl| {
            const lhs = try copy(al, try toPretty(self, al, decl.lhs));
            const init = if (decl.init) |init|
                try copy(al, try toPretty(self, al, init))
            else
                null;
            return .{
                .variable_declarator = .{
                    .lhs = lhs,
                    .init = init,
                },
            };
        },

        .variable_declaration => |d| {
            const decls = try prettySubRange(al, self, d.declarators);
            return .{
                .variable_declaration = .{
                    .declarators = decls,
                    .kind = d.kind,
                },
            };
        },

        .function_expr, .function_declaration => |f| {
            const body = try copy(al, try toPretty(self, al, f.body));
            const params = try copy(al, try toPretty(self, al, f.parameters));
            const info = self.extra_data.items[@intFromEnum(f.info)];
            const func_flags = info.function.flags;
            const func_name = f.getName(self);
            const flags = .{
                .is_async = func_flags.is_async,
                .is_generator = func_flags.is_generator,
                .is_arrow = func_flags.is_arrow,
            };

            return .{
                .function = .{
                    .body = body,
                    .parameters = params,
                    .info = .{
                        .function = .{
                            .flags = flags,
                            .name = func_name,
                        },
                    },
                },
            };
        },

        .none => {
            return .{ .none = {} };
        },
    }
}

pub fn toJsonString(allocator: std.mem.Allocator, parser: *const Parser, node: ast.Node.Index) error{OutOfMemory}![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();
    defer arena.deinit();

    const pretty_node = try toPretty(parser, al, node);
    return try std.json.stringifyAlloc(allocator, pretty_node, .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });
}
