// A zig port of [ESTraverse](https://github.com/estools/estraverse),
// modified to suit Jam's syntax tree, and tweaked for performance.

const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("./ast.zig");

const NodeKind = std.meta.Tag(ast.NodeData);

/// Create a struct that traverses an AST, and calls `TControl.onEnter`
/// every time it enters a node, and `TControl.onExit` every time it exits a node.
/// This structure that determines what actions to perform is called a "Control".
/// (can I come up with a better name?).
pub fn Traverser(TControl: type) type {
    const ctrl_type: std.builtin.Type = @typeInfo(TControl);
    if (std.meta.activeTag(ctrl_type) != .@"struct") {
        @compileError("type of `control` passed to `Traverser` must have a struct type");
    }

    return struct {
        const Self = @This();

        t: *const ast.Tree,
        ctrl: *TControl,

        // TODO: get rid of this `anyerror`.
        fn subRange(self: *Self, maybe_statements: ?ast.SubRange) anyerror!void {
            const stat_range = maybe_statements orelse return;
            const children = self.t.getSubRange(stat_range.from, stat_range.to);
            for (children) |child| try self.visit(child);
        }

        fn binaryPayload(self: *Self, binary: *const ast.BinaryPayload) anyerror!void {
            try self.visit(binary.lhs);
            try self.visit(binary.rhs);
        }

        fn visit(self: *Self, node_id: ast.Node.Index) anyerror!void {
            const data = self.t.nodeData(node_id).*;
            if (@hasDecl(TControl, "onEnter"))
                try self.ctrl.onEnter(node_id, data);

            switch (data) {
                // payload is ?SubRange
                .program,
                .array_literal,
                .array_pattern,
                .parameters,
                .block_statement,
                .object_pattern,
                .object_literal,
                => |maybe_statements| if (maybe_statements) |stmts| {
                    try self.subRange(stmts);
                },

                .object_property => |o| {
                    try self.visit(o.key);
                    try self.visit(o.value);
                },

                .if_statement, .conditional_expr => |if_pl| {
                    try self.visit(if_pl.condition);
                    try self.visit(if_pl.consequent);
                    try self.visit(if_pl.alternate);
                },

                .while_statement, .do_while_statement => |while_pl| {
                    try self.visit(while_pl.condition);
                    try self.visit(while_pl.body);
                },

                // payload is Node.Index
                .expression_statement,
                .optional_expr,
                .spread_element,
                .rest_element,
                .throw_statement,
                => |e| try self.visit(e),

                .class_declaration,
                .class_expression,
                => |class_pl| {
                    const info = self.t.getExtraData(class_pl.class_information).class;
                    if (info.name) |name| try self.visit(name);
                    try self.visit(info.super_class);
                    try self.subRange(class_pl.body);
                },

                .return_statement => |maybe_node_id| {
                    if (maybe_node_id) |id| try self.visit(id);
                },

                .template_literal => |nodes| try self.subRange(nodes),

                .labeled_statement => |pl| {
                    try self.visit(pl.label);
                    try self.visit(pl.body);
                },

                // payload is UnaryPayload
                .unary_expr,
                .update_expr,
                .await_expr,
                => |u| try self.visit(u.operand),

                .yield_expr => |y| {
                    if (y.value) |val|
                        try self.visit(val);
                },

                // payload is BinaryPayload
                .assignment_expr,
                .binary_expr,
                .assignment_pattern,
                => |binary| try self.binaryPayload(&binary),

                .variable_declaration => |decl| {
                    try self.subRange(decl.declarators);
                },

                .variable_declarator => |decl| {
                    try self.visit(decl.lhs);
                    if (decl.init) |init_expr_id|
                        try self.visit(init_expr_id);
                },

                .try_statement => |try_pl| {
                    try self.visit(try_pl.body);
                    try self.visit(try_pl.catch_clause);
                    try self.visit(try_pl.finalizer);
                },

                .catch_clause => |catch_pl| {
                    if (catch_pl.param) |param| try self.visit(param);
                    try self.visit(catch_pl.body);
                },

                .function_declaration, .function_expr => |func_pl| {
                    if (self.t.getExtraData(func_pl.info).function.name) |func_name| {
                        try self.visit(func_name);
                    }
                    try self.visit(func_pl.body);
                },

                .for_of_statement,
                .for_in_statement,
                .for_statement,
                => |for_pl| {
                    const iterator = self.t.getExtraData(for_pl.iterator);
                    switch (data) {
                        .for_statement => {
                            try self.visit(iterator.for_iterator.init);
                            try self.visit(iterator.for_iterator.condition);
                            try self.visit(iterator.for_iterator.update);
                        },

                        else => {
                            try self.visit(iterator.for_in_of_iterator.left);
                            try self.visit(iterator.for_in_of_iterator.right);
                        },
                    }

                    try self.visit(for_pl.body);
                },

                // leaf nodes cannot be explored further
                .number_literal,
                .string_literal,
                .boolean_literal,
                .null_literal,
                .regex_literal,
                .identifier,
                .template_element,
                .empty_statement,
                .debugger_statement,
                .none,
                .this,
                => {},
                else => std.debug.panic("not implemented: {s}!\n", .{@tagName(std.meta.activeTag(data))}),
            }

            if (@hasDecl(TControl, "onExit"))
                try self.ctrl.onExit(node_id, data);
        }

        pub fn traverse(self: *Self) !void {
            try self.visit(self.t.root);
        }
    };
}

const t = std.testing;

fn parse(program: []const u8) !Parser.Result {
    var parser = try Parser.init(t.allocator, program, .{});
    defer parser.deinit();
    return try parser.parse();
}

fn enterNode(_: ast.Node.Index, pl: ast.NodeData) !void {
    std.debug.print("visiting: {s}\n", .{@tagName(std.meta.activeTag(pl))});
}

const TestCtrl = struct {
    allocator: std.mem.Allocator,
    visited: std.ArrayList(NodeKind),

    pub fn init() TestCtrl {
        return .{
            .allocator = t.allocator,
            .visited = std.ArrayList(NodeKind).init(t.allocator),
        };
    }

    pub fn deinit(self: *TestCtrl) void {
        self.visited.deinit();
    }

    pub fn onEnter(self: *TestCtrl, _: ast.Node.Index, pl: ast.NodeData) !void {
        try self.visited.append(std.meta.activeTag(pl));
    }
};

test Traverser {
    var p = try parse("a = 1 + 2;");
    defer p.deinit();

    var ctrl = TestCtrl.init();
    defer ctrl.deinit();

    var traverser = Traverser(TestCtrl){ .t = p.tree, .ctrl = &ctrl };
    try traverser.traverse();

    try t.expectEqualSlices(NodeKind, &.{
        .program,
        .expression_statement,
        .assignment_expr,
        .identifier,
        .binary_expr,
        .number_literal,
        .number_literal,
    }, ctrl.visited.items);
}
