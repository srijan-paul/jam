// A zig port of [ESTraverse](https://github.com/estools/estraverse),
// modified to suit Jam's syntax tree, and tweaked for performance.

const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("./ast.zig");

const NodeKind = std.meta.Tag(ast.NodeData);

/// TODO: use a stack instead of recursion.
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
        // TODO: should `*Self` be const?
        fn subRange(self: *Self, maybe_statements: ?ast.SubRange, parent_id: ?ast.Node.Index) anyerror!void {
            const stat_range = maybe_statements orelse return;
            const children = self.t.getSubRange(stat_range.from, stat_range.to);
            for (children) |child| try self.visit(child, parent_id);
        }

        fn binaryPayload(self: *Self, binary: *const ast.BinaryPayload, parent_id: ast.Node.Index) anyerror!void {
            try self.visit(binary.lhs, parent_id);
            try self.visit(binary.rhs, parent_id);
        }

        fn visit(self: *Self, node_id: ast.Node.Index, parent_id: ?ast.Node.Index) anyerror!void {
            const data = node_id.get(self.t).*;
            if (@hasDecl(TControl, "onEnter"))
                try self.ctrl.onEnter(node_id, data, parent_id);

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
                    try self.subRange(stmts, node_id);
                },

                .statement_list => |stmts| try self.subRange(stmts, node_id),

                .object_property => |o| {
                    try self.visit(o.key, node_id);
                    try self.visit(o.value, node_id);
                },

                .shorthand_property => |o| try self.visit(o.name, node_id),

                .if_statement, .conditional_expr => |if_pl| {
                    try self.visit(if_pl.condition, node_id);
                    try self.visit(if_pl.consequent, node_id);
                    try self.visit(if_pl.alternate, node_id);
                },

                .while_statement, .do_while_statement => |while_pl| {
                    try self.visit(while_pl.condition, node_id);
                    try self.visit(while_pl.body, node_id);
                },

                // payload is Node.Index
                .expression_statement,
                .optional_expr,
                .spread_element,
                .rest_element,
                .throw_statement,
                => |e| try self.visit(e, node_id),

                .class_declaration,
                .class_expression,
                => |class_pl| {
                    try self.visit(class_pl.meta, node_id);
                    try self.subRange(class_pl.body, node_id);
                },

                .class_meta => |meta_pl| {
                    if (meta_pl.name != .empty)
                        try self.visit(meta_pl.name, node_id);
                    if (meta_pl.super_class != .empty)
                        try self.visit(meta_pl.super_class, node_id);
                },

                .return_statement => |maybe_node_id| {
                    if (maybe_node_id) |id| try self.visit(id, node_id);
                },

                .template_literal => |nodes| try self.subRange(nodes, node_id),

                .labeled_statement => |pl| {
                    try self.visit(pl.label, node_id);
                    try self.visit(pl.body, node_id);
                },

                // payload is UnaryPayload
                .unary_expr,
                .update_expr,
                .await_expr,
                .post_unary_expr,
                => |u| try self.visit(u.operand, node_id),

                .sequence_expr => |seq| try self.subRange(seq, node_id),
                .parenthesized_expr => |expr| try self.visit(expr, node_id),
                .class_field, .class_method => |field| {
                    try self.visit(field.key, node_id);
                    try self.visit(field.value, node_id);
                },

                .yield_expr => |y| {
                    if (y.value) |val|
                        try self.visit(val, node_id);
                },

                // payload is BinaryPayload
                .assignment_expr,
                .binary_expr,
                .assignment_pattern,
                => |binary| try self.binaryPayload(&binary, node_id),

                .variable_declaration => |decl| {
                    try self.subRange(decl.declarators, node_id);
                },

                .variable_declarator => |decl| {
                    try self.visit(decl.lhs, node_id);
                    if (decl.init) |init_expr_id|
                        try self.visit(init_expr_id, node_id);
                },

                .try_statement => |try_pl| {
                    try self.visit(try_pl.body, node_id);
                    try self.visit(try_pl.catch_clause, node_id);
                    try self.visit(try_pl.finalizer, node_id);
                },

                .catch_clause => |catch_pl| {
                    if (catch_pl.param) |param| try self.visit(param, node_id);
                    try self.visit(catch_pl.body, node_id);
                },

                .function_declaration, .function_expr => |func_pl| {
                    try self.visit(func_pl.meta, node_id);
                    try self.visit(func_pl.parameters, node_id);
                    try self.visit(func_pl.body, node_id);
                },

                .function_meta => |func_meta_pl| {
                    if (func_meta_pl.name) |name|
                        try self.visit(name, node_id);
                },

                .for_of_statement,
                .for_in_statement,
                .for_statement,
                => |for_pl| {
                    try self.visit(for_pl.body, node_id);
                    try self.visit(for_pl.iterator, node_id);
                },

                .for_iterator => |iter| {
                    try self.visit(iter.init, node_id);
                    try self.visit(iter.condition, node_id);
                    try self.visit(iter.update, node_id);
                },

                .for_in_of_iterator => |iter| {
                    try self.visit(iter.left, node_id);
                    try self.visit(iter.right, node_id);
                },

                .call_expr => |call_pl| {
                    try self.visit(call_pl.callee, node_id);
                    try self.visit(call_pl.arguments, node_id);
                },

                .new_expr => |call_pl| {
                    try self.visit(call_pl.callee, node_id);
                    if (call_pl.arguments) |args|
                        try self.visit(args, node_id);
                },

                .super_call_expr => |super_pl| try self.subRange(super_pl, node_id),
                .arguments => |args| try self.subRange(args, node_id),

                .meta_property => |meta_pl| {
                    try self.visit(meta_pl.meta, node_id);
                    try self.visit(meta_pl.property, node_id);
                },

                .tagged_template_expr => |tagged_pl| {
                    try self.visit(tagged_pl.tag, node_id);
                    try self.visit(tagged_pl.template, node_id);
                },

                .member_expr => |member_pl| {
                    try self.visit(member_pl.object, node_id);
                    try self.visit(member_pl.property, node_id);
                },

                .computed_member_expr => |member_pl| {
                    try self.visit(member_pl.object, node_id);
                    try self.visit(member_pl.property, node_id);
                },

                .with_statement => |with_pl| {
                    try self.visit(with_pl.object, node_id);
                    try self.visit(with_pl.body, node_id);
                },

                .switch_statement => |switch_pl| {
                    try self.visit(switch_pl.discriminant, node_id);
                    try self.subRange(switch_pl.cases, node_id);
                },

                .switch_case => |case_pl| {
                    try self.visit(case_pl.expression, node_id);
                    try self.subRange(case_pl.consequent, node_id);
                },

                .default_case => |case_pl| try self.subRange(case_pl.consequent, node_id),

                .import_declaration => |import_pl| {
                    try self.visit(import_pl.source, node_id);
                    try self.subRange(import_pl.specifiers, node_id);
                },

                .import_default_specifier => |ns| try self.visit(ns.name, node_id),
                .import_namespace_specifier => |ns| try self.visit(ns.name, node_id),
                .import_specifier => |ns| {
                    if (ns.imported) |imported|
                        try self.visit(imported, node_id);
                    try self.visit(ns.local, node_id);
                },

                .export_declaration => |export_pl| try self.visit(export_pl.declaration, node_id),
                .export_all_declaration => |export_pl| {
                    try self.visit(export_pl.source, node_id);
                    if (export_pl.name) |name| try self.visit(name, node_id);
                },

                .export_list_declaration => |export_pl| try self.subRange(export_pl.specifiers, node_id),
                .export_from_declaration => |export_pl| {
                    try self.visit(export_pl.source, node_id);
                    try self.subRange(export_pl.specifiers, node_id);
                },

                .export_specifier => |export_pl| {
                    if (export_pl.exported) |exported|
                        try self.visit(exported, node_id);
                    try self.visit(export_pl.local, node_id);
                },

                .break_statement,
                .continue_statement,
                => |b| if (b.label) |label| try self.visit(label, node_id),

                .jsx_fragment => |fragment| try self.subRange(fragment.children, node_id),
                .jsx_expression => |expr| try self.visit(expr, node_id),
                .jsx_spread_child => |argument| try self.visit(argument, node_id),
                .jsx_element => |element| {
                    try self.visit(element.opening_element, node_id);
                    try self.visit(element.closing_element, node_id);
                    if (element.children != .empty)
                        try self.visit(element.children, node_id);
                },

                .jsx_opening_element => |element| {
                    try self.visit(element.name, node_id);
                    try self.subRange(element.attributes, node_id);
                },
                .jsx_self_closing_element => |element| {
                    try self.visit(element.name, node_id);
                    try self.subRange(element.attributes, node_id);
                },
                .jsx_closing_element => |element| try self.visit(element.name, node_id),

                .jsx_member_expression => |member| {
                    try self.visit(member.object, node_id);
                    try self.visit(member.property, node_id);
                },

                .jsx_namespaced_name => |name| {
                    try self.visit(name.namespace, node_id);
                    try self.visit(name.name, node_id);
                },

                .jsx_children => |children| try self.subRange(children, node_id),
                .jsx_attribute => |attr| {
                    try self.visit(attr.name, node_id);
                    if (attr.value) |value| try self.visit(value, node_id);
                },

                .jsx_spread_attribute => |arg| try self.visit(arg, node_id),

                // leaf nodes cannot be explored further
                .number_literal,
                .string_literal,
                .boolean_literal,
                .null_literal,
                .regex_literal,
                .identifier,
                .binding_identifier,
                .identifier_reference,
                .template_element,
                .empty_statement,
                .debugger_statement,
                .empty_array_item,
                .none,
                .this,
                .super,
                .jsx_text,
                .jsx_identifier,
                .jsx_identifier_reference,
                => {},
            }

            if (@hasDecl(TControl, "onExit"))
                try self.ctrl.onExit(node_id, data, parent_id);
        }

        pub fn traverse(self: *Self) !void {
            try self.visit(self.t.root, null);
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
            .visited = .{},
        };
    }

    pub fn deinit(self: *TestCtrl) void {
        self.visited.deinit(self.allocator);
    }

    pub fn onEnter(
        self: *TestCtrl,
        _: ast.Node.Index,
        pl: ast.NodeData,
        _: ?ast.Node.Index,
    ) !void {
        try self.visited.append(self.allocator, std.meta.activeTag(pl));
    }
};

test Traverser {
    var p = try parse("a = 1 + 2;");
    defer p.deinit();

    var ctrl = TestCtrl.init();
    defer ctrl.deinit();

    var traverser = Traverser(TestCtrl){ .t = p.tree, .ctrl = &ctrl };
    try traverser.traverse();

    const expected = [_]NodeKind{
        .program,
        .expression_statement,
        .assignment_expr,
        .identifier_reference,
        .binary_expr,
        .number_literal,
        .number_literal,
    };
    try t.expectEqualSlices(NodeKind, &expected, ctrl.visited.items);

    const Iterator = @import("./iterator.zig");
    var iter = try Iterator.init(t.allocator, p.tree, p.tree.root);
    defer iter.deinit();

    const meta = std.meta;

    var i: usize = 0;
    while (iter.next()) |item| : (i += 1) {
        try t.expectEqual(expected[i], meta.activeTag(item.node_id.get(p.tree).*));
        try iter.enqueueChildren(item.node_id);
    }
}
