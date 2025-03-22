// !THIS IS A GENERATED FILE!
// To regenerate, run `zig build astgen`.
// To modify the generation process, see: tools/gen/main.zig
const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Tree = ast.Tree;
const NodeData = ast.NodeData;
const Node = ast.Node;

const Self = @This();

/// A Node Id + the data it contains,
/// returned by a node iterator.
const Item = struct {
    /// Node ID
    node_id: Node.Index,
    /// Parent of this node
    parent_id: ?Node.Index,
};

allocator: Allocator,

t: *const Tree,
stack: std.ArrayListUnmanaged(Item),
/// Slice of all node payloads from the AST
node_pls: []const NodeData,

pub fn init(
    allocator: std.mem.Allocator,
    tree: *const Tree,
    start_node_id: Node.Index,
) Allocator.Error!Self {
    const node_pls = tree.nodes.items(.data);
    var stack = try std.ArrayListUnmanaged(Item).initCapacity(allocator, 128);
    stack.appendAssumeCapacity(.{
        .node_id = start_node_id,
        .parent_id = null,
    });

    return Self{
        .t = tree,
        .allocator = allocator,
        .node_pls = node_pls,
        .stack = stack,
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit(self.allocator);
}

pub fn childrenOf(
    allocator: Allocator,
    tree: *const Tree,
    start_node_id: Node.Index,
) Allocator.Error!Self {
    var iter = Self.init(allocator, tree, start_node_id);
    try iter.enqueueChildren(start_node_id);
    return iter;
}

/// Get the next node from the node iterator.
pub fn next(self: *Self) ?Item {
    return self.stack.pop();
}

pub fn pushNode(
    self: *Self,
    node_id: Node.Index,
    parent_id: ?Node.Index,
) Allocator.Error!void {
    try self.stack.append(self.allocator, .{
        .node_id = node_id,
        .parent_id = parent_id,
    });
}

fn visitSubRange(
    self: *Self,
    range: *const ast.SubRange,
    parent_id: ?Node.Index,
) Allocator.Error!void {
    const from: usize = @intFromEnum(range.from);
    const to: usize = @intFromEnum(range.to);

    if (from == to) return;

    var i = to;
    while (i > from) : (i -= 1) {
        const node_id = self.t.node_indices.items[i - 1];
        try self.pushNode(node_id, parent_id);
    }
}
fn visitFunction(
    self: *Self,
    data: *const ast.Function,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.meta, node_id);
    try self.pushNode(data.body, node_id);
    try self.pushNode(data.parameters, node_id);
}

fn visitExportFromDeclaration(
    self: *Self,
    data: *const ast.ExportFromDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.specifiers, node_id);
    try self.pushNode(data.source, node_id);
}

fn visitExportListDeclaration(
    self: *Self,
    data: *const ast.ExportListDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.specifiers, node_id);
}

fn visitForIterator(
    self: *Self,
    data: *const ast.ForIterator,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.update, node_id);
    try self.pushNode(data.condition, node_id);
    try self.pushNode(data.init, node_id);
}

fn visitPropertyDefinition(
    self: *Self,
    data: *const ast.PropertyDefinition,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.value, node_id);
    try self.pushNode(data.key, node_id);
}

fn visitForStatement(
    self: *Self,
    data: *const ast.ForStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.body, node_id);
    try self.pushNode(data.iterator, node_id);
}

fn visitImportDefaultSpecifier(
    self: *Self,
    data: *const ast.ImportDefaultSpecifier,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.name, node_id);
}

fn visitUnaryPayload(
    self: *Self,
    data: *const ast.UnaryPayload,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.operand, node_id);
}

fn visitTryStatement(
    self: *Self,
    data: *const ast.TryStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.finalizer, node_id);
    try self.pushNode(data.catch_clause, node_id);
    try self.pushNode(data.body, node_id);
}

fn visitExportSpecifier(
    self: *Self,
    data: *const ast.ExportSpecifier,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.exported) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.local, node_id);
}

fn visitShorthandProperty(
    self: *Self,
    data: *const ast.ShorthandProperty,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.name, node_id);
}

fn visitComputedPropertyAccess(
    self: *Self,
    data: *const ast.ComputedPropertyAccess,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.property, node_id);
    try self.pushNode(data.object, node_id);
}

fn visitWhileStatement(
    self: *Self,
    data: *const ast.WhileStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.body, node_id);
    try self.pushNode(data.condition, node_id);
}

fn visitBinaryPayload(
    self: *Self,
    data: *const ast.BinaryPayload,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.rhs, node_id);
    try self.pushNode(data.lhs, node_id);
}

fn visitClassMeta(
    self: *Self,
    data: *const ast.ClassMeta,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.super_class, node_id);
    try self.pushNode(data.name, node_id);
}

fn visitImportSpecifier(
    self: *Self,
    data: *const ast.ImportSpecifier,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.local, node_id);
    if (data.imported) |_pl|
        try self.pushNode(_pl, node_id);
}

fn visitJsxOpeningElement(
    self: *Self,
    data: *const ast.JsxOpeningElement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.attributes, node_id);
    try self.pushNode(data.name, node_id);
}

fn visitWithStatement(
    self: *Self,
    data: *const ast.WithStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.body, node_id);
    try self.pushNode(data.object, node_id);
}

fn visitNewExpr(
    self: *Self,
    data: *const ast.NewExpr,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.arguments) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.callee, node_id);
}

fn visitExportedDeclaration(
    self: *Self,
    data: *const ast.ExportedDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.declaration, node_id);
}

fn visitSwitchDefaultCase(
    self: *Self,
    data: *const ast.SwitchDefaultCase,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.consequent, node_id);
}

fn visitCallExpr(
    self: *Self,
    data: *const ast.CallExpr,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.arguments, node_id);
    try self.pushNode(data.callee, node_id);
}

fn visitConditional(
    self: *Self,
    data: *const ast.Conditional,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.alternate, node_id);
    try self.pushNode(data.consequent, node_id);
    try self.pushNode(data.condition, node_id);
}

fn visitPropertyAccess(
    self: *Self,
    data: *const ast.PropertyAccess,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.property, node_id);
    try self.pushNode(data.object, node_id);
}

fn visitClass(
    self: *Self,
    data: *const ast.Class,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.body, node_id);
    try self.pushNode(data.meta, node_id);
}

fn visitJsxMemberExpression(
    self: *Self,
    data: *const ast.JsxMemberExpression,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.property, node_id);
    try self.pushNode(data.object, node_id);
}

fn visitJsxNamespacedName(
    self: *Self,
    data: *const ast.JsxNamespacedName,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.name, node_id);
    try self.pushNode(data.namespace, node_id);
}

fn visitImportNamespaceSpecifier(
    self: *Self,
    data: *const ast.ImportNamespaceSpecifier,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.name, node_id);
}

fn visitSwitchCase(
    self: *Self,
    data: *const ast.SwitchCase,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.consequent, node_id);
    try self.pushNode(data.expression, node_id);
}

fn visitMetaProperty(
    self: *Self,
    data: *const ast.MetaProperty,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.property, node_id);
    try self.pushNode(data.meta, node_id);
}

fn visitVariableDeclarator(
    self: *Self,
    data: *const ast.VariableDeclarator,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.init) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.lhs, node_id);
}

fn visitJsxElement(
    self: *Self,
    data: *const ast.JsxElement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.closing_element, node_id);
    try self.pushNode(data.children, node_id);
    try self.pushNode(data.opening_element, node_id);
}

fn visitCatchClause(
    self: *Self,
    data: *const ast.CatchClause,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.param) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.body, node_id);
}

fn visitJsxClosingElement(
    self: *Self,
    data: *const ast.JsxClosingElement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.name, node_id);
}

fn visitClassFieldDefinition(
    self: *Self,
    data: *const ast.ClassFieldDefinition,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.value, node_id);
    try self.pushNode(data.key, node_id);
}

fn visitLabeledStatement(
    self: *Self,
    data: *const ast.LabeledStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.body, node_id);
    try self.pushNode(data.label, node_id);
}

fn visitTaggedTemplateExpression(
    self: *Self,
    data: *const ast.TaggedTemplateExpression,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.template, node_id);
    try self.pushNode(data.tag, node_id);
}

fn visitJsxFragment(
    self: *Self,
    data: *const ast.JsxFragment,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.children, node_id);
}

fn visitForInOfIterator(
    self: *Self,
    data: *const ast.ForInOfIterator,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.right, node_id);
    try self.pushNode(data.left, node_id);
}

fn visitImportDeclaration(
    self: *Self,
    data: *const ast.ImportDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.pushNode(data.source, node_id);
    try self.visitSubRange(&data.specifiers, node_id);
}

fn visitVariableDeclaration(
    self: *Self,
    data: *const ast.VariableDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.declarators, node_id);
}

fn visitExportAllDeclaration(
    self: *Self,
    data: *const ast.ExportAllDeclaration,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.name) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.source, node_id);
}

fn visitJsxAttribute(
    self: *Self,
    data: *const ast.JsxAttribute,
    node_id: Node.Index,
) Allocator.Error!void {
    if (data.value) |_pl|
        try self.pushNode(_pl, node_id);
    try self.pushNode(data.name, node_id);
}

fn visitSwitchStatement(
    self: *Self,
    data: *const ast.SwitchStatement,
    node_id: Node.Index,
) Allocator.Error!void {
    try self.visitSubRange(&data.cases, node_id);
    try self.pushNode(data.discriminant, node_id);
}
pub fn enqueueChildren(self: *Self, node_id: Node.Index) Allocator.Error!void {
    const node_pl = &self.node_pls[@intFromEnum(node_id)];
    switch (node_pl.*) {
        .program => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .assignment_expr => |pl| try self.visitBinaryPayload(&pl, node_id),
        .binary_expr => |pl| try self.visitBinaryPayload(&pl, node_id),
        .member_expr => |pl| try self.visitPropertyAccess(&pl, node_id),
        .computed_member_expr => |pl| try self.visitComputedPropertyAccess(&pl, node_id),
        .tagged_template_expr => |pl| try self.visitTaggedTemplateExpression(&pl, node_id),
        .meta_property => |pl| try self.visitMetaProperty(&pl, node_id),
        .arguments => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .new_expr => |pl| try self.visitNewExpr(&pl, node_id),
        .call_expr => |pl| try self.visitCallExpr(&pl, node_id),
        .super_call_expr => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .super => {}, // leaf (token)
        .optional_expr => |child_id| try self.pushNode(child_id, node_id),
        .function_expr => |pl| try self.visitFunction(&pl, node_id),
        .post_unary_expr => |pl| try self.visitUnaryPayload(&pl, node_id),
        .unary_expr => |pl| try self.visitUnaryPayload(&pl, node_id),
        .await_expr => |pl| try self.visitUnaryPayload(&pl, node_id),
        .yield_expr => {}, // leaf
        .update_expr => |pl| try self.visitUnaryPayload(&pl, node_id),
        .identifier => {}, // leaf (token)
        .identifier_reference => {}, // leaf (token)
        .binding_identifier => {}, // leaf (token)
        .string_literal => {}, // leaf (token)
        .number_literal => {}, // leaf
        .boolean_literal => {}, // leaf
        .null_literal => {}, // leaf (token)
        .regex_literal => {}, // leaf (token)
        .this => {}, // leaf (token)
        .empty_array_item => {}, // leaf
        .array_literal => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .array_pattern => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .spread_element => |child_id| try self.pushNode(child_id, node_id),
        .rest_element => |child_id| try self.pushNode(child_id, node_id),
        .object_literal => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .object_property => |pl| try self.visitPropertyDefinition(&pl, node_id),
        .shorthand_property => |pl| try self.visitShorthandProperty(&pl, node_id),
        .class_expression => |pl| try self.visitClass(&pl, node_id),
        .class_meta => |pl| try self.visitClassMeta(&pl, node_id),
        .class_field => |pl| try self.visitClassFieldDefinition(&pl, node_id),
        .class_method => |pl| try self.visitClassFieldDefinition(&pl, node_id),
        .sequence_expr => |pl| try self.visitSubRange(&pl, node_id),
        .parenthesized_expr => |child_id| try self.pushNode(child_id, node_id),
        .conditional_expr => |pl| try self.visitConditional(&pl, node_id),
        .template_literal => |pl| try self.visitSubRange(&pl, node_id),
        .template_element => {}, // leaf (token)
        .assignment_pattern => |pl| try self.visitBinaryPayload(&pl, node_id),
        .object_pattern => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .empty_statement => {}, // leaf
        .labeled_statement => |pl| try self.visitLabeledStatement(&pl, node_id),
        .try_statement => |pl| try self.visitTryStatement(&pl, node_id),
        .catch_clause => |pl| try self.visitCatchClause(&pl, node_id),
        .block_statement => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .statement_list => |pl| try self.visitSubRange(&pl, node_id),
        .expression_statement => |child_id| try self.pushNode(child_id, node_id),
        .variable_declaration => |pl| try self.visitVariableDeclaration(&pl, node_id),
        .variable_declarator => |pl| try self.visitVariableDeclarator(&pl, node_id),
        .function_declaration => |pl| try self.visitFunction(&pl, node_id),
        .function_meta => {}, // leaf
        .class_declaration => |pl| try self.visitClass(&pl, node_id),
        .debugger_statement => {}, // leaf
        .if_statement => |pl| try self.visitConditional(&pl, node_id),
        .do_while_statement => |pl| try self.visitWhileStatement(&pl, node_id),
        .while_statement => |pl| try self.visitWhileStatement(&pl, node_id),
        .with_statement => |pl| try self.visitWithStatement(&pl, node_id),
        .throw_statement => |child_id| try self.pushNode(child_id, node_id),
        .for_statement => |pl| try self.visitForStatement(&pl, node_id),
        .for_of_statement => |pl| try self.visitForStatement(&pl, node_id),
        .for_in_statement => |pl| try self.visitForStatement(&pl, node_id),
        .for_iterator => |pl| try self.visitForIterator(&pl, node_id),
        .for_in_of_iterator => |pl| try self.visitForInOfIterator(&pl, node_id),
        .switch_statement => |pl| try self.visitSwitchStatement(&pl, node_id),
        .switch_case => |pl| try self.visitSwitchCase(&pl, node_id),
        .default_case => |pl| try self.visitSwitchDefaultCase(&pl, node_id),
        .break_statement => {}, // leaf
        .continue_statement => {}, // leaf
        .parameters => |maybe_pl| if (maybe_pl) |pl| try self.visitSubRange(&pl, node_id),
        .return_statement => |maybe_child_id| if (maybe_child_id) |child_id| try self.pushNode(child_id, node_id),
        .import_declaration => |pl| try self.visitImportDeclaration(&pl, node_id),
        .import_default_specifier => |pl| try self.visitImportDefaultSpecifier(&pl, node_id),
        .import_specifier => |pl| try self.visitImportSpecifier(&pl, node_id),
        .import_namespace_specifier => |pl| try self.visitImportNamespaceSpecifier(&pl, node_id),
        .export_declaration => |pl| try self.visitExportedDeclaration(&pl, node_id),
        .export_specifier => |pl| try self.visitExportSpecifier(&pl, node_id),
        .export_list_declaration => |pl| try self.visitExportListDeclaration(&pl, node_id),
        .export_from_declaration => |pl| try self.visitExportFromDeclaration(&pl, node_id),
        .export_all_declaration => |pl| try self.visitExportAllDeclaration(&pl, node_id),
        .jsx_fragment => |pl| try self.visitJsxFragment(&pl, node_id),
        .jsx_element => |pl| try self.visitJsxElement(&pl, node_id),
        .jsx_children => |pl| try self.visitSubRange(&pl, node_id),
        .jsx_opening_element => |pl| try self.visitJsxOpeningElement(&pl, node_id),
        .jsx_closing_element => |pl| try self.visitJsxClosingElement(&pl, node_id),
        .jsx_self_closing_element => |pl| try self.visitJsxOpeningElement(&pl, node_id),
        .jsx_attribute => |pl| try self.visitJsxAttribute(&pl, node_id),
        .jsx_text => {}, // leaf (token)
        .jsx_expression => |child_id| try self.pushNode(child_id, node_id),
        .jsx_identifier => {}, // leaf (token)
        .jsx_identifier_reference => {}, // leaf (token)
        .jsx_member_expression => |pl| try self.visitJsxMemberExpression(&pl, node_id),
        .jsx_namespaced_name => |pl| try self.visitJsxNamespacedName(&pl, node_id),
        .jsx_spread_child => |child_id| try self.pushNode(child_id, node_id),
        .jsx_spread_attribute => |child_id| try self.pushNode(child_id, node_id),
        .none => {}, // leaf
    }
}
