const std = @import("std");

const assert = std.debug.assert;
const panic = std.debug.panic;
const print = std.debug.print;
const zig = std.zig;
const Allocator = std.mem.Allocator;
const AllocError = Allocator.Error;
const Ast = zig.Ast;

/// type decl nodes in the `ast.zig` file
const TypesInAst = struct {
    ast: zig.Ast,
    /// `pub const NodeData = union(enum) { ... };`
    node_data_tagged_union: Ast.full.ContainerDecl,
    /// `pub const Name = struct { ... };`
    struct_decls: std.StringHashMap(Ast.Node.Index),
};

fn collectTypeNodesFromAst(allocator: Allocator, ast: zig.Ast) !TypesInAst {
    const root_decls = ast.rootDecls();
    var node_data_tagged_union: ?Ast.full.ContainerDecl = null;
    var struct_decls = std.StringHashMap(Ast.Node.Index).init(allocator);

    for (root_decls) |decl| {
        const node = ast.nodes.get(decl);
        if (node.tag != .simple_var_decl) continue;
        // print("simple var decl: {s}\n", .{ast.getNodeSource(decl)});

        const var_decl = ast.simpleVarDecl(decl);
        if (var_decl.visib_token == null) continue;
        const name_token_index = var_decl.ast.mut_token + 1;
        const name_token = ast.tokens.get(name_token_index);
        assert(name_token.tag == .identifier);

        if (var_decl.ast.init_node == 0) continue;

        const name = ast.tokenSlice(name_token_index);
        if (std.mem.eql(u8, name, "Node")) continue;

        const init = ast.nodes.get(var_decl.ast.init_node);
        if (init.tag == .tagged_union_enum_tag) {
            assert(std.mem.eql(u8, name, "NodeData"));
            node_data_tagged_union = ast.taggedUnionEnumTag(var_decl.ast.init_node);
            continue;
        }

        switch (init.tag) {
            .container_decl,
            .container_decl_two,
            .container_decl_two_trailing,
            .container_decl_trailing,
            => {
                try struct_decls.put(name, var_decl.ast.init_node);
            },
            else => {},
        }
    }

    return TypesInAst{
        .ast = ast,
        .node_data_tagged_union = node_data_tagged_union orelse
            panic("NodeData not found", .{}),
        .struct_decls = struct_decls,
    };
}

fn generateStmtForFieldAccessAnn(
    allocator: Allocator,
    types: *const TypesInAst,
    w: std.fs.File,
    field_name: []const u8,
    ty_ann_node: Ast.Node,
    maybe_param_name: ?[]const u8,
) !void {
    const ast = types.ast;
    assert(ty_ann_node.tag == .field_access);
    const field_access_lhs = ast.nodes.get(ty_ann_node.data.lhs);
    const property_name = ast.tokenSlice(ty_ann_node.data.rhs);
    assert(std.mem.eql(u8, property_name, "Index"));
    assert(field_access_lhs.tag == .identifier);
    const type_name = ast.tokenSlice(field_access_lhs.main_token);

    // a `Node.Index` field.
    if (std.mem.eql(u8, type_name, "Node")) {
        const push_node_str = if (maybe_param_name) |param_name|
            try std.fmt.allocPrint(
                allocator,
                "    try self.pushNode({s}, node_id);\n",
                .{param_name},
            )
        else
            try std.fmt.allocPrint(
                allocator,
                "    try self.pushNode(data.{s}, node_id);\n",
                .{field_name},
            );

        _ = try w.write(push_node_str);
    }
}

fn isPayloadTypeLeaf(
    types: *const TypesInAst,
    type_node: Ast.full.ContainerDecl,
) bool {
    const ast = types.ast;
    for (type_node.ast.members) |member_id| {
        const member = ast.nodes.get(member_id);
        if (member.tag != .container_field_init) continue;
        const type_ann_node = ast.nodes.get(member.data.lhs);
        if (type_ann_node.tag == .identifier) {
            if (std.mem.eql(u8, ast.tokenSlice(type_ann_node.main_token), "SubRange")) {
                return false;
            }
            const type_ann_name = ast.tokenSlice(type_ann_node.main_token);
            if (types.struct_decls.get(type_ann_name)) |decl_id| {
                var buffer: [2]Ast.Node.Index = undefined;
                const decl = ast.fullContainerDecl(&buffer, decl_id).?;
                const init = ast.tokens.get(decl.ast.main_token);
                if (init.tag == .keyword_enum or !isPayloadTypeLeaf(types, decl))
                    return false;
            }
        } else if (type_ann_node.tag == .optional_type) {
            const actual_type = ast.nodes.get(type_ann_node.data.lhs);
            if (actual_type.tag == .identifier) {
                const type_name = ast.tokenSlice(actual_type.main_token);
                if (std.mem.eql(u8, type_name, "SubRange")) {
                    return false;
                }
                if (types.struct_decls.get(type_name)) |decl_id| {
                    var buffer: [2]Ast.Node.Index = undefined;
                    const decl = ast.fullContainerDecl(&buffer, decl_id).?;
                    const init = ast.tokens.get(decl.ast.main_token);
                    if (init.tag == .keyword_enum or !isPayloadTypeLeaf(types, decl)) {
                        return false;
                    }
                }
            }
        } else if (type_ann_node.tag == .field_access) {
            const field_lhs = ast.nodes.get(type_ann_node.data.lhs);
            const property_name = ast.tokenSlice(type_ann_node.data.rhs);
            assert(std.mem.eql(u8, property_name, "Index"));
            assert(field_lhs.tag == .identifier);
            const type_name = ast.tokenSlice(field_lhs.main_token);
            if (std.mem.eql(u8, type_name, "Node")) return false;
        } else {
            print("type_ann_node.tag: {s}\n", .{@tagName(type_ann_node.tag)});
            return false;
        }
    }

    return true;
}

fn generateStmtForAnn(
    allocator: Allocator,
    w: std.fs.File,
    types: *const TypesInAst,
    type_ann_node: Ast.Node,
    field_name: []const u8,
    maybe_param_name: ?[]const u8,
) !void {
    const ast = types.ast;

    if (type_ann_node.tag == .field_access) {
        try generateStmtForFieldAccessAnn(
            allocator,
            types,
            w,
            field_name,
            type_ann_node,
            maybe_param_name,
        );
    } else if (type_ann_node.tag == .optional_type) {
        const s = try std.fmt.allocPrint(
            allocator,
            "    if (data.{s}) |_pl|\n    ",
            .{field_name},
        );
        _ = try w.write(s);
        const actual_type = ast.nodes.get(type_ann_node.data.lhs);
        try generateStmtForAnn(
            allocator,
            w,
            types,
            actual_type,
            field_name,
            "_pl",
        );
    } else if (type_ann_node.tag == .identifier) {
        const type_name = ast.tokenSlice(type_ann_node.main_token);
        if (std.mem.eql(u8, type_name, "SubRange")) {
            const s = try std.fmt.allocPrint(
                allocator,
                "    try self.visitSubRange(&data.{s}, node_id);\n",
                .{field_name},
            );
            _ = try w.write(s);
        } else {
            if (std.mem.indexOf(u8, type_name, "Flags") != null)
                return;

            if (std.mem.eql(u8, type_name, "bool") or
                std.mem.eql(u8, type_name, "f64"))
            {
                return;
            }

            if (types.struct_decls.get(type_name)) |decl_id| {
                var buffer: [2]Ast.Node.Index = undefined;
                const container_decl = ast.fullContainerDecl(&buffer, decl_id).?;
                const container_type = ast.tokens.get(container_decl.ast.main_token);
                if (container_type.tag == .keyword_enum) {
                    return;
                }
                print("type_name: {s}\n", .{type_name});
            }

            panic(
                "This type annotation '{s}' on the field is not supported by the iterator generation script\n",
                .{type_name},
            );
        }
    }
}

fn generateVisitFn(
    allocator: Allocator,
    w: std.fs.File,
    types: *const TypesInAst,
    ty_name: []const u8,
    init_node_id: Ast.Node.Index,
) !void {
    const ast = types.ast;
    if (std.mem.eql(u8, ty_name, "NodeData") or
        std.mem.eql(u8, ty_name, "Tree") or
        std.mem.eql(u8, ty_name, "ExtraData") or
        std.mem.eql(u8, ty_name, "SubRange") or
        // Any `...Flags`
        std.mem.indexOf(u8, ty_name, "Flags") != null)
    {
        return;
    }

    var buffer: [2]Ast.Node.Index = undefined;
    const init_node = ast.fullContainerDecl(&buffer, init_node_id).?;
    if (isPayloadTypeLeaf(types, init_node)) return;

    const container_type = ast.tokens.get(init_node.ast.main_token);
    if (container_type.tag != .keyword_struct) {
        return;
    }

    const fn_header = try std.fmt.allocPrint(
        allocator,
        "\nfn visit{s}(self: *Self, data: *const ast.{s}, node_id: Node.Index,) Allocator.Error!void {{\n",
        .{ ty_name, ty_name },
    );
    _ = try w.write(fn_header);

    const members = try allocator.dupe(Ast.Node.Index, init_node.ast.members);
    std.mem.reverse(Ast.Node.Index, members); // for correct traversal order.
    for (members) |member_id| {
        const member_node = ast.nodes.get(member_id);
        if (member_node.tag != .container_field_init) {
            continue;
        }

        const field_name_token = member_node.main_token;
        const field_name = ast.tokenSlice(field_name_token);
        if (member_node.data.lhs == 0) continue;

        const type_ann_node = ast.nodes.get(member_node.data.lhs);
        try generateStmtForAnn(
            allocator,
            w,
            types,
            type_ann_node,
            field_name,
            null,
        );
    }

    _ = try w.write("\n}\n");
}

fn generateNodeIterator(
    allocator: Allocator,
    types: *const TypesInAst,
    out_file: std.fs.File,
) !void {
    const iterator_file_start =
        \\// !THIS IS A GENERATED FILE!
        \\// To regenerate, run `zig build astgen`.
        \\// To modify the generation process, see: tools/gen/main.zig
        \\const std = @import("std");
        \\const Allocator = std.mem.Allocator;
        \\
        \\const ast = @import("ast.zig");
        \\const Tree = ast.Tree;
        \\const NodeData = ast.NodeData;
        \\const Node = ast.Node;
        \\
        \\const Self = @This();
        \\
        \\/// A Node Id + the data it contains,
        \\/// returned by a node iterator.
        \\const Item = struct {
        \\    /// Node ID
        \\    node_id: Node.Index,
        \\    /// Parent of this node 
        \\    parent_id: ?Node.Index, 
        \\};
        \\
        \\allocator: Allocator,
        \\
        \\t: *const Tree,
        \\stack: std.ArrayListUnmanaged(Item),
        \\/// Slice of all node payloads from the AST
        \\node_pls: []const NodeData,
        \\
        \\pub fn init(
        \\    allocator: std.mem.Allocator,
        \\    tree: *const Tree,
        \\    start_node_id: Node.Index,
        \\) Allocator.Error!Self {
        \\    const node_pls = tree.nodes.items(.data);
        \\    var stack = try std.ArrayListUnmanaged(Item).initCapacity(allocator, 128);
        \\    stack.appendAssumeCapacity(. { 
        \\      .node_id = start_node_id, 
        \\      .parent_id = null,
        \\    });
        \\
        \\    return Self{
        \\        .t = tree,
        \\        .allocator = allocator,
        \\        .node_pls = node_pls,
        \\        .stack = stack,
        \\    };
        \\}
        \\
        \\pub fn deinit(self: *Self) void {
        \\    self.stack.deinit(self.allocator);
        \\}
        \\
        \\pub fn childIterator(allocator: Allocator, tree: *const Tree, start_node_id: Node.Index,) Allocator.Error!Self {
        \\    var iter = Self.init(allocator, tree, start_node_id);
        \\    try iter.enqueueChildren(start_node_id);
        \\    return iter;
        \\}
        \\
        \\/// Get the next node from the node iterator.
        \\pub fn next(self: *Self) ?Item {
        \\    return self.stack.pop();
        \\}
        \\
        \\pub fn pushNode(self: *Self, node_id: Node.Index, parent_id: ?Node.Index,) Allocator.Error!void {
        \\    try self.stack.append(self.allocator, .{
        \\        .node_id = node_id,
        \\        .parent_id = parent_id,
        \\    });
        \\}
        \\
        \\fn visitSubRange(self: *Self, range: *const ast.SubRange, parent_id: ?Node.Index,) Allocator.Error!void {
        \\    const from: usize = @intFromEnum(range.from);
        \\    const to: usize = @intFromEnum(range.to);
        \\
        \\    if (from == to) return;
        \\
        \\    var i = to;
        \\    while (i > from) : (i -= 1) {
        \\        const node_id = self.t.node_indices.items[i - 1];
        \\        try self.pushNode(node_id, parent_id);
        \\    }
        \\}
    ;
    const ast = types.ast;
    const NodeData = types.node_data_tagged_union;

    _ = try out_file.write(iterator_file_start);

    var iter = types.struct_decls.iterator();
    while (iter.next()) |kv| {
        const k = kv.key_ptr.*;
        const v = kv.value_ptr.*;
        try generateVisitFn(allocator, out_file, types, k, v);
    }

    _ = try out_file.write(
        \\pub fn enqueueChildren(self: *Self, node_id: Node.Index) Allocator.Error!void {
        \\    const node_pl = &self.node_pls[@intFromEnum(node_id)];
        \\    switch (node_pl.*) {
        \\
    );

    for (NodeData.ast.members) |mem| {
        const member = ast.nodes.get(mem);
        if (member.tag != .container_field_init) continue;
        const field_name_token = member.main_token;

        try generateCaseArm(
            allocator,
            types,
            field_name_token,
            member.data.lhs,
            false,
            out_file,
        );
    }

    _ = try out_file.write("}\n"); // close switch
    _ = try out_file.write("}"); // close fn

}

fn generateCaseArm(
    allocator: Allocator,
    types: *const TypesInAst,
    field_name_token: u32,
    type_ann_node_id: Ast.Node.Index,
    is_optional_type: bool,
    out_file: std.fs.File,
) !void {
    const ast = types.ast;
    const field_name = ast.tokenSlice(field_name_token);
    const type_ann_node = ast.nodes.get(type_ann_node_id);

    if (type_ann_node.tag == .optional_type) {
        if (is_optional_type) {
            panic("Nested optional types are not allowed in 'ast.zig'", .{});
        }

        const actual_ann_id = type_ann_node.data.lhs;
        return generateCaseArm(
            allocator,
            types,
            field_name_token,
            actual_ann_id,
            true,
            out_file,
        );
    } else if (type_ann_node.tag == .field_access) {
        const field_lhs = ast.nodes.get(type_ann_node.data.lhs);
        const property_name = ast.tokenSlice(type_ann_node.data.rhs);
        assert(std.mem.eql(u8, property_name, "Index"));
        assert(field_lhs.tag == .identifier);
        const type_name = ast.tokenSlice(field_lhs.main_token);
        if (std.mem.eql(u8, type_name, "Token")) {
            _ = try out_file.write(".");
            _ = try out_file.write(field_name);
            _ = try out_file.write(" => {}, // leaf (token)\n");
        } else if (std.mem.eql(u8, type_name, "Node")) {
            _ = try out_file.write(".");
            _ = try out_file.write(field_name);
            if (is_optional_type) {
                _ = try out_file.write(" => |maybe_child_id| if (maybe_child_id) |child_id| try self.pushNode(child_id, node_id),\n");
            } else {
                _ = try out_file.write(" => |child_id| try self.pushNode(child_id, node_id),\n");
            }
        } else {
            panic("'{s}.{s}' payload type in `ast.zig` is not supported by the Node iterator gen tool", .{
                type_name,
                field_name,
            });
        }
        return;
    }

    if (type_ann_node.tag != .identifier) {
        panic(
            "This type annotation of field '{s}' ({s}) is not supported by the iterator generation script\n",
            .{ field_name, @tagName(type_ann_node.tag) },
        );
    }

    const type_ann_token = type_ann_node.main_token;
    if (type_ann_token == field_name_token) {
        _ = try out_file.write(".");
        _ = try out_file.write(field_name);
        _ = try out_file.write(" => {}, // leaf\n");
        return;
    }

    const type_ann = ast.tokenSlice(type_ann_token);
    assert(types.struct_decls.contains(type_ann));
    const type_id = types.struct_decls.get(type_ann).?;
    var buffer: [2]Ast.Node.Index = undefined;
    const type_decl_node = ast.fullContainerDecl(&buffer, type_id).?;
    if (!std.mem.eql(u8, type_ann, "SubRange") and isPayloadTypeLeaf(types, type_decl_node)) {
        _ = try out_file.write(".");
        _ = try out_file.write(field_name);
        _ = try out_file.write(" => {}, // leaf\n");
        return;
    }

    const type_name = ast.tokenSlice(type_ann_token);
    const arm = try if (!is_optional_type) std.fmt.allocPrint(
        allocator,
        " => |pl| try self.visit{s}(&pl, node_id),\n",
        .{type_name},
    ) else std.fmt.allocPrint(
        allocator,
        " => |maybe_pl| if (maybe_pl) |pl| try self.visit{s}(&pl, node_id),\n",
        .{type_name},
    );

    _ = try out_file.write(".");
    _ = try out_file.write(field_name);
    _ = try out_file.write(arm);
}

/// Parse the jam ast.zig file and return the AST of the ast (yeah...)
fn parseAstZigSource(allocator: Allocator) !zig.Ast {
    const ast_file_path = "src/js/ast.zig";

    var dir = std.fs.cwd();
    const ast_source = try dir.readFileAlloc(allocator, ast_file_path, std.math.maxInt(usize));
    const ast_source_z = try allocator.dupeZ(u8, ast_source);

    return try zig.Ast.parse(allocator, ast_source_z, .zig);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa_allocator = gpa.allocator();
    defer assert(gpa.deinit() == .ok);

    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const ast = try parseAstZigSource(allocator);
    const ast_types = try collectTypeNodesFromAst(allocator, ast);

    const out_file_path = try std.fs.path.join(allocator, &.{ "src", "js", "iterator.zig" });
    const cwd = std.fs.cwd();

    const out_file =
        try cwd.createFile(out_file_path, .{ .truncate = true });
    try generateNodeIterator(allocator, &ast_types, out_file);
}
