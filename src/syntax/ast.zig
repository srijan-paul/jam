const std = @import("std");
const Token = @import("token.zig").Token;
const types = @import("util").types;

pub const BinaryPayload = struct {
    lhs: Node.Index,
    rhs: Node.Index,
    operator: Token.Index,
};

pub const UnaryPayload = struct {
    operand: Node.Index,
    operator: Token.Index,
};

pub const PropertyAccess = struct {
    object: Node.Index,
    property: Token.Index,
};

pub const ComputedPropertyAccess = struct {
    object: Node.Index,
    property: Node.Index,
};

pub const NodeList = struct {
    pub const Index = enum(u32) { _ };
    from: Index,
    to: Index,
};

pub const CallExpr = struct {
    callee: Node.Index,
    arguments: Node.Index,
};

pub const ObjectProperty = struct {
    key: Node.Index,
    value: Node.Index,
};

pub const ConditionalExpr = struct {
    condition: Node.Index,
    consequent: Node.Index,
    alternate: Node.Index,
};

pub const NodeData = union(enum) {
    assignment_expr: BinaryPayload,
    binary_expr: BinaryPayload,
    member_expr: PropertyAccess,
    computed_member_expr: ComputedPropertyAccess,
    arguments: ?NodeList,
    new_expr: CallExpr,
    call_expr: CallExpr,
    super_call_expr: ?NodeList,
    // points to  call_expr, member_expr, or computed_member_expr
    optional_expr: Node.Index,

    post_unary_expr: UnaryPayload,
    unary_expr: UnaryPayload,
    update_expr: UnaryPayload,

    identifier: Token.Index,
    literal: Token.Index,
    this: Token.Index,
    empty_array_item: void,
    array_literal: ?NodeList,
    array_pattern: ?NodeList,
    spread_element: Node.Index,

    object_literal: ?NodeList,
    object_property: ObjectProperty,
    sequence_expr: NodeList,
    conditional_expr: ConditionalExpr,

    assignment_pattern: BinaryPayload,

    comptime {
        std.debug.assert(@bitSizeOf(NodeData) <= 128);
    }
};

const Type = std.builtin.Type;

pub const Node = struct {
    /// An index into the AST's `nodes` array list.
    pub const Index = enum(u32) { _ };
    /// Byte offset into the source file where this node begins.
    start: u32,
    /// Byte offset into the source file where this node ends.
    end: u32,
    /// The actual data stored by this node.
    data: NodeData,
};

pub const NodePretty = union(enum) {
    const BinaryPayload_ = Pretty(BinaryPayload);
    const PropertyAccess_ = Pretty(PropertyAccess);
    const ComputedPropertyAccess_ = Pretty(ComputedPropertyAccess);
    const UnaryPayload_ = Pretty(UnaryPayload);
    const Token_ = Pretty(Token.Index);

    assignment_expression: BinaryPayload_,
    binary_expression: BinaryPayload_,
    member_expression: PropertyAccess_,
    computed_member_expression: ComputedPropertyAccess_,
    optional_expression: Pretty(Node.Index),
    arguments: Pretty(NodeList),
    new_expression: Pretty(CallExpr),
    call_expression: Pretty(CallExpr),
    super_call_expression: Pretty(NodeList),
    spread_element: Pretty(Node.Index),

    post_unary_expression: UnaryPayload_,
    unary_expression: UnaryPayload_,
    update_expression: UnaryPayload_,

    identifier: Token_,
    literal: Token_,
    this: void,
    empty_array_item: void,
    array: Pretty(NodeList),
    array_pattern: Pretty(NodeList),
    assignment_pattern: BinaryPayload_,
    object_literal: Pretty(NodeList),
    object_property: Pretty(ObjectProperty),
    sequence_expression: Pretty(NodeList),
    conditional_expression: Pretty(ConditionalExpr),
};

fn Pretty(T: type) type {
    if (T == Node.Index) return *NodePretty;
    if (T == NodeList) return []NodePretty;
    if (T == Token.Index) return []const u8;

    switch (@typeInfo(T)) {
        .@"struct" => |s| {
            const fields: []const Type.StructField = s.fields;
            var new_fields: [fields.len]Type.StructField = undefined;
            for (0.., fields) |i, field| {
                new_fields[i] = field;
                new_fields[i].type = Pretty(field.type);
            }
            var new_struct_info = s;
            new_struct_info.fields = &new_fields;
            return @Type(.{ .@"struct" = new_struct_info });
        },
        else => {
            return T;
        },
    }
}

// Dependency loop that shouldn't exist.
// The zig compiler caches types to tie loops.
// Why does this happen? I should submit a bug report to ziglang/zig.
// pub const NodePretty =
//     switch (@typeInfo(Node)) {
//     .@"union" => |struct_info| {
//         const fields: []const Type.UnionField = struct_info.fields;
//         var new_fields: [fields.len]Type.UnionField = undefined;
//         for (0.., fields) |i, field| {
//             new_fields[i] = field;
//             new_fields[i].type = ToPrettyPayload(field.type);
//         }

//         var new_union_info = struct_info;
//         new_union_info.fields = &new_fields;
//         @Type(.{ .@"union" = new_union_info });
//     },
//     else => unreachable,
// };
