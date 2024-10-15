const std = @import("std");
const Token = @import("token.zig").Token;

pub const BinaryPayload = struct {
    lhs: Node.Index,
    rhs: Node.Index,
    op: Token.Index,
};

pub const Node = union(enum) {
    pub const Index = enum(u32) { _ };
    // arranged in order of precedence.
    assignment_expr: BinaryPayload,
    binary_expr: BinaryPayload,
    identifier: Token.Index,
    literal: Token.Index,
};

const BinaryPayloadPretty = struct {
    lhs: *NodePretty,
    rhs: *NodePretty,
    op: []const u8,
};

pub const NodePretty = union(enum) {
    binary_expr: BinaryPayloadPretty,
    assignment_expr: BinaryPayloadPretty,
    literal: []const u8,
    identifier: []const u8,
    _: []const u8,
};

fn PrettyType(T: anytype) type {
    if (T == Node.Index) {
        return NodePretty;
    } else if (T == Token.Index) {
        return []const u8;
    } else {
        const info = @typeInfo(@TypeOf(T));
        const fields = std.meta.fields(info);
        const new_fields: [fields.len]std.builtin.Type.StructField = undefined;
        for (0.., fields) |i, field| {
            new_fields[i] = field;
            new_fields[i].type = PrettyType(field.type);
        }
        const return_type_info = info;
        info.@"struct".fields = new_fields;
        return @Type(return_type_info);
    }
}
