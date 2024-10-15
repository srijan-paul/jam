const std = @import("std");
const Token = @import("token.zig").Token;

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

pub const Node = union(enum) {
    pub const Index = enum(u32) { _ };
    // arranged in order of precedence.
    assignment_expr: BinaryPayload,
    binary_expr: BinaryPayload,
    member_expr: PropertyAccess,
    computed_member_expr: ComputedPropertyAccess,

    post_unary_expr: UnaryPayload,
    unary_expr: UnaryPayload,
    update_expr: UnaryPayload,

    identifier: Token.Index,
    literal: Token.Index,
    this: Token.Index,
};

const BinaryPayloadPretty = struct {
    lhs: *NodePretty,
    rhs: *NodePretty,
    operator: []const u8,
};

const UnaryPayloadPretty = struct {
    operand: *NodePretty,
    operator: []const u8,
};

const ComputedPropertyPretty = struct {
    object: *NodePretty,
    property: *NodePretty,
};

const PropertyAccessPretty = struct {
    object: *NodePretty,
    property: []const u8,
};

pub const NodePretty = union(enum) {
    assignment_expr: BinaryPayloadPretty,
    binary_expr: BinaryPayloadPretty,

    unary_expr: UnaryPayloadPretty,
    post_unary_expr: UnaryPayloadPretty,

    member_expr: PropertyAccessPretty,
    computed_member_expr: ComputedPropertyPretty,

    literal: []const u8,
    identifier: []const u8,
    this: void,
};
