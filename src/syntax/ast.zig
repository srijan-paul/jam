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

pub const NodeList = struct {
    pub const Index = enum(u32) { _ };
    from: Index,
    to: Index,
};

pub const CallExpr = struct {
    callee: Node.Index,
    arguments: Node.Index,
};

pub const FunctionFlags = packed struct(u8) {
    is_generator: bool = false,
    is_async: bool = false,
    is_arrow: bool = false,
    _: u5 = 0,
};

pub const FunctionExpression = struct {
    parameters: Node.Index, // points to a `parameters`
    body: Node.Index, // points to a an expression, or a block statement.
    flags: FunctionFlags = .{},
};

pub const PropertyDefinitionKind = enum(u5) {
    get,
    set,
    init,
};

/// Flags for property definitions of an object literal.
pub const PropertyDefinitionFlags = packed struct(u8) {
    is_method: bool = false,
    is_shorthand: bool = false,
    is_computed: bool = false,
    kind: PropertyDefinitionKind = .init,
};

pub const PropertyDefinition = struct {
    key: Node.Index,
    value: Node.Index,
    flags: PropertyDefinitionFlags = .{},
};

/// Payloads for ternary expressions and if statements.
pub const Conditional = struct {
    condition: Node.Index,
    consequent: Node.Index,
    /// Index to the "else" branch,
    /// This can be 0 (i.e pointing to a `.none`)
    /// for an if-statement without an "else" branch.
    alternate: Node.Index,
};

pub const VariableDeclarator = struct {
    lhs: Node.Index,
    init: ?Node.Index,
};

pub const VarDeclKind = enum {
    let,
    @"var",
    @"const",
};

pub const VariableDeclaration = struct {
    declarators: NodeList,
    kind: VarDeclKind,
};

pub const NodeData = union(enum(u8)) {
    program: ?NodeList,

    // Expressions:
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
    function_expr: FunctionExpression,

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
    object_property: PropertyDefinition,
    sequence_expr: NodeList,
    conditional_expr: Conditional,

    assignment_pattern: BinaryPayload,
    object_pattern: ?NodeList,

    // Statements:
    empty_statement: void,
    block_statement: ?NodeList,
    expression_statement: Node.Index,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    debugger_statement: void,
    if_statement: Conditional,
    parameters: ?NodeList,

    /// Represents `null` AST node.
    /// This is a sentinel, and always present at index 0 of the `nodes` array.
    /// Used to represent nodes like a missing `else` branch (instead of ?Node.Index)
    /// which would take up more space, and increase the size of this union.
    none: void,

    comptime {
        std.debug.assert(@bitSizeOf(NodeData) <= 128);
    }
};

const Type = std.builtin.Type;

pub const Node = struct {
    /// An index into the AST's `nodes` array list.
    pub const Index = enum(u32) { empty = 0, _ };
    /// Byte offset into the source file where this node begins.
    start: u32,
    /// Byte offset into the source file where this node ends.
    end: u32,
    /// The actual data stored by this node.
    data: NodeData,

    comptime {
        std.debug.assert(@bitSizeOf(Node) <= 196);
    }
};

pub const NodePretty = union(enum) {
    const BinaryPayload_ = Pretty(BinaryPayload);
    const PropertyAccess_ = Pretty(PropertyAccess);
    const ComputedPropertyAccess_ = Pretty(ComputedPropertyAccess);
    const UnaryPayload_ = Pretty(UnaryPayload);
    const Token_ = Pretty(Token.Index);

    program: Pretty(NodeList),

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
    object_pattern: Pretty(NodeList),
    assignment_pattern: BinaryPayload_,
    object_literal: Pretty(NodeList),
    object_property: Pretty(PropertyDefinition),
    sequence_expression: Pretty(NodeList),
    conditional_expression: Pretty(Conditional),
    function_expression: Pretty(FunctionExpression),

    // statements
    empty_statement: void,
    debugger_statement: void,
    expression_statement: Pretty(Node.Index),
    block_statement: Pretty(NodeList),
    if_statement: Pretty(Conditional),
    variable_declaration: Pretty(VariableDeclaration),
    variable_declarator: Pretty(VariableDeclarator),

    // declarations
    none: void,

    // helpers
    parameters: Pretty(NodeList),
};

fn Pretty(T: type) type {
    if (T == Node.Index) return *NodePretty;
    if (T == ?Node.Index) return ?*NodePretty;
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
