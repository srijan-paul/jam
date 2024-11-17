const std = @import("std");
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig");

pub const BinaryPayload = struct {
    lhs: Node.Index,
    rhs: Node.Index,
    operator: Token.Index,
};

pub const UnaryPayload = struct {
    operand: Node.Index,
    operator: Token.Index,
};

// TODO: make `computed` a flag in the `PropertyAccess` struct
pub const PropertyAccess = struct {
    object: Node.Index,
    property: Token.Index,
};

pub const ComputedPropertyAccess = struct {
    object: Node.Index,
    property: Node.Index,
};

pub const SubRange = struct {
    pub const Index = enum(u32) { _ };
    from: Index,
    to: Index,

    /// Return a slice of `Node.Index`es that represents the sub-range.
    pub fn asSlice(self: *const SubRange, parser: *const Parser) []const Node.Index {
        const from: usize = @intFromEnum(self.from);
        const to: usize = @intFromEnum(self.to);
        return parser.node_lists.items[from..to];
    }
};

pub const YieldPayload = struct {
    value: ?Node.Index,
    is_delegated: bool,
};

pub const CallExpr = struct {
    callee: Node.Index,
    // TODO: store the arguments inline
    arguments: Node.Index,
};

pub const FunctionFlags = packed struct(u8) {
    is_generator: bool = false,
    is_async: bool = false,
    is_arrow: bool = false,
    _: u5 = 0,
};

/// Represents the payload for a function expression or declaration.
pub const Function = struct {
    /// points to a `ast.NodeData.parameters` (?SubRange)
    parameters: Node.Index,
    /// points to an expression or a block statement
    body: Node.Index,
    /// Function name and flags.
    info: ExtraData.Index,
    /// Get the name of this function, if it has one,
    /// directly from the source buffer.
    pub fn getName(self: *const Function, parser: *const Parser) ?[]const u8 {
        const maybe_name_token = parser.getExtraData(self.info).function.name;
        if (maybe_name_token) |name_token| {
            const token = parser.getToken(name_token);
            return token.toByteSlice(parser.source);
        }
        return null;
    }

    /// Returns a slice containing all the parameter nodes in the function.
    pub fn getParameterSlice(
        self: *const Function,
        parser: *const Parser,
    ) []const Node.Index {
        const params_node = parser.getNode(self.parameters);
        const maybe_params_range = params_node.data.parameters;
        if (maybe_params_range) |params_range| {
            return parser.getSubRange(params_range.from, params_range.to);
        }
        return &[_]Node.Index{};
    }

    /// Returns the number of parameters in the function.
    pub fn getParameterCount(
        self: *const Function,
        parser: *const Parser,
    ) usize {
        const params_node = parser.getNode(self.parameters);
        const maybe_params_range = params_node.data.parameters;
        if (maybe_params_range) |params_range| {
            const to: usize = @intFromEnum(params_range.to);
            const from: usize = @intFromEnum(params_range.from);
            return to - from;
        }
        return 0;
    }
};

pub const ForStatement = struct {
    iterator: ExtraData.Index,
    body: Node.Index,
};

/// Describes the kind of property in an object literal.
/// Differentiates getters and setters from regular property definitions.
pub const PropertyDefinitionKind = enum(u5) {
    /// Getter
    get,
    /// Setter
    set,
    /// Regular property or method
    init,
};

pub const ClassFieldKind = enum(u5) {
    /// Regular property or method
    init,
    /// Getter
    get,
    /// Setter
    set,
    /// The class constructor
    constructor,
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

pub const ClassFieldFlags = packed struct(u8) {
    is_static: bool = false,
    is_computed: bool = false,
    kind: ClassFieldKind = .init,
    _: bool = false, // padding
};

pub const ClassFieldDefinition = struct {
    key: Node.Index,
    value: Node.Index,
    flags: ClassFieldFlags = .{},
};

/// Payloads for ternary expressions and if statements.
pub const Conditional = struct {
    condition: Node.Index,
    consequent: Node.Index,
    /// Index to the "else" branch,
    /// This can be 0 (i.e point to a `.none` AST node)
    /// for an if-statement without an "else" branch.
    alternate: Node.Index,
};

pub const WhileStatement = struct {
    condition: Node.Index,
    body: Node.Index,
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
    declarators: SubRange,
    kind: VarDeclKind,
};

pub const CatchClause = struct {
    /// points to a 'block_statement'
    body: Node.Index,
    /// points to an identifier or a pattern.
    param: ?Node.Index,
};

pub const TryStatement = struct {
    /// Points to a 'block_statement'
    body: Node.Index,
    /// Points to a 'catch_clause'
    catch_clause: Node.Index,
    /// Points to a 'block_statement'
    finalizer: Node.Index,
};

/// A single 'case' block in a switch statement.
pub const SwitchCase = struct {
    /// The `<test>` in `case <test>:`
    expression: Node.Index,
    /// A list of statements in the case block.
    consequent: SubRange,
};

pub const SwitchDefaultCase = struct {
    /// A list of statements in the default case block.
    consequent: SubRange,
};

pub const SwitchStatement = struct {
    /// The `expr` in `switch (expr) { ... }`
    discriminant: Node.Index,
    /// A list of `switch_case` nodes with at-most one `default_case`.
    cases: SubRange,
};

pub const WithStatement = struct {
    object: Node.Index,
    body: Node.Index,
};

pub const LabeledStatement = struct {
    label: Token.Index,
    body: Node.Index,
};

pub const Class = struct {
    class_information: ExtraData.Index,
    body: SubRange,

    /// Return the name of the class.
    pub fn className(self: *const Class, p: *const Parser) ?[]const u8 {
        const maybe_name_node = p.getExtraData(self.class_information).class.name;
        if (maybe_name_node) |name_node| {
            const node = p.getNode(name_node);
            return p.source[node.start..node.end];
        }
        return null;
    }
};

pub const TaggedTemplateExpression = struct {
    tag: Node.Index,
    /// Points to a template_literal
    template: Node.Index,
};

pub const MetaProperty = struct {
    meta: Node.Index,
    property: Node.Index,
};

pub const ImportDeclaration = struct {
    specifiers: SubRange,
    source: Node.Index,
};

pub const ImportDefaultSpecifier = struct { name: Node.Index };
pub const ImportNamespaceSpecifier = struct { name: Node.Index };

pub const ImportSpecifier = struct {
    imported: ?Node.Index,
    local: Node.Index,
};

pub const NodeData = union(enum(u8)) {
    program: ?SubRange,

    // Expressions:
    assignment_expr: BinaryPayload,
    binary_expr: BinaryPayload,
    member_expr: PropertyAccess,
    computed_member_expr: ComputedPropertyAccess,
    tagged_template_expr: TaggedTemplateExpression,
    meta_property: MetaProperty,
    arguments: ?SubRange,
    new_expr: CallExpr,
    call_expr: CallExpr,
    super_call_expr: ?SubRange,
    super: Token.Index,
    // points to  call_expr, member_expr, or computed_member_expr
    optional_expr: Node.Index,
    function_expr: Function,

    post_unary_expr: UnaryPayload,
    unary_expr: UnaryPayload,
    await_expr: UnaryPayload,
    yield_expr: YieldPayload,
    update_expr: UnaryPayload,

    identifier: Token.Index,
    literal: Token.Index,
    this: Token.Index,
    empty_array_item: void,
    array_literal: ?SubRange,
    array_pattern: ?SubRange,
    /// A SpreadElement is used in expressions to splat arrays/objects.
    /// `const arr = [1, 2, ...[3, 4]];`
    spread_element: Node.Index,
    /// A RestElement is used in patterns to represent destructured bindings like:
    /// `const [a, ...rest] = [1, 2, 3];`
    rest_element: Node.Index,
    object_literal: ?SubRange,
    object_property: PropertyDefinition,
    class_expression: Class,
    class_field: ClassFieldDefinition,
    class_method: ClassFieldDefinition,
    sequence_expr: SubRange,
    conditional_expr: Conditional,
    /// Template literals are split into their subelements.
    /// So `Hello ${"world"}` becomes: `template_element, string_literal, template_element`
    template_literal: SubRange,
    /// TODO: store raw and cooked values?
    template_element: Token.Index,
    assignment_pattern: BinaryPayload,
    object_pattern: ?SubRange,

    // Statements:
    empty_statement,
    labeled_statement: LabeledStatement,
    try_statement: TryStatement,
    catch_clause: CatchClause,
    block_statement: ?SubRange,
    expression_statement: Node.Index,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    function_declaration: Function,
    class_declaration: Class,
    debugger_statement: void,
    if_statement: Conditional,
    do_while_statement: WhileStatement,
    while_statement: WhileStatement,
    with_statement: WithStatement,
    throw_statement: Node.Index,
    for_statement: ForStatement,
    for_of_statement: ForStatement,
    for_in_statement: ForStatement,
    switch_statement: SwitchStatement,
    /// A single 'case' block in a switch statement.
    switch_case: SwitchCase,
    /// The default case in a switch statement.
    default_case: SwitchDefaultCase,
    /// 'break' ';'
    break_statement: struct { label: ?Node.Index },
    /// 'continue' ';'
    continue_statement: struct { label: ?Node.Index },
    parameters: ?SubRange,
    return_statement: ?Node.Index,
    import_declaration: ImportDeclaration,
    import_default_specifier: ImportDefaultSpecifier,
    import_specifier: ImportSpecifier,
    import_namespace_specifier: ImportNamespaceSpecifier,

    /// Represents `null` AST node.
    /// This is a sentinel, and always present at index 0 of the `nodes` array.
    /// Used to represent nodes like a missing `else` branch (instead of ?Node.Index)
    /// which would take up more space, and increase the size of this union.
    none,

    comptime {
        std.debug.assert(@bitSizeOf(NodeData) <= 128);
    }
};

/// Iterator for a regular old for-loop
/// (i.e `for (let i = 0; i < 10; i++) { ... }`).
/// init: `let i = 0`;
/// condition: `i < 10`;
/// update: `i++`
pub const ForIterator = struct {
    init: Node.Index,
    condition: Node.Index,
    update: Node.Index,
};

pub const ForInOfIterator = struct {
    /// Left side of the 'of' keyword
    left: Node.Index,
    /// Right side of the 'of' keyword
    right: Node.Index,
    /// Set for for-await loops.
    is_await: bool = false,
};

pub const ClassInfo = struct {
    name: ?Node.Index,
    super_class: Node.Index,
};

/// Extra metadata about a node.
/// The specific type of meta-data is determined by the node's
/// tag (i.e the active field of NodeData).
pub const ExtraData = union {
    pub const Index = enum(u32) { none = 0, _ };
    function: struct {
        /// Name of the function, if present (always an identifier token).
        name: ?Token.Index,
        /// Flags: generator, async, arrow, etc.
        flags: FunctionFlags,
    },
    for_iterator: ForIterator,
    for_in_of_iterator: ForInOfIterator,
    class: ClassInfo,
};

const Type = std.builtin.Type;

pub const Node = struct {
    /// An index into the AST's `nodes` array list.
    pub const Index = enum(u32) { empty = 0, _ };
    /// The actual data stored by this node.
    data: NodeData,
    /// Byte offset into the source file where this node begins.
    start: u32,
    /// Byte offset into the source file where this node ends.
    end: u32,

    comptime {
        std.debug.assert(@bitSizeOf(Node) <= 196);
    }
};

pub const PrettyForInOfStatement = struct {
    lhs: *NodePretty,
    rhs: *NodePretty,
    body: *NodePretty,
};

/// Used for pretty printing and debugging.
pub const NodePretty = union(enum) {
    const BinaryPayload_ = Pretty(BinaryPayload);
    const PropertyAccess_ = Pretty(PropertyAccess);
    const ComputedPropertyAccess_ = Pretty(ComputedPropertyAccess);
    const UnaryPayload_ = Pretty(UnaryPayload);
    const Token_ = Pretty(Token.Index);

    program: Pretty(SubRange),

    assignment_expression: BinaryPayload_,
    binary_expression: BinaryPayload_,
    member_expression: PropertyAccess_,
    computed_member_expression: ComputedPropertyAccess_,
    optional_expression: Pretty(Node.Index),
    tagged_template_expression: Pretty(TaggedTemplateExpression),
    meta_property: Pretty(MetaProperty),
    arguments: Pretty(SubRange),
    new_expression: Pretty(CallExpr),
    call_expression: Pretty(CallExpr),
    super_call_expression: Pretty(SubRange),
    super,
    spread_element: Pretty(Node.Index),
    rest_element: Pretty(Node.Index),

    post_unary_expression: UnaryPayload_,
    unary_expression: UnaryPayload_,
    await_expression: UnaryPayload_,
    yield_expression: Pretty(YieldPayload),
    update_expression: UnaryPayload_,

    identifier: Token_,
    literal: Token_,
    this,
    empty_array_item,
    array: Pretty(SubRange),
    array_pattern: Pretty(SubRange),
    object_pattern: Pretty(SubRange),
    assignment_pattern: BinaryPayload_,
    object_literal: Pretty(SubRange),
    object_property: Pretty(PropertyDefinition),
    class_expression: struct {
        name: ?[]const u8,
        body: Pretty(SubRange),
    },
    class_declaration: struct {
        name: []const u8,
        body: Pretty(SubRange),
    },
    class_field: Pretty(ClassFieldDefinition),
    class_method: Pretty(ClassFieldDefinition),
    sequence_expression: Pretty(SubRange),
    conditional_expression: Pretty(Conditional),

    catch_clause: struct {
        param: ?Pretty(Node.Index),
        body: Pretty(Node.Index),
    },
    try_statement: struct {
        body: Pretty(Node.Index),
        catch_clause: Pretty(Node.Index),
        finalizer: Pretty(Node.Index),
    },
    with_statement: struct {
        object: Pretty(Node.Index),
        body: Pretty(Node.Index),
    },
    throw_statement: Pretty(Node.Index),
    function: struct {
        parameters: Pretty(Node.Index),
        body: Pretty(Node.Index),
        info: Pretty(ExtraData.Index),
    },

    // statements
    empty_statement,
    debugger_statement,
    labeled_statement: Pretty(LabeledStatement),
    expression_statement: Pretty(Node.Index),
    block_statement: Pretty(SubRange),
    if_statement: Pretty(Conditional),
    for_statement: struct {
        init: ?*NodePretty,
        condition: ?*NodePretty,
        update: ?*NodePretty,
        body: Pretty(Node.Index),
    },
    for_of_statement: PrettyForInOfStatement,
    for_in_statement: PrettyForInOfStatement,
    do_while_statement: Pretty(WhileStatement),
    while_statement: Pretty(WhileStatement),
    switch_statement: struct {
        discriminant: *NodePretty,
        cases: Pretty(SubRange),
    },
    switch_case: struct {
        expression: *NodePretty,
        consequent: Pretty(SubRange),
    },
    default_case: struct {
        consequent: Pretty(SubRange),
    },
    break_statement: struct { label: Pretty(?Node.Index) },
    continue_statement: struct { label: Pretty(?Node.Index) },
    variable_declaration: Pretty(VariableDeclaration),
    variable_declarator: Pretty(VariableDeclarator),
    import_declaration: Pretty(ImportDeclaration),
    import_default_specifier: Pretty(ImportDefaultSpecifier),
    import_specifier: Pretty(ImportSpecifier),
    import_namespace_specifier: Pretty(ImportNamespaceSpecifier),

    return_statement: Pretty(?Node.Index),
    template_element: Token_,
    template_literal: Pretty(SubRange),

    none: void,

    // helpers
    parameters: Pretty(SubRange),
};

pub const ExtraPretty = union(enum) {
    function: Pretty(std.meta.FieldType(ExtraData, .function)),
    for_iterator: Pretty(ForIterator),
    class: Pretty(ClassInfo),
};

pub fn Pretty(T: type) type {
    if (T == Node.Index) return *NodePretty;
    if (T == ?Node.Index) return ?*NodePretty;
    if (T == Token.Index) return []const u8;
    if (T == ?Token.Index) return ?[]const u8;
    if (T == SubRange) return []NodePretty;
    if (T == ?SubRange) return []NodePretty;
    if (T == ExtraData.Index) return ExtraPretty;

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
