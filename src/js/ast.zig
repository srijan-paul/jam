const std = @import("std");
pub const Token = @import("token.zig").Token;
const Parser = @import("parser.zig");
const util = @import("util");

const String = util.StringPool.String;

/// Represents a parse tree.
/// Nodes are stored in a flat array, and reference each other using indices.
pub const Tree = struct {
    allocator: std.mem.Allocator,
    /// index of the "program" node in the `nodes` array
    root: Node.Index,
    /// Source code represented by this tree
    source: []const u8,
    source_type: Parser.SourceType,
    /// A flat list that stores all nodes in the AST.
    nodes: std.MultiArrayList(Node),
    tokens: std.ArrayList(Token),
    /// Extra information for some nodes.
    /// referenced using indicies (see: `ExtraData.Index`)
    extras: std.ArrayList(ExtraData),
    /// An array containing indices of AST nodes.
    /// Related nodes are stored next to each other.
    /// For example, indices of all arguments in a call expression are stored in a row.
    /// Then, the `CallExpression` node can use an `ast.SubRange` to reference a slice of nodes
    /// from within this array.
    node_indices: std.ArrayList(Node.Index),
    /// String intern table for fast string comparison
    string_pool: util.StringPool,

    /// Obtain all the data for a single AST Node.
    pub fn getNode(self: *const Tree, index: Node.Index) Node {
        return self.nodes.get(@intFromEnum(index));
    }

    pub fn nodeData(self: *const Tree, index: Node.Index) *const NodeData {
        return &self.nodes.items(.data)[@intFromEnum(index)];
    }

    // TODO: add a getTokenPtr
    /// Get a token by its index (Token.Index)
    pub fn getToken(self: *const Tree, index: Token.Index) *const Token {
        return &self.tokens.items[@intFromEnum(index)];
    }

    /// Get the extra data of a node from its ExtraData.Index
    pub fn getExtraData(self: *const Tree, index: ExtraData.Index) ExtraData {
        return self.extras.items[@intFromEnum(index)];
    }

    /// Get a slice of nodes from an ast.SubRange.
    pub fn getSubRange(
        self: *const Tree,
        from_: SubRange.Index,
        to_: SubRange.Index,
    ) []const Node.Index {
        const from: usize = @intFromEnum(from_);
        const to: usize = @intFromEnum(to_);
        return self.node_indices.items[from..to];
    }

    pub fn tag(self: *const Tree, node_id: Node.Index) std.meta.Tag(NodeData) {
        return std.meta.activeTag(
            self.nodes.items(.data)[@intFromEnum(node_id)],
        );
    }

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit(self.allocator);
        self.tokens.deinit();
        self.extras.deinit();
        self.string_pool.deinit();
        self.node_indices.deinit();
    }
};

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
    property: Node.Index,
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
    pub fn asSlice(self: *const SubRange, tree: *const Tree) []const Node.Index {
        const from: usize = @intFromEnum(self.from);
        const to: usize = @intFromEnum(self.to);
        return tree.node_indices.items[from..to];
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

pub const NewExpr = struct {
    callee: Node.Index,
    arguments: ?Node.Index,
};

pub const JumpLabel = struct {
    label: ?Node.Index,
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
    pub fn getName(self: *const Function, tree: *const Tree) ?[]const u8 {
        const name_id = tree.getExtraData(self.info).function.name orelse
            return null;
        const name = tree.nodes.items(.data)[@intFromEnum(name_id)];
        return tree.getToken(name.binding_identifier).toByteSlice(tree.source);
    }

    /// Get the token ID for the name of this function
    pub fn getNameTokenId(self: *const Function, tree: *const Tree) ?Token.Index {
        if (tree.getExtraData(self.info).function.name) |id_node| {
            return tree.getNode(id_node).data.binding_identifier;
        }

        return null;
    }

    /// Get the token for the name of this function
    pub fn getNameToken(self: *const Function, tree: *const Tree) ?*const Token {
        if (self.getNameTokenId(tree)) |id|
            return tree.getToken(id);
        return null;
    }

    /// Returns a slice containing all the parameter nodes in the function.
    pub fn getParameterSlice(self: *const Function, tree: *const Tree) []const Node.Index {
        const params_node = tree.getNode(self.parameters);
        const maybe_params_range = params_node.data.parameters;
        if (maybe_params_range) |params_range| {
            return tree.getSubRange(params_range.from, params_range.to);
        }
        return &[_]Node.Index{};
    }

    /// Returns the number of parameters in the function.
    pub fn getParameterCount(self: *const Function, tree: *const Tree) usize {
        const params_node = tree.getNode(self.parameters);
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
pub const PropertyDefinitionKind = enum(u6) {
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
    is_computed: bool = false,
    kind: PropertyDefinitionKind = .init,
};

pub const PropertyDefinition = struct {
    key: Node.Index,
    value: Node.Index,
    flags: PropertyDefinitionFlags = .{},
};

/// A shorthand property definition in an object literal.
/// e.g: `{ x }`
pub const ShorthandProperty = struct { name: Node.Index };

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
    label: Node.Index,
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
            const start_token = p.getToken(node.start);
            return start_token.toByteSlice(p.source);
        }
        return null;
    }
};

pub const TaggedTemplateExpression = struct {
    tag: Node.Index,
    /// Points to a template_literal
    template: Node.Index,
};

/// A meta property like `new.target` or `import.meta`
pub const MetaProperty = struct {
    meta: Node.Index,
    property: Node.Index,
};

/// An import declaration with zero or more specifiers.
pub const ImportDeclaration = struct {
    /// Represents 'X', a', 'b', 'ns', and 'c' in:
    /// ```js
    /// import Foo, { a as a_alias, b, c } from "module"
    /// import * as ns from "module"
    /// ```
    /// A specifier can be:
    /// - `ImportDefaultSpecifier` (Foo)
    /// - `ImportSpecifier` (`a as a_alias`, b, c)
    /// - `ImportNamespaceSpecifier` (`* as ns`)
    specifiers: SubRange,
    /// String literal. Module path that was imported.
    source: Node.Index,
};

/// A default import.
/// `name` is always an identifier
pub const ImportDefaultSpecifier = struct { name: Node.Index };
/// A namespace import: `import * as ns from "module"`
pub const ImportNamespaceSpecifier = struct { name: Node.Index };

/// An import specifier with an optional alias.
/// `import x from "module"` -> `x` is the specifier
/// `import { a as b } from "module"` -> `a as b` is the specifier
pub const ImportSpecifier = struct {
    /// The name used when referencing the item in the module its imported from
    /// If `null`, then its the same as `local`.
    imported: ?Node.Index,
    /// The alias used when referencing the item in the current module.
    /// If `imported` is null, then this is just the name of the imported item.
    local: Node.Index,
};

/// An exported identifier.
/// `export { foo as bar }` -> `foo as bar` is the specifier
pub const ExportSpecifier = struct {
    /// Name of the top-level declaration that is being exported
    local: Node.Index,
    /// Alias used when importing the exported item.
    /// If no alias is present (i.e `import {foo}` instead of import `{ foo as bar }`),
    /// this is null.
    exported: ?Node.Index,
};

/// Represents all exports of the following manner:
/// ```js
/// export function f() { }
/// export let x = 10;
/// export default function f() { }
/// export default 1
/// ```
pub const ExportedDeclaration = struct {
    /// The declaration that is being exported.
    declaration: Node.Index,
    /// `true` if this declaration is the default export.
    default: bool = false,
};

pub const Number = struct {
    value_id: ExtraData.Index,
    token: Token.Index,

    /// Get the value of this numeric literal as an f64
    pub fn value(self: Number, t: *const Tree) f64 {
        const info = t.getExtraData(self.value_id);
        return info.number_value;
    }
};

pub const Boolean = struct {
    value: bool,
    token: Token.Index,
};

/// Data contained inside an AST node
pub const NodeData = union(enum(u8)) {
    program: ?SubRange,
    assignment_expr: BinaryPayload,
    binary_expr: BinaryPayload,
    member_expr: PropertyAccess,
    computed_member_expr: ComputedPropertyAccess,
    tagged_template_expr: TaggedTemplateExpression,
    meta_property: MetaProperty,
    arguments: ?SubRange,
    new_expr: NewExpr,
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

    /// An identifier that is neither a reference, nor a binding.
    /// E.g: property node in a member expression, a label in a labeled statement,
    /// or a key in an object literal.
    identifier: Token.Index,
    /// An identifier that references some value in the program.
    /// ```js
    /// let x = 1; // "x" is a `binding_identifier`
    /// x = 2; // "x" is an `identifier_reference`
    /// ```
    identifier_reference: Token.Index,
    /// An identifier occurring on the LHS of a declaration.
    /// E.g: In a variable binding, destructuring, or function parameter.
    /// ```js
    /// let x = 1; // "x" is a `binding_identifier`
    /// x = 2; // "x" is an `identifier_reference`
    /// ```
    binding_identifier: Token.Index,

    string_literal: Token.Index,
    number_literal: Number,
    boolean_literal: Boolean,
    null_literal: Token.Index,
    regex_literal: Token.Index,

    this: Token.Index,
    /// Represents a "missing" item in an array literal: `[,]`
    empty_array_item,
    array_literal: ?SubRange,
    array_pattern: ?SubRange,
    /// A SpreadElement is used in expressions to splat arrays/objects.
    /// `const arr = [1, 2, ...[3, 4]];`
    spread_element: Node.Index,
    /// A RestElement is used in patterns to represent destructured bindings like:
    /// `const [a, ...rest] = [1, 2, 3];`
    rest_element: Node.Index,
    object_literal: ?SubRange,
    /// key-value pair or method in an object-literal.
    object_property: PropertyDefinition,
    /// shorthand key-value pair in an object literal.
    /// e.g: `{ x }`.
    shorthand_property: ShorthandProperty,
    class_expression: Class,
    class_field: ClassFieldDefinition,
    class_method: ClassFieldDefinition,
    sequence_expr: SubRange,
    /// An expression wrapped in parentheses.
    /// The `start` and `end` tokens for this node are the opening
    /// and closing braces respectively.
    parenthesized_expr: Node.Index,
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
    debugger_statement,
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
    break_statement: JumpLabel,
    /// 'continue' ';'
    continue_statement: JumpLabel,
    parameters: ?SubRange,
    return_statement: ?Node.Index,
    import_declaration: ImportDeclaration,
    import_default_specifier: ImportDefaultSpecifier,
    import_specifier: ImportSpecifier,
    import_namespace_specifier: ImportNamespaceSpecifier,

    export_declaration: ExportedDeclaration,
    export_specifier: ExportSpecifier,
    /// Represents a node like `export { x, y, z }`
    export_list_declaration: struct { specifiers: SubRange },
    /// `export { <specifiers> } from "module"`
    export_from_declaration: struct {
        source: Node.Index,
        specifiers: SubRange,
    },
    export_all_declaration: struct {
        source: Node.Index,
        name: ?Node.Index,
    },

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
pub const ExtraData = union(enum) {
    pub const Index = enum(u32) { none = 0, _ };
    function: struct {
        /// Name of the function, if present (always an identifier node).
        name: ?Node.Index,
        /// Flags: generator, async, arrow, etc.
        flags: FunctionFlags,
    },
    number_value: f64,
    for_iterator: ForIterator,
    for_in_of_iterator: ForInOfIterator,
    class: ClassInfo,
};

pub const Node = struct {
    /// An index into the AST's `nodes` array list.
    pub const Index = enum(u32) {
        empty = 0,
        _,

        /// Get a pointer to the `NodeData` associated with this node-id.
        pub inline fn get(self: Index, tree: *const Tree) *const NodeData {
            return tree.nodeData(self);
        }
    };
    /// The actual data stored by this node.
    data: NodeData,
    /// The start token for this node.
    start: Token.Index,
    /// The end token for this node.
    end: Token.Index,

    comptime {
        std.debug.assert(@bitSizeOf(Node) <= 196);
    }
};
