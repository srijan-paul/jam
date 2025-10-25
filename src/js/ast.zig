const std = @import("std");
const util = @import("util");
const Parser = @import("parser.zig");
pub const Token = @import("token.zig").Token;

const meta = std.meta;
const assert = std.debug.assert;
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

    pub fn getStartTokenId(self: *const Tree, index: Node.Index) Token.Index {
        return self.nodes.items(.start)[@intFromEnum(index)];
    }

    pub fn getEndTokenId(self: *const Tree, index: Node.Index) Token.Index {
        return self.nodes.items(.end)[@intFromEnum(index)];
    }

    pub fn nodeData(self: *const Tree, index: Node.Index) *const NodeData {
        return &self.nodes.items(.data)[@intFromEnum(index)];
    }

    // TODO: add a getTokenPtr
    /// Get a token by its index (Token.Index)
    pub fn getToken(self: *const Tree, index: Token.Index) *const Token {
        return &self.tokens.items[@intFromEnum(index)];
    }

    /// Returns the string slice for a token by its ID
    pub fn getTokenSlice(self: *const Tree, id: Token.Index) []const u8 {
        return self.tokens.items[@intFromEnum(id)].toByteSlice(self.source);
    }

    /// Returns the tag representing the type of a token.
    pub fn getTokenKind(self: *const Tree, id: Token.Index) Token.Tag {
        return self.tokens.items[@intFromEnum(id)].tag;
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

    pub fn tag(self: *const Tree, node_id: Node.Index) meta.Tag(NodeData) {
        // TODO: optimize using a cached self.nodes.slice().
        return meta.activeTag(
            self.nodes.items(.data)[@intFromEnum(node_id)],
        );
    }

    /// Get the source text for a node as a byte slice.
    pub fn nodeToByteSlice(self: *const Tree, node_id: Node.Index) []const u8 {
        // TODO: use a cached nodes.slice instead of .start and .end
        const start = self.nodes.items(.start)[@intFromEnum(node_id)];
        const end = self.nodes.items(.end)[@intFromEnum(node_id)];

        const start_byte = self.getToken(start).start;
        const end_byte = start_byte + self.getToken(end).len;
        return self.source[start_byte..end_byte];
    }

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit(self.allocator);
        self.tokens.deinit(self.allocator);
        self.extras.deinit(self.allocator);
        self.string_pool.deinit();
        self.node_indices.deinit(self.allocator);
    }
};

pub const BinaryPayload = struct {
    lhs: Node.Index,
    rhs: Node.Index,
    operator: Token.Index,

    /// Return the operator token
    pub fn getOperator(self: *const BinaryPayload, t: *const Tree) *const Token {
        return t.getToken(self.operator);
    }

    /// Return the type tag of the operator token
    pub fn getOperatorKind(self: *const BinaryPayload, t: *const Tree) Token.Tag {
        return t.getTokenKind(self.operator);
    }
};

pub const UnaryPayload = struct {
    operand: Node.Index,
    operator: Token.Index,

    /// Return the operator token
    pub fn getOperator(self: *const UnaryPayload, t: *const Tree) *const Token {
        return t.getToken(self.operator);
    }

    /// Return the type tag of the operator token
    pub fn getOperatorKind(self: *const UnaryPayload, t: *const Tree) Token.Tag {
        return t.getTokenKind(self.operator);
    }
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
    /// Whether the function body has a "use strict" directive.
    has_strict_directive: bool = false,
    _: u4 = 0,
};

// TODO(@injuly): have separate `Function` structs for function declaration
// and function expression, to properly handle the `name` field in the type system,
// is optional for function expressions.

/// Represents the payload for a function expression or declaration.
pub const Function = struct {
    /// points to a `ast.NodeData.parameters` (?SubRange)
    parameters: Node.Index,
    /// points to an expression or a statement list
    body: Node.Index,
    /// Function name and flags.
    meta: Node.Index,
    /// Get the name node of this function, if it has one.
    pub fn getName(self: *const Function, tree: *const Tree) ?Node.Index {
        const fn_meta = self.meta.get(tree);
        assert(meta.activeTag(fn_meta.*) == .function_meta);

        if (fn_meta.function_meta.name) |name_id| {
            assert(name_id.tag(tree) == .binding_identifier);
            return name_id;
        }

        return null;
    }

    /// Get a string slice representing the name of this function.
    pub fn getNameSlice(self: *const Function, tree: *const Tree) ?[]const u8 {
        if (self.getName(tree)) |name_id| {
            assert(name_id.tag(tree) == .binding_identifier);
            return tree.getTokenSlice(name_id.get(tree).binding_identifier);
        }
        return null;
    }

    /// Get the token ID for the name of this function
    pub fn getNameTokenId(self: *const Function, tree: *const Tree) ?Token.Index {
        if (self.getName(tree)) |name| {
            assert(name.tag(tree) == .binding_identifier);
            return name.get(tree).binding_identifier;
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
            return params_range.asSlice(tree);
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

    /// Check whether the body of this function has a "use strict" directive
    pub fn hasStrictDirective(self: *const Function, tree: *const Tree) bool {
        const fn_meta = self.meta.get(tree);
        assert(meta.activeTag(fn_meta.*) == .function_meta);
        return fn_meta.function_meta.flags.has_strict_directive;
    }
};

pub const ForStatement = struct {
    iterator: Node.Index,
    body: Node.Index,
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
    /// points to a 'statement_list'
    body: Node.Index,
    /// points to an identifier or a pattern.
    param: ?Node.Index,
};

pub const TryStatement = struct {
    /// Points to a 'statement_list'
    body: Node.Index,
    /// Points to a 'catch_clause'
    catch_clause: Node.Index,
    /// Points to a 'statement_list'
    finalizer: Node.Index,

    // Get a pointer to the body of this try statement
    pub fn getBody(self: *const TryStatement, t: *const Tree) *const SubRange {
        const body = self.body.get(t);
        assert(meta.activeTag(body.*) == .statement_list);
        return &body.statement_list;
    }

    /// Get a slice of all statements in the body of this try statement
    pub fn getBodySlice(self: *const TryStatement, t: *const Tree) []const Node.Index {
        return self.getBody(t).asSlice(t);
    }
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

    /// Returns a slice containing all the case nodes in the switch statement
    pub fn getCases(self: *const SwitchStatement, t: *const Tree) []const Node.Index {
        return self.cases.asSlice(t);
    }
};

pub const WithStatement = struct {
    object: Node.Index,
    body: Node.Index,
};

pub const LabeledStatement = struct {
    label: Node.Index,
    body: Node.Index,
};

pub const ClassMeta = struct {
    name: Node.Index,
    super_class: Node.Index,
};

pub const FunctionMeta = struct {
    name: ?Node.Index,
    flags: FunctionFlags,
};

// TODO(@injuly): have optional name property for class expression,
// but make it non-nullable for class declaration.
pub const Class = struct {
    /// Points to a `ast.NodeData.class_meta`
    meta: Node.Index,
    body: SubRange,

    /// Return a slice representing the name of the class.
    pub fn className(self: *const Class, t: *const Tree) ?[]const u8 {
        if (self.nameTokenId(t)) |name_id| {
            return t.getTokenSlice(name_id);
        }

        return null;
    }

    /// Get the token ID for the name of this class
    pub fn nameTokenId(self: *const Class, t: *const Tree) ?Token.Index {
        const class_meta_id = self.meta.get(t);
        assert(meta.activeTag(class_meta_id.*) == .class_meta);

        const name_id = class_meta_id.class_meta.name;
        if (name_id == .empty) return null;
        return name_id.get(t).binding_identifier;
    }
};

/// A tagged template expression like:
/// ```js
/// div`something ${here}`
/// ```
pub const TaggedTemplateExpression = struct {
    tag: Node.Index,
    /// Points to a template_literal
    template: Node.Index,

    /// Get a slice of of all elements in the template literal
    pub fn getTemplateLiteral(self: *const TaggedTemplateExpression, t: *const Tree) []const Node.Index {
        const template_node = self.template.get(t);
        assert(meta.activeTag(template_node.*) == .template_literal);
        return template_node.template_literal.asSlice(t);
    }
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

/// `export * from "module"`
pub const ExportAllDeclaration = struct {
    source: Node.Index,
    // TODO: this is always a string literal
    name: ?Node.Index,
};

/// `export { x, y, z } from "module"`
pub const ExportFromDeclaration = struct {
    source: Node.Index,
    specifiers: SubRange,
};

/// `export { x, y, z }`
pub const ExportListDeclaration = struct {
    specifiers: SubRange,
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

/// JSX children wrapped inside '<> </>'.
/// https://facebook.github.io/jsx/#prod-JSXFragment
pub const JsxFragment = struct {
    /// All children are one of:
    /// - `jsx_text`
    /// - `jsx_expression`
    /// - `jsx_fragment`
    /// - `jsx_element`
    children: SubRange,
    /// The tokens in '<>' and '</>'
    open_close_tags: ExtraData.Index,

    /// Get the '<' and '>' tokens in the opening and closing elements of the fragment
    /// ("<>" and "</>")
    pub fn getOpenAndCloseTags(
        self: *const JsxFragment,
        t: *const Tree,
    ) JsxFragmentTags {
        const data = t.getExtraData(self.open_close_tags);
        assert(meta.activeTag(data) == .jsx_fragment_indices);
        return data.jsx_fragment_indices;
    }
};

pub const JsxElement = struct {
    opening_element: Node.Index,
    children: Node.Index,
    closing_element: Node.Index,

    /// Get the opening element of this JSXElement
    pub fn getOpening(self: JsxElement, t: *const Tree) JsxOpeningElement {
        return self.opening_element.get(t).jsx_opening_element;
    }

    /// Get the closing element of this JSXElement
    pub fn getClosing(self: JsxElement, t: *const Tree) JsxClosingElement {
        return self.closing_element.get(t).jsx_closing_element;
    }
};

pub const JsxOpeningElement = struct {
    name: Node.Index,
    attributes: SubRange,
};

pub const JsxClosingElement = struct {
    name: Node.Index,
};

pub const JsxAttribute = struct {
    name: Node.Index,
    value: ?Node.Index,
};

pub const JsxMemberExpression = struct {
    /// Can be one of:
    /// - jsx_identifier_reference
    /// - jsx_member_expression
    object: Node.Index,
    /// The field name after the ".".
    /// This is guaranteed to always be a 'jsx_identifier_reference'
    property: Node.Index,

    /// Get the field name of the member expression
    pub fn getProperty(self: *const JsxMemberExpression, t: *const Tree) Token.Index {
        const property = self.property.get(t);
        assert(meta.activeTag(property.*) == .jsx_identifier);
        return property.jsx_identifier;
    }
};

pub const JsxNamespacedName = struct {
    /// Can be one of:
    /// - jsx_identifier_reference
    /// - jsx_member_expression
    namespace: Node.Index,
    /// The name after the ":".
    /// This is guaranteed to always be a 'jsx_identifier_reference'
    name: Node.Index,

    /// Get the field name of the member expression
    pub fn getProperty(self: *const JsxNamespacedName, t: *const Tree) Token.Index {
        const property = self.name.get(t);
        assert(meta.activeTag(property.*) == .jsx_identifier);
        return property.jsx_identifier;
    }
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

    // Literals:
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
    /// The name and superclass of a class def node.
    class_meta: ClassMeta,
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
    // TODO: replace ?SubRange with SubRange
    block_statement: ?SubRange,
    statement_list: SubRange,
    expression_statement: Node.Index,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    function_declaration: Function,
    function_meta: FunctionMeta,
    class_declaration: Class,
    debugger_statement,
    if_statement: Conditional,
    do_while_statement: WhileStatement,
    while_statement: WhileStatement,
    with_statement: WithStatement,
    // TODO: store the throw keyword in a struct
    throw_statement: Node.Index,

    for_statement: ForStatement,
    for_of_statement: ForStatement,
    for_in_statement: ForStatement,

    for_iterator: ForIterator,
    for_in_of_iterator: ForInOfIterator,

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
    export_list_declaration: ExportListDeclaration,
    /// `export { <specifiers> } from "module"`
    export_from_declaration: ExportFromDeclaration,
    export_all_declaration: ExportAllDeclaration,

    // JSX:

    /// A list of JSX children nodes.
    jsx_fragment: JsxFragment,
    jsx_element: JsxElement,
    jsx_children: SubRange,

    /// A pair of jsx_opening_element and jsx_closing_element
    jsx_opening_element: JsxOpeningElement,
    /// A closing JSX element like '</Foo>'
    jsx_closing_element: JsxClosingElement,
    /// A self-closing JSX element like '<Foo />'
    jsx_self_closing_element: JsxOpeningElement,
    jsx_attribute: JsxAttribute,
    /// Non-javascript text inside a JSX element.
    jsx_text: Token.Index,
    /// A JSX element wrapped within {}s
    jsx_expression: Node.Index,
    /// An attribute name, like a prop key.
    jsx_identifier: Token.Index,
    /// A JSX Element name
    jsx_identifier_reference: Token.Index,
    jsx_member_expression: JsxMemberExpression,
    jsx_namespaced_name: JsxNamespacedName,
    /// A JSX spread element wrapped within '{}'s
    /// The Node.Index payload points to the expression that is being spread.
    jsx_spread_child: Node.Index,
    jsx_spread_attribute: Node.Index,

    /// Represents `null` AST node.
    /// This is a sentinel, and always present at index 0 of the `nodes` array.
    /// Used to represent nodes like a missing `else` branch (instead of ?Node.Index)
    /// which would take up more space, and increase the size of this union.
    none,

    comptime {
        assert(@bitSizeOf(NodeData) <= 128);
    }
};

/// The tokens in '<>' and '</>' that open and close a JSX fragment.
pub const JsxFragmentTags = struct {
    /// The '<' token that opens the jsx fragment.
    opening_lt_token: Token.Index,
    /// The '<' token that closes the jsx fragment.
    closing_lt_token: Token.Index,

    /// The opening "<>"
    pub const Opening = struct {
        lt: Token.Index,
        gt: Token.Index,
    };

    /// The "</>" tokens
    pub const Closing = struct {
        lt: Token.Index,
        gt: Token.Index,
        slash: Token.Index,
    };

    /// Get the "<" and ">" tokens that open the JSX fragment.
    pub fn getOpening(self: JsxFragmentTags, t: *const Tree) Opening {
        // skip any comments or whitespace tokens between the '<' and closing '>'
        var opening_gt_token: u32 = self.opening_lt_token.asU32() + 1;
        while (t.getTokenKind(@enumFromInt(opening_gt_token)) != .@">") : (opening_gt_token += 1) {}

        return Opening{
            .lt = self.opening_lt_token,
            .gt = @enumFromInt(opening_gt_token),
        };
    }

    /// Get the "<", "/" and ">" tokens that close the JSX fragment
    pub fn getClosing(self: JsxFragmentTags, t: *const Tree) Closing {
        var slash_token: u32 = self.closing_lt_token.asU32() + 1;
        while (t.getTokenKind(@enumFromInt(slash_token)) != .@"/") : (slash_token += 1) {}

        var closing_gt_token: u32 = slash_token + 1;
        while (t.getTokenKind(@enumFromInt(closing_gt_token)) != .@">") : (closing_gt_token += 1) {}

        return Closing{
            .lt = self.closing_lt_token,
            .gt = @enumFromInt(closing_gt_token),
            .slash = @enumFromInt(slash_token),
        };
    }
};

/// Extra metadata about a node.
/// The specific type of meta-data is determined by the node's
/// tag (i.e the active field of NodeData).
pub const ExtraData = union(enum) {
    pub const Index = enum(u32) { none = 0, _ };
    number_value: f64,
    jsx_fragment_indices: JsxFragmentTags,
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

        /// Get the AST node tag.
        pub inline fn tag(self: Index, tree: *const Tree) meta.Tag(NodeData) {
            return tree.tag(self);
        }

        pub fn startTokenId(self: Index, tree: *const Tree) Token.Index {
            return tree.getStartTokenId(self);
        }

        pub fn endTokenId(self: Index, tree: *const Tree) Token.Index {
            return tree.getEndTokenId(self);
        }
    };
    /// The actual data stored by this node.
    data: NodeData,
    /// The start token for this node.
    start: Token.Index,
    /// The end token for this node.
    end: Token.Index,

    /// Check if this node is a literal
    pub fn isLiteral(self: *const Node) bool {
        return switch (meta.activeTag(self.data)) {
            .string_literal,
            .number_literal,
            .boolean_literal,
            .null_literal,
            .regex_literal,
            => true,
            else => false,
        };
    }

    /// Get the type tag for this AST node
    pub fn tag(self: *const Node) meta.Tag(NodeData) {
        return meta.activeTag(self.data);
    }

    comptime {
        assert(@bitSizeOf(Node) <= 196);
    }
};
