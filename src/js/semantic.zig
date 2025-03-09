// A semantic avar nalysis pass over the AST that:
// 1. Resolves variable definitions, scopes, and references.
// 2. Builds the control flow graph (TBD).
// 3. Creates a `parent_of_node` array, where `parent_of_node[node_id]`
//    is the parent of `node_id`.

const std = @import("std");
const util = @import("util");
const traverse = @import("./traverse.zig");
const ast = @import("ast.zig");
const StrUtil = @import("strings.zig");
const Parser = @import("./parser.zig");

const assert = std.debug.assert;
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const List = std.ArrayListUnmanaged;
const Map = std.AutoHashMapUnmanaged;
const Traverser = traverse.Traverser;

const StringPool = util.StringPool;
const String = StringPool.String;
const Token = ast.Token;

/// Result of the semantic analysis pass.
/// Contains resolved references, scopes, control flow graph, etc.
pub const AnalyzedProgram = struct {
    /// Used to allocate memory for this struct
    allocator: Allocator,
    /// The syntax tree that was analyzed.
    tree: *ast.Tree,
    /// Top-level scope of the program.
    root_scope: *Scope,
    /// Maps the ID of an AST node to the ID of its parent
    /// For the root node (the `program`), the parent is `null`.
    /// Every other node, except `.empty` and `.program` is guaranteed
    /// to have a parent.
    parent_of_node: []?ast.Node.Index,

    pub fn deinit(self: *AnalyzedProgram) void {
        // release resources allocated during semantic analysis
        self.root_scope.deinit(self.allocator);
        self.allocator.destroy(self.root_scope);
        self.allocator.free(self.parent_of_node);

        // release the tree
        self.tree.deinit();
        self.tree.allocator.destroy(self.tree);
    }
};

/// Represents any declaration in the program.
/// Can be a function name, imported item, class,
/// a parameter for catch/function block, etc.
pub const Variable = struct {
    pub const Kind = enum(u8) {
        const IsHoistedMask: u8 = 1 << 5;

        /// A variable that is declared with `let` or `const`
        lexical_binding = 0,
        /// A variable that is declared with `var`
        variable_binding = 1 | IsHoistedMask,
        /// A function parameter binding
        function_parameter = 2 | IsHoistedMask,
        /// A parameter in a catch block
        catch_parameter = 3,

        /// Whether the declaration is hoisted to the nearest function scope
        pub inline fn isHoisted(self: Kind) bool {
            return @intFromEnum(self) & IsHoistedMask != 0;
        }

        comptime {
            assert(!Kind.lexical_binding.isHoisted());
            assert(Kind.variable_binding.isHoisted());
            assert(Kind.function_parameter.isHoisted());
        }
    };

    /// Name of the variable, after processing any escapes codes
    /// in the identifier.
    name: String,
    /// AST node that declared this variable.
    /// E.g: variable declarator, function declaration,
    /// function parameter, import statement, or class declaration.
    def_node: ast.Node.Index,
    /// Scope to which this variable belongs
    scope: *Scope,
    kind: Kind,
};

pub const Reference = struct {
    /// The variable that this reference points to.
    to: *const Variable,
    /// The AST node that is the reference.
    from: ast.Node.Index,
};

/// Scope contains information about the variables contained within it,
/// along with any references made to other variables within the same or an outer scope.
pub const Scope = struct {
    pub const Kind = enum {
        function,
        block,
        module,
        script,
        class,
    };

    /// Points to the surrounding scope.
    /// Is `null` for the global scope of the program.
    parent: ?*Scope,
    /// Map of variable name -> variable object
    variables: Map(String, Variable),
    /// List of all variable references in this scope.
    references: List(Reference),
    /// Nested scopes within this scope.
    children: List(*Scope),
    /// Whether this a function, block, or module scope etc.
    kind: Kind,
    /// The node that this scope is associated with.
    node: ast.Node.Index,
    is_strict: bool,

    pub fn init(
        allocator: Allocator,
        node: ast.Node.Index,
        kind: Kind,
        parent: ?*Scope,
        is_strict: bool,
    ) Allocator.Error!Scope {
        return Scope{
            .variables = Map(String, Variable){},
            .references = try List(Reference).initCapacity(allocator, 8),
            .children = try List(*Scope).initCapacity(allocator, 4),
            .parent = parent,
            .kind = kind,
            .is_strict = is_strict,
            .node = node,
        };
    }

    /// De-initialize this scope and all it's child scopes
    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.variables.deinit(allocator);
        self.references.deinit(allocator);
        for (self.children.items) |child| {
            child.deinit(allocator);
            allocator.destroy(child);
        }
        self.children.deinit(allocator);
    }

    /// Find a variable by name in this scope or any of its parent scopes.
    pub fn findVariable(self: *Scope, variable_name: String) ?*const Variable {
        if (self.variables.getPtr(variable_name)) |v| {
            return v;
        }

        if (self.parent) |parent_scope| {
            return parent_scope.findVariable(variable_name);
        }

        return null;
    }

    /// Search for a variable inside this scope, but do not visit parent scopes.
    pub fn findVariableWithin(self: *Scope, variable_name: String) ?*const Variable {
        return self.variables.getPtr(variable_name);
    }
};

const Self = @This();

const AnalysisError = error{
    DuplicateBinding,
    DuplicateParameterBinding,
    Overflow,
    OutOfMemory,
};

const UnresolvedRef = struct {
    /// Name used to reference the declaration (that couldn't be found).
    name: String,
    /// The scope where the `identifier_reference` occurs
    ref_scope: *Scope,
};

/// Denotes the type of a variable binding (lexical or hoisted)
const BindingKind = enum {
    /// A variable that is declared with `let` or `const`, or class declaration
    lexical,
    /// A variable that is declared with `var`, or a function declaration.
    variable,
};

/// Information about the current variable binding that we're processing
const BindingContext = struct {
    /// Kind of the binding (lexical or variable/hoisted).
    kind: Variable.Kind,
    /// Nearest surrounding node that all subsequent declarations should be associated with.
    /// E.g, when registering `a` in `var a = 1`, `decl_node` is the `variable_declarator` node ('a = 1').
    decl_node: ast.Node.Index,
};

allocator: Allocator,
/// Syntax tree produced by the parser.
/// Whether the tree is owned by this semantic analyzer.
/// If true, the tree will be deallocated when the analyzer is deallocated.
/// If false, the tree is owned by the caller (because they called `analyze` successfully)
/// and will not be deallocated.
owns_tree: bool = true,
/// The parse tree returned by the parser
tree: *ast.Tree,
/// Maps an AST node ID to the ID of its parent
/// Every node is guaranteed to have a parent except
/// the `.program` and `.none` node types.
parent_of_node: []?ast.Node.Index,
/// Presently active scope.
current_scope: *Scope,
/// Top-level scope of the entire program.
/// Global declarations are stored here.
root_scope: *Scope,

/// Stack of nested variable binding contexts.
/// Eg:
/// ```js
/// var { a /*var*/ =
///             () => {  let x /*lexical*/ = 1 },
///       b /*var*/ } = obj;
/// ```
binding_stack: List(BindingContext),
/// Helper for string manipulation and escape code processing in identifiers
strings: StrUtil,
/// List of references that could not be resolved during the first
/// semantic analysis pass because of, say, hoisting.
/// When the AST walk is over and we exit the `.program` node,
/// all unresolved references are visited to try and resolve them
/// one more time.
unresolved_refs: List(UnresolvedRef),

/// Create a semantic analyzer from a Parser.Result.
/// Takes ownership of the parse_result, and the caller must not call `parse_result.deinit()`
/// after calling this function.
pub fn init(allocator: Allocator, parse_result: Parser.Result) Allocator.Error!Self {
    // since we own the parse result now, we must ensure it's deallocated
    // if we error out from this function.

    var p = parse_result; // params are const...
    errdefer p.deinit();

    const root_scope = try allocator.create(Scope);
    root_scope.* = try Scope.init(
        allocator,
        parse_result.tree.root,
        Scope.Kind.module,
        null, // top-level scope has no parent
        parse_result.tree.source_type == .module, // top-level scope is strict in ESM
    );

    const parent_of_node = try allocator.alloc(?ast.Node.Index, parse_result.tree.nodes.len);

    return Self{
        .allocator = allocator,
        .tree = parse_result.tree,
        .root_scope = root_scope,
        .current_scope = root_scope,
        .parent_of_node = parent_of_node,
        .binding_stack = try List(BindingContext).initCapacity(allocator, 8),
        .unresolved_refs = try List(UnresolvedRef).initCapacity(allocator, 8),
        .strings = try StrUtil.init(allocator, parse_result.tree.source, &parse_result.tree.string_pool),
    };
}

pub fn deinit(self: *Self) void {
    self.binding_stack.deinit(self.allocator);
    self.strings.deinit();
    self.unresolved_refs.deinit(self.allocator);

    // TODO: ensure that we're using the right allocators for the right
    // things. OR that both self.tree.allocator and self.allocator are the same.
    if (self.owns_tree) {
        self.tree.deinit();
        self.tree.allocator.destroy(self.tree);

        self.root_scope.deinit(self.allocator);
        self.allocator.free(self.parent_of_node);
        self.allocator.destroy(self.root_scope);
    }
}

// TODO(@injuly): add explicit error union annotation
pub fn analyze(self: *Self) !AnalyzedProgram {
    const AstTraverser = Traverser(Self);
    var traverser = AstTraverser{
        .t = self.tree,
        .ctrl = self,
    };

    try traverser.traverse();

    self.owns_tree = false;
    return AnalyzedProgram{
        .allocator = self.allocator,
        .tree = self.tree,
        .root_scope = self.root_scope,
        .parent_of_node = self.parent_of_node,
    };
}

pub fn onEnter(
    self: *Self,
    node_id: ast.Node.Index,
    node: ast.NodeData,
    parent_id: ?ast.Node.Index,
) !void {
    self.parent_of_node[@intFromEnum(node_id)] = parent_id;

    switch (node) {
        .function_expr, .function_declaration => {
            const func_ctx = BindingContext{ .kind = .variable_binding, .decl_node = node_id };
            try self.binding_stack.append(self.allocator, func_ctx);
        },

        .class_declaration => |class| {
            if (class.nameTokenId(self.tree)) |name_id| {
                try self.registerDeclaration(
                    self.tree.getToken(name_id),
                    .lexical_binding,
                    node_id,
                );
            }

            try self.createScope(.class, node_id, true);
        },

        .catch_clause => |c| {
            if (c.param) |param_id| {
                const param_ctx = BindingContext{ .kind = .catch_parameter, .decl_node = param_id };
                try self.binding_stack.append(self.allocator, param_ctx);
            }
            try self.createScope(Scope.Kind.block, node_id, false);
        },

        .import_default_specifier, .import_specifier, .import_namespace_specifier => {
            const import_ctx = BindingContext{ .kind = .lexical_binding, .decl_node = node_id };
            try self.binding_stack.append(self.allocator, import_ctx);
        },

        .for_iterator,
        .for_in_of_iterator,
        .block_statement,
        => try self.createScope(Scope.Kind.block, node_id, false),

        .variable_declarator => {
            const parent = parent_id.?.get(self.tree); // SAFETY: parent is sure to exist here
            const binding_kind: Variable.Kind = switch (parent.variable_declaration.kind) {
                .let, .@"const" => .lexical_binding,
                .@"var" => .variable_binding,
            };

            try self.binding_stack.append(
                self.allocator,
                BindingContext{
                    .kind = binding_kind,
                    .decl_node = node_id,
                },
            );
        },
        .binding_identifier => |name_id| {
            switch (self.tree.tag(parent_id.?)) {
                .class_meta, .function_meta => return,
                else => {},
            }

            assert(self.binding_stack.items.len > 0);
            const curr_binding_ctx = self.binding_stack.getLast();
            const name_token = self.tree.getToken(name_id);

            try self.registerDeclaration(
                name_token,
                curr_binding_ctx.kind,
                curr_binding_ctx.decl_node,
            );
        },
        .parameters => {
            // create a broader scope for the function
            const func_id = parent_id.?; // SAFETY: parent is sure to exist here
            const func_has_use_strict =
                switch (func_id.get(self.tree).*) {
                    .function_declaration,
                    .function_expr,
                    => |func| func.hasStrictDirective(self.tree),
                    else => panic("BUG: invalid parent node for `parameters` AST node type", .{}),
                };

            try self.createScope(.function, func_id, func_has_use_strict);

            const params_ctx = BindingContext{ .kind = .function_parameter, .decl_node = node_id };
            try self.binding_stack.append(self.allocator, params_ctx);
        },
        .identifier_reference => |token_id| {
            const name = try self.strings.stringValue(self.tree.getToken(token_id));
            if (self.current_scope.findVariable(name)) |variable| {
                const reference = Reference{ .from = node_id, .to = variable };
                try variable.scope.references.append(self.allocator, reference);
            } else {
                try self.unresolved_refs.append(self.allocator, UnresolvedRef{
                    .name = name,
                    .ref_scope = self.current_scope,
                });
            }
        },
        else => {},
    }
}

/// Create a new variable in the current scope with the given name.
fn registerDeclaration(
    self: *Self,
    name_token: *const Token,
    binding_kind: Variable.Kind,
    def_node: ast.Node.Index,
) AnalysisError!void {
    const name_str = try self.strings.stringValue(name_token);

    // Var bindings are hoisted out to then nearest functon scope
    // TODO: instead of doing this loop every time, we should cache it
    // and only update when exit a new scope.
    var dst_scope = self.current_scope;
    if (binding_kind.isHoisted() and dst_scope.kind != .function) {
        while (dst_scope.parent) |parent| {
            if (dst_scope.kind == .function) break;
            dst_scope = parent;
        }
    }

    const variable = Variable{
        .name = name_str,
        .def_node = def_node,
        .scope = dst_scope,
        .kind = binding_kind,
    };

    // the only case where both declarations are allowed to
    // co-exist is when both are var-bindings.
    // ```js
    // var a = 1;
    // var a = 2; // OK
    // function a() {} // OK
    // ```
    if (dst_scope.findVariableWithin(name_str)) |existing_var| {
        const existing_kind = existing_var.kind;
        const new_kind = variable.kind;
        // There's already a variable with the same name in the current scope.
        // Only allow re-declaration if the existing variable is a var-binding.
        if (!(existing_kind.isHoisted() and new_kind.isHoisted())) {
            // TODO(@injuly): emit a diagnostic, and move on. Do not early exit on this error.
            return AnalysisError.DuplicateBinding;
        } else {
            // In non-strict mode, also called "sloppy mode" (yeah...), it's okay to
            // have duplicate parameter bindings in the same function scope.
            //
            // ```js
            // function f(a, a) { } // OK!
            // function f(a, a) { "use strict" } // NOT OK!
            // ```
            const is_illegal_parameter_redecl =
                existing_kind == .function_parameter and
                new_kind == .function_parameter and
                dst_scope.kind == .function and dst_scope.is_strict;

            if (is_illegal_parameter_redecl) {
                return AnalysisError.DuplicateParameterBinding;
            }
        }
    }

    try self.current_scope.variables.put(self.allocator, name_str, variable);
}

pub fn onExit(
    self: *Self,
    node_id: ast.Node.Index,
    _: ast.NodeData,
    _: ?ast.Node.Index,
) Allocator.Error!void {
    const stack_len = self.binding_stack.items.len;
    if (stack_len > 0 and self.binding_stack.items[stack_len - 1].decl_node == node_id) {
        _ = self.binding_stack.pop();
    }

    if (node_id != self.tree.root and self.current_scope.node == node_id) {
        self.exitScope();
    }
}

/// Create and enter a new scope
fn createScope(
    self: *Self,
    scope_kind: Scope.Kind,
    node: ast.Node.Index,
    is_strict: bool,
) Allocator.Error!void {
    const parent = self.current_scope;
    const new_scope = try self.allocator.create(Scope);
    new_scope.* = try Scope.init(
        self.allocator,
        node,
        scope_kind,
        parent,
        parent.is_strict or is_strict,
    );

    try parent.children.append(self.allocator, new_scope);
    self.current_scope = new_scope;
}

fn exitScope(self: *Self) void {
    self.current_scope = self.current_scope.parent orelse
        panic("attempt to exit root scope", .{});
}

pub fn parseAndAnalyze(allocator: Allocator, source: []const u8, parser_config: Parser.Config) !AnalyzedProgram {
    var parser = try Parser.init(allocator, source, parser_config);
    defer parser.deinit();
    const parse_result = try parser.parse();

    var semantic_analyzer = try Self.init(allocator, parse_result);
    defer semantic_analyzer.deinit();

    return semantic_analyzer.analyze();
}

// tests

const t = std.testing;
fn expectError(source: []const u8, expected_error: AnalysisError) !void {
    var result = parseAndAnalyze(
        t.allocator,
        source,
        .{ .source_type = .script },
    ) catch |actual_error| {
        return t.expectEqual(expected_error, actual_error);
    };

    std.debug.print(
        "Expected {} but source was parsed successfully:\n{s}\n",
        .{ expected_error, source },
    );
    result.deinit();

    return error.TestExpectEqual;
}

fn expectErrorInModule(source: []const u8, expected_error: AnalysisError) !void {
    var result = parseAndAnalyze(
        t.allocator,
        source,
        .{ .source_type = .module },
    ) catch |actual_error| {
        return t.expectEqual(expected_error, actual_error);
    };

    std.debug.print(
        "Expected {} but module was parsed successfully:\n{s}\n",
        .{ expected_error, source },
    );
    result.deinit();

    return error.TestExpectEqual;
}

fn expectNoError(source: []const u8) !void {
    var result = parseAndAnalyze(t.allocator, source, .{}) catch |e| {
        std.debug.print(
            "Expected no error but got {}:\n{s}\n",
            .{ e, source },
        );

        return error.TestExpectError;
    };
    result.deinit();
}

test Self {
    // error cases:

    try expectError(
        "function f(param) { let param; }",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        \\ let x = 1;
        \\ let x = 2;
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let { y: x } = 1;
        \\ let x = 1;
    , AnalysisError.DuplicateBinding);

    try expectError(
        "let { x, x } = 1",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "let { x, x: { y: x } } = 1",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "let { x, x: { x } } = 1",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "class C {}; let C;",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "class C {}; var C;",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "class C{}; class C{};",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "class C { constructor(a, a) {} };",
        AnalysisError.DuplicateParameterBinding,
    );

    try expectError(
        "class C {}; let C;",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        \\ let x; 
        \\ { var x; }  
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let { x } = 1;
        \\ let x = 1;
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let { y: x } = 456;
        \\ {
        \\     var { x: x } = 123;
        \\ }
    , AnalysisError.DuplicateBinding);

    try expectError(
        "function f() { let f; let f; }",
        AnalysisError.DuplicateBinding,
    );

    try expectError(
        "function f(a, a) { 'use strict' }",
        AnalysisError.DuplicateParameterBinding,
    );

    try expectErrorInModule(
        "function f(a, a) {}",
        AnalysisError.DuplicateParameterBinding,
    );

    try expectError(
        \\ let f = function () { let f; let f; }
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let x = 1;
        \\ var x = 2;
    , AnalysisError.DuplicateBinding);

    // success cases:

    try expectNoError(
        \\ let x = 123;
        \\ let y = 123;
    );

    try expectNoError(
        \\ var x = 123;
        \\ var x = 123;
    );

    try expectNoError(
        \\ var x = 123;
        \\ function x() { let x; }
    );

    try expectNoError("let f = function() { let f; } ");
    try expectNoError("let f = function f() {}");
    try expectNoError("function f() { let f; }");
    try expectNoError("let { x: a, x: b } = 2");

    try expectNoError(
        \\ let x;
        \\ {
        \\   let x;
        \\ }
    );

    try expectNoError(
        \\ var x;
        \\ {
        \\   let x;
        \\ }
    );

    try expectNoError(
        \\ try { let e; }
        \\ catch (e) { }
    );

    try expectNoError(
        \\ let e
        \\ try { }
        \\ catch (e) { }
    );

    try expectNoError("for (let a;;); let a; ");
}
