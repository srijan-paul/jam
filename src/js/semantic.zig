// A semantic avar nalysis pass over the AST that:
// 1. Resolves variable definitions, scopes, and references.
// 2. Builds the control flow graph (TBD).
// 3. Creates a `parent_of_node` array, where `parent_of_node[node_id]`
//    is the parent of `node_id` (TBD)

const std = @import("std");
const util = @import("util");
const traverse = @import("./traverse.zig");
const ast = @import("ast.zig");
const StrUtil = @import("strings.zig");
const Parser = @import("./parser.zig");

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
        self.root_scope.deinit(self.tree.allocator);
        self.tree.allocator.destroy(self.root_scope);
        self.tree.allocator.free(self.parent_of_node);

        self.tree.deinit();
        self.tree.allocator.destroy(self.tree);
    }
};

pub const Variable = struct {
    pub const Kind = enum {
        /// A variable that is declared with `let` or `const`
        lexical_binding,
        /// A variable that is declared with `var`
        variable_binding,
    };

    /// Name of the variable, after processing any escapes codes
    /// in the identifier.
    name: String,
    /// AST node that declared this variable.
    /// E.g: variable declarator, function declaration,
    /// function parameter, import statement, or class declaration.
    def_node: ast.Node.Index,
    /// Scope to which this variable belongs
    scope: *const Scope,
    kind: Kind,
};

pub const Reference = struct {
    /// The variable that this reference points to.
    variable: *const Variable,
    /// The AST node that is the reference.
    node: ast.Node.Index,
};

/// Scope contains information about the variables contained within it,
/// along with any references made to other variables within the same or an outer scope.
pub const Scope = struct {
    pub const Kind = enum { function, block, module, script };

    /// Points to the surrounding scope.
    /// Is `null` for the global scope of the program.
    parent: ?*Scope,
    /// Map of variable name -> variable object
    variables: Map(String, Variable),
    /// List of all variable references in this scope.
    references: List(Reference),
    /// Nested scopes within this scope.
    children: List(*Scope),
    kind: Kind,
    is_strict: bool,

    pub fn init(
        allocator: Allocator,
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
    pub fn findVariable(self: *Scope, variable_name: String) ?Variable {
        if (self.variables.get(variable_name)) |v| {
            return v;
        }

        if (self.parent) |parent_scope| {
            return parent_scope.findVariable(variable_name);
        }

        return null;
    }

    /// Search for a variable inside this scope, but do not visit parent scopes.
    pub fn findVariableWithin(self: *Scope, variable_name: String) ?Variable {
        return self.variables.get(variable_name);
    }
};

const Self = @This();

const AnalysisError = error{ DuplicateBinding, Overflow, OutOfMemory };

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
stack: List(BindingContext),
/// Helper for string manipulation and escape code processing in identifiers
strings: StrUtil,

/// Create a semantic analyzer from a parse_result.
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
        .stack = try List(BindingContext).initCapacity(allocator, 8),
        .strings = try StrUtil.init(allocator, parse_result.tree.source, &parse_result.tree.string_pool),
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit(self.allocator);
    self.strings.deinit();

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
        .tree = self.tree,
        .root_scope = self.root_scope,
        .parent_of_node = self.parent_of_node,
    };
}

pub fn onEnter(self: *Self, node_id: ast.Node.Index, node: ast.NodeData, parent_id: ?ast.Node.Index) !void {
    switch (node) {
        .block_statement => try self.createScope(Scope.Kind.block),
        .function_expr => try self.createScope(Scope.Kind.function),
        .function_declaration => {
            try self.createScope(Scope.Kind.function);

            const ctx = BindingContext{
                .kind = .variable_binding,
                .decl_node = node_id,
            };
            try self.stack.append(self.allocator, ctx);
        },
        .variable_declarator => {
            const parent = parent_id.?.get(self.tree); // SAFETY: parent is sure to exist here
            const binding_kind: Variable.Kind = switch (parent.variable_declaration.kind) {
                .let, .@"const" => .lexical_binding,
                .@"var" => .variable_binding,
            };

            try self.stack.append(
                self.allocator,
                BindingContext{
                    .kind = binding_kind,
                    .decl_node = node_id,
                },
            );
        },
        .binding_identifier => |name_id| {
            std.debug.assert(self.stack.items.len > 0);
            const curr_binding_ctx = self.stack.getLast();
            const name_token = self.tree.getToken(name_id);

            if (std.mem.eql(u8, self.strings.toByteSlice(
                try self.strings.stringValue(name_token),
            ), "feb15")) {
                std.debug.print("here\n", .{});
            }

            try self.registerDeclaration(
                name_token,
                curr_binding_ctx.kind,
                curr_binding_ctx.decl_node,
            );
        },
        .parameters => unreachable, // TODO
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

    const variable = Variable{
        .name = name_str,
        .def_node = def_node,
        .scope = self.current_scope,
        .kind = binding_kind,
    };

    var dst_scope = self.current_scope;
    // Var bindings are hoisted out to then nearest functon scope
    // TODO: instead of doing this loop every time, we should cache it
    // and only update when exit a new scope.
    if (variable.kind == .variable_binding) {
        while (dst_scope.parent) |parent| {
            dst_scope = parent;
            if (parent.kind == .function)
                break;
        }
    }

    // the only case where both declarations are allowed to
    // co-exist is when both are var-bindings.
    // ```js
    // var a = 1;
    // var a = 2; // OK
    // function a() {} // OK
    // ```
    if (dst_scope.findVariableWithin(name_str)) |existing_var| {
        // There's already a variable with the same name in the current scope.
        // Only allow re-declaration if the existing variable is a var-binding.
        if (!(existing_var.kind == .variable_binding and variable.kind == .variable_binding))
            // TODO(@injuly): emit a diagnostic, and move on. Do not early exit on this error.
            return AnalysisError.DuplicateBinding;
    }

    try self.current_scope.variables.put(self.allocator, name_str, variable);
}

pub fn onExit(self: *Self, _: ast.Node.Index, node: ast.NodeData, _: ?ast.Node.Index) Allocator.Error!void {
    switch (node) {
        .block_statement, .function_expr => self.exitScope(),
        .function_declaration => {
            _ = self.stack.pop();
            self.exitScope();
        },
        .variable_declaration => _ = self.stack.pop(),
        else => {},
    }
}

/// Create and enter a new scope
fn createScope(self: *Self, scope_kind: Scope.Kind) Allocator.Error!void {
    const parent = self.current_scope;
    const new_scope = try self.allocator.create(Scope);
    new_scope.* = try Scope.init(
        self.allocator,
        scope_kind,
        parent,
        parent.is_strict,
    );

    try parent.children.append(self.allocator, new_scope);
    self.current_scope = new_scope;
}

fn exitScope(self: *Self) void {
    self.current_scope = self.current_scope.parent orelse
        std.debug.panic("attempt to exit root scope", .{});
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
    var result = parseAndAnalyze(t.allocator, source, .{}) catch |actual_error| {
        return t.expectEqual(expected_error, actual_error);
    };

    std.debug.print(
        "Expected {} but source was parsed successfully:\n{s}\n",
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

test {
    // error cases:

    try expectError(
        \\ let x = 1;
        \\ let x = 2;
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let { y: x } = 1;
        \\ let x = 1;
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let x; 
        \\ { var x; }  
    , AnalysisError.DuplicateBinding);

    // TODO: make this fail.
    // try expectError(
    //     \\ let { feb15 } = 1;
    //     \\ let feb15 = 1;
    // , AnalysisError.DuplicateBinding);

    try expectError(
        \\ function f() { let f; let f; }
    , AnalysisError.DuplicateBinding);

    try expectError(
        \\ let f = function () { let f; let f; }
    , AnalysisError.DuplicateBinding);

    // success cases:

    try expectError(
        \\ let x = 1;
        \\ var x = 2;
    , AnalysisError.DuplicateBinding);

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

    try expectNoError(
        \\ let f = function() { let f; } 
    );

    try expectNoError("let f = function f() {}");

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
}
