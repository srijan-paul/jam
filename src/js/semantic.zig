const std = @import("std");
const util = @import("util");
const traverse = @import("./traverse.zig");
const ast = @import("ast.zig");
const StrUtil = @import("strings.zig");

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
    tree: *const ast.Tree,
    /// Top-level scope of the program.
    root_scope: *Scope,

    pub fn deinit(self: *AnalyzedProgram) void {
        self.root_scope.deinit(self.tree.allocator);
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
    /// The variable that this reference refers to.
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

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.variables.deinit(allocator);
        self.references.deinit(allocator);
        for (self.children.items) |child| {
            child.deinit(allocator);
        }
        self.children.deinit(allocator);
    }
};

const Self = @This();

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
tree: *const ast.Tree,
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

pub fn init(allocator: Allocator, tree: *ast.Tree) Allocator.Error!Self {
    const root_scope = try allocator.create(Scope);
    root_scope.* = try Scope.init(
        allocator,
        Scope.Kind.module,
        null, // top-level scope has no parent
        tree.source_type == .module, // top-level scope is strict in ESM
    );

    return Self{
        .allocator = allocator,
        .tree = tree,
        .root_scope = root_scope,
        .current_scope = root_scope,
        .stack = try List(BindingContext).initCapacity(allocator, 8),
        .strings = try StrUtil.init(allocator, tree.source, &tree.string_pool),
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit(self.allocator);
    self.strings.deinit();
}

pub fn analyze(_: *Self) void {}

pub fn onEnter(self: *Self, _: ast.Node.Index, node: ast.NodeData) !void {
    switch (node) {
        .block_statement => try self.createScope(Scope.Kind.block),
        .function_expr => try self.createScope(Scope.Kind.function),
        .function_declaration => |func| {
            const name_token_id = func.getNameTokenId(self.tree) orelse
                unreachable; // function *declaration*s always have a name
            try self.registerDeclaration(name_token_id);
            try self.createScope(Scope.Kind.function);
        },
        .binding_identifier => |name_id| try self.registerDeclaration(name_id),
        else => {},
    }
}

/// Create a new variable in the current scope with the given name.
pub fn registerDeclaration(self: *Self, name: Token.Index) !void {
    const curr_binding_ctx = self.stack.getLast();
    const token = self.tree.getToken(name);
    const name_str = try self.strings.stringValue(&token);

    const variable = Variable{
        .name = name_str,
        .def_node = curr_binding_ctx.decl_node,
        .scope = self.current_scope,
        .kind = curr_binding_ctx.kind,
    };

    try self.current_scope.variables.put(self.allocator, name_str, variable);
}

pub fn onExit(self: *Self, _: ast.Node.Index, node: ast.NodeData) Allocator.Error!void {
    switch (node) {
        .block_statement,
        .function_expr,
        .function_declaration,
        => self.exitScope(),
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

test {}
