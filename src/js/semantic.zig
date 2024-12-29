const std = @import("std");
const util = @import("util");
const traverse = @import("./traverse.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const List = std.ArrayListUnmanaged;
const Map = std.AutoHashMapUnmanaged;
const Traverser = traverse.Traverser;

const StringPool = util.StringPool;
const String = StringPool.String;

/// Result of the semantic analysis pass.
/// Contains resolved references, scopes, control flow graph, etc.
pub const AnalyzedProgram = struct {
    /// The syntax tree that was analyzed.
    tree: *const ast.Tree,
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

pub const Scope = struct {
    pub const Kind = enum { function, block, module, script };

    /// Points to the surrounding scope.
    /// Is `null` for the global scope of the program.
    parent: ?*const Scope,
    /// Map of variable name -> variable object
    variables: Map(String, Variable),
    /// List of all variable references in this scope.
    references: List(ast.Node.Index, ?*const Variable),
    /// Nested scopes within this scope.
    children: List(*const Scope),
    kind: Kind,
    is_strict: bool,

    pub fn init(
        allocator: Allocator,
        kind: Kind,
        parent: ?Scope.Id,
        is_strict: bool,
    ) Allocator.Error!Scope {
        return Scope{
            .variables = Map(String, Variable){},
            .references = try List(Reference).initCapacity(allocator, 8),
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
            allocator.destroy(self.children);
        }
    }
};

const Self = @This();

const BindingKind = enum {
    /// A variable that is declared with `let` or `const`, or class declaration
    lexical,
    /// A variable that is declared with `var`, or a function declaration.
    variable,
};

allocator: Allocator,
/// Syntax tree produced by the parser.
tree: *const ast.Tree,
/// Presently active scope.
current_scope: *Scope,
/// Top-level scope of the entire program.
/// Global declarations are stored here.
root_scope: *Scope,

current_binding: ?BindingKind = null,

pub fn init(allocator: Allocator, tree: *const ast.Tree) Self {
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
    };
}

pub fn analyze(_: *Self) void {}

pub fn onEnter(self: *Self, _: ast.Node.Index, node: ast.NodeData) Allocator.Error!void {
    switch (node) {
        .block_statement => try self.createScope(Scope.Kind.block),
        .function_expr => try self.createScope(Scope.Kind.function),
        .function_declaration => |_| {
            // TODO: add function name to the scope.
            try self.createScope(Scope.Kind.function);
        },
        else => {},
    }
}

pub fn onExit(self: *Self, _: ast.Node.Index, node: ast.NodeData) Allocator.Error!void {
    switch (node) {
        .block_statement,
        .function_expr,
        .function_declaration,
        => try self.exitScope(),
        else => {},
    }
}

/// Enter a new scope
fn createScope(self: *Self, scope_kind: Scope.Kind) Allocator.Error!void {
    const parent = self.current_scope;
    const new_scope = try self.allocator.create(Scope);
    new_scope.* = try Scope.init(
        self.allocator,
        scope_kind,
        parent,
        parent.is_strict,
    );

    try parent.children.append(new_scope);
    self.current_scope = new_scope;
}

fn exitScope(self: *Self) void {
    self.current_scope = self.current_scope.parent orelse
        std.debug.panic("attempt to exit root scope", .{});
}

test {}
