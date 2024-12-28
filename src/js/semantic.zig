const std = @import("std");
const util = @import("util");
const traverse = @import("./traverse.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
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
    /// Id of the scope to which this variable belongs
    scope_id: Scope.Id,
    kind: Kind,
};

pub const Scope = struct {
    /// Uniquely identifies a scope.
    /// This is a number that can be used to index into a scope slice.
    pub const Id = enum(u32) { _ };
    pub const Kind = enum {
        function,
        block,
        module,
        script,
    };

    /// Points to the surrounding scope.
    /// Is `null` for the global scope of the program.
    parent: ?Scope.Id,
    /// Map of variable name -> variable object
    variables: std.AutoHashMapUnmanaged(String, Variable),

    /// ID for this scope,
    /// unique within the same file.
    id: Scope.Id,
    kind: Kind,
    is_strict: bool,

    pub fn init() void {
        // TODO
    }

    pub fn deinit(self: *Scope) void {
        self.variables.deinit();
    }
};

const Self = @This();

allocator: Allocator,

pub fn init(allocator: Allocator) Self {
    return Self{ .allocator = allocator };
}

pub fn analyze(_: *Self) void {}

pub fn deinit() void {}

pub fn onEnter(_: *Self) void {}
pub fn onExit(_: *Self) void {}

test {}
