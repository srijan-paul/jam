// A scope analyzer for JavaScript.
// Consider this a zig port of eslint/eslint-scope.

const std = @import("std");
const util = @import("util");

const ast = @import("./ast.zig");

const StringPool = util.StringPool;
const String = StringPool.String;

const Scope = @This();

const ScopeKind = enum {
    global,
    module,
    function,
    block,
    @"switch",
    @"catch",
    @"for",
    with,
    class,
};

/// Definition for a `Variable`.
const Definition = struct {
    pub const Kind = enum { parameter, variable };
    /// Whether the definition is a parameter or variable declaration.
    kind: Kind,
    /// the AST node that defines this variable.
    /// Usually an identifier/pattern that represents a  parameter,
    /// or a variable_declarator node.
    node: ast.Node.Index,
};

/// A parameter, function, or variable declared in a scope.
const Variable = struct {
    /// Name of the variable, after processing any escapes codes
    /// in the identifier.
    name: String,
    /// References to the variable in the program.
    /// Only stores `ast.Node.identifier` nodes.
    refs: std.ArrayList(ast.Node.Index),
    /// Information about the variable's definition.
    definition: Definition,
};

kind: ScopeKind,
/// Reference to the node that defines this scope
/// in the program.
node: ast.Node.Index,
/// List of variables declared in this scope.
variables: std.ArrayList(Variable),
/// Unresolved references in this scope.
unresolved: std.ArrayList(ast.Node.Index),
/// The surrounding scope that encloses this scope
upper: *Scope,
/// Whether this scope is in strict mode or not.
is_strict: bool,
/// Child scopes enclosed by this scope.
children: std.ArrayList(Scope),
