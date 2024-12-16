// Some rudimentary scope analysis needed for parsing.
const std = @import("std");
const util = @import("util");
const ast = @import("ast.zig");

const StringPool = util.StringPool;
const String = StringPool.String;

const Self = @This();

pub const Variable = struct {
    pub const Kind = enum { lexical_binding, variable_binding };
    /// Index into `variables`.
    pub const Id = enum(u32) { _ };
    /// Name of the variable, after processing any escapes codes
    /// in the identifier.
    name: String,
    /// AST node that declared this variable.
    /// E.g: variable declarator, function declaration,
    /// function parameter, import statement, or class declaration.
    def_node: ast.Node.Index,
    /// Next variable in the same scope.
    next: ?Variable.Id = null,
    kind: Kind,
};

const AllocError = std.mem.Allocator.Error;

pub const Scope = struct {
    /// Index into `scopes`
    pub const Id = enum(u32) { _ };

    pub const Kind = enum {
        function,
        block,
        module,
        script,
    };

    /// Points to the surrounding scope
    /// Only nil for the global/root scope
    /// of the program.
    upper: ?Scope.Id,

    /// Variables are stored in a linked list.
    variables: ?struct {
        head: Variable.Id,
        tail: Variable.Id,
    } = null,

    kind: Kind,
    /// `true` if this scope is in strict mode.
    is_strict: bool,
};

al: std.mem.Allocator,

/// All variables declared in the program are stored in this flat array.
variables: std.ArrayList(Variable),
/// All scopes that exist in the program
scopes: std.ArrayList(Scope),
/// Currently active scope
current_scope: *Scope,

pub fn init(al: std.mem.Allocator, root_scope_kind: Scope.Kind) AllocError!Self {
    const current_scope = Scope{
        .upper = null,
        .kind = root_scope_kind,
        .is_strict = root_scope_kind == .module,
    };

    var self = Self{
        .al = al,
        .current_scope = undefined, // initialized below
        .variables = try std.ArrayList(Variable).initCapacity(al, 16),
        .scopes = try std.ArrayList(Scope).initCapacity(al, 4),
    };

    try self.scopes.append(current_scope);
    self.current_scope = &self.scopes.items[0];
    return self;
}

pub fn deinit(self: *Self) void {
    self.variables.deinit();
    self.scopes.deinit();
}

pub fn enterScope(self: *Self, kind: Scope.Kind, is_strict: bool) AllocError!void {
    const scope = Scope{
        .kind = kind,
        .is_strict = is_strict,
        .upper = @enumFromInt(self.scopes.items.len - 1),
    };

    try self.scopes.append(scope);
    self.current_scope = &self.scopes.items[self.scopes.items.len - 1];
}

pub fn exitScope(self: *Self) void {
    const parent_scope_id = self.current_scope.upper orelse
        std.debug.panic("Bad call to `exitScope` - attempt to exit top-most scope", .{});
    self.current_scope = self.getScopeMut(parent_scope_id);
}

pub fn findFromCurrentScope(self: *const Self, name: String) ?*const Variable {
    var cur_scope: *const Scope = self.current_scope;
    while (true) {
        if (self.findInScope(cur_scope, name)) |variable|
            return variable;

        const parent_scope_id = cur_scope.upper orelse return null;
        cur_scope = self.getScope(parent_scope_id);
    }
    return null;
}

fn findInScope(self: *const Self, scope: *const Scope, name: String) ?*const Variable {
    const vars = scope.variables orelse return null;

    var maybe_cur_var: ?Variable.Id = vars.head;
    while (maybe_cur_var) |cur_var_id| {
        const variable = self.getVar(cur_var_id);
        if (variable.name.eql(name)) return variable;
        maybe_cur_var = variable.next;
    }

    return null;
}

/// Get a const pointer to the scope by its ID.
/// Pointer may be invalidated if a new scope is added.
/// In such cases, the pointer should be de-referenced before adding a new scope.
pub fn getScope(self: *const Self, id: Scope.Id) *const Scope {
    return &self.scopes.items[@intFromEnum(id)];
}

/// Get a const pointer to the variable by its ID.
/// Pointer may be invalidated if a new variable or scope is added.
/// In such cases, the pointer should be de-referenced before adding a new variable.
pub fn getVar(self: *const Self, var_id: Variable.Id) *const Variable {
    return &self.variables.items[@intFromEnum(var_id)];
}

/// Same as `getVar`, but returns a non-const pointer.
pub fn getVarMut(self: *Self, var_id: Variable.Id) *Variable {
    return &self.variables.items[@intFromEnum(var_id)];
}

/// Same as `getScope`, but returns a non-const pointer.
pub fn getScopeMut(self: *Self, id: Scope.Id) *Scope {
    return &self.scopes.items[@intFromEnum(id)];
}

/// Append a new variable to the currently active scope.
/// If [kind] is `variable_binding`, then the variable is
/// added to the nearest function-scope instead.
pub fn addVariable(
    self: *Self,
    name: String,
    def_node: ast.Node.Index,
    kind: Variable.Kind,
) AllocError!void {
    // Find the scope where this variable should be added.
    // (Can't add to the current scope because of hoisting).
    const target_scope = blk: {
        if (kind == .lexical_binding) break :blk self.current_scope;

        // For "var" bindings, find the nearest
        // function scope.
        var scope = self.current_scope;
        while (scope.kind == .block) {
            if (scope.upper) |parent| {
                scope = self.getScopeMut(parent);
            } else {
                // we've reached the top-most scope
                std.debug.assert(scope.kind == .script or scope.kind == .module);
                break;
            }
        }

        break :blk scope;
    };

    const variable = Variable{ .name = name, .def_node = def_node, .kind = kind };
    const new_var_id: Variable.Id = @enumFromInt(self.variables.items.len);
    try self.variables.append(variable);

    if (target_scope.variables) |*vars| {
        // extend the tail of the scope by adding a new variable
        var tail = self.getVarMut(vars.tail);
        tail.next = new_var_id;
        vars.tail = new_var_id;
    } else {
        // The scope was empty.
        // The head and tail both now point this new var
        target_scope.variables = .{
            .head = new_var_id,
            .tail = new_var_id,
        };
    }
}

const t = std.testing;
test Self {
    var strings = try StringPool.init(t.allocator);
    defer strings.deinit();

    const a = try strings.getInsert("a");
    const b = try strings.getInsert("b");

    {

        //  let a = 1;
        //  // both a and b can be accessed here
        //  {
        //      var b = 1;
        //  }
        //

        var scopes = try Self.init(t.allocator, .module);
        defer scopes.deinit();

        try scopes.addVariable(a, ast.Node.Index.empty, .lexical_binding); // let a = 1
        try scopes.enterScope(.block, false); // {
        try scopes.addVariable(b, ast.Node.Index.empty, .variable_binding); // var b = 1

        // both 'a' and 'b' are accessible inside the block
        try t.expectEqualDeep(scopes.findFromCurrentScope(b).?.name, b);
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);

        scopes.exitScope(); // }

        // both a and b are accessible here
        try t.expectEqualDeep(scopes.findFromCurrentScope(b).?.name, b);
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);
    }

    {
        //  let a = 1;
        //  // both a and b can be accessed here
        //  {
        //      let b = 1;
        //  }
        //
        var scopes = try Self.init(t.allocator, .module);
        defer scopes.deinit();

        try scopes.addVariable(a, ast.Node.Index.empty, .lexical_binding); // let a = 1
        try scopes.enterScope(.block, false); // {
        try scopes.addVariable(b, ast.Node.Index.empty, .lexical_binding); // var b = 1

        // both 'a' and 'b' are accessible inside the block
        try t.expectEqualDeep(scopes.findFromCurrentScope(b).?.name, b);
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);

        scopes.exitScope(); // }

        // Only 'a' is accessible outside the block
        try t.expectEqualDeep(scopes.findFromCurrentScope(b), null);
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);
    }

    {
        // function f() {
        //  {
        //      var a;
        //  }
        //  "a is accessible here"
        // }

        var scopes = try Self.init(t.allocator, .module);
        defer scopes.deinit();

        try scopes.enterScope(.function, false); // function f() {
        try scopes.enterScope(.block, false); // // {
        try scopes.addVariable(a, ast.Node.Index.empty, .variable_binding); // var a;
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);
        scopes.exitScope(); // } exit block
        try t.expectEqualDeep(scopes.findFromCurrentScope(a).?.name, a);
        scopes.exitScope(); // } exit function

        // 'a' isn't visible outside the function
        try t.expectEqualDeep(scopes.findFromCurrentScope(a), null);
    }
}
