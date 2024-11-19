// Some rudimentary scope analysis needed for parsing.
const std = @import("std");
const util = @import("util");
const ast = @import("ast.zig");

const StringPool = util.StringPool;
const String = StringPool.String;

const Self = @This();

const Variable = struct {
    pub const Index = enum(u32) { _ };
    /// Name of the variable, after processing any escapes codes
    /// in the identifier.
    name: String,
    /// AST node that declared this variable.
    /// Usually a parameter, or a variable declarator.
    def_node: ast.Node.Index,
};

const Scope = struct {
    pub const Kind = enum {
        function,
        block,
        module,
        script,
    };

    start: Variable.Index,
    kind: Kind,
    /// `true` if this scope is in strict mode.
    is_strict: bool,
    /// Returns a slice of the variables in this scope.
    pub fn asSlice(self: *const Scope, all_variables: []Variable) []Variable {
        return all_variables[@intFromEnum(self.start)..];
    }
};

al: std.mem.Allocator,

/// All variables declared in the program are stored in this flat array.
/// A "scope" is just a view into this array.
variables: std.ArrayList(Variable),
scope_stack: std.ArrayList(Scope),
current_scope: *Scope,

pub fn init(al: std.mem.Allocator, root_scope_kind: Scope.Kind) std.mem.Allocator.Error!Self {
    const current_scope = Scope{
        .start = @enumFromInt(0),
        .kind = root_scope_kind,
        .is_strict = root_scope_kind == .module,
    };

    var self = Self{
        .al = al,
        .current_scope = undefined, // initialized below
        .variables = try std.ArrayList(Variable).initCapacity(al, 16),
        .scope_stack = try std.ArrayList(Scope).initCapacity(al, 4),
    };

    try self.scope_stack.append(current_scope);
    self.current_scope = &self.scope_stack.items[0];
    return self;
}

pub fn deinit(self: *Self) void {
    self.variables.deinit();
    self.scope_stack.deinit();
}

pub fn enterScope(self: *Self, kind: Scope.Kind, is_strict: bool) std.mem.Allocator.Error!void {
    const scope = Scope{
        .start = @enumFromInt(self.variables.items.len),
        .kind = kind,
        .is_strict = is_strict,
    };

    try self.scope_stack.append(scope);
    self.current_scope = self.scope_stack.items[self.scope_stack.items.len - 1];
}

pub fn exitScope(self: *Self) void {
    std.debug.assert(self.scope_stack.items.len > 1);
    const cur_scope = self.scope_stack.pop();
    self.variables.items.len = @intFromEnum(cur_scope.start);
}

pub fn findInCurrentScope(self: *Self, name: String) ?*const Variable {
    const vars: []Variable = self.current_scope.asSlice(self.variables.items);
    for (vars) |*variable| {
        if (variable.name.eql(name)) {
            return variable;
        }
    }

    return null;
}
