const std = @import("std");
const offsets = @import("./offsets");
const types = @import("./types.zig");

const Self = @This();

const Diagnostic = struct {
    message: []const u8,
    coord: types.Coordinate,
};

allocator: std.mem.Allocator,
diagnostics: std.ArrayList(Diagnostic),

pub fn init(allocator: std.mem.Allocator) Self {
    const diagnostics: std.ArrayList(Diagnostic) = .{};
    return Self{
        .allocator = allocator,
        .diagnostics = diagnostics,
    };
}

pub fn deinit(self: *Self) void {
    self.diagnostics.deinit(self.allocator);
}

/// Get the list of diagnostics.
pub fn items(self: *const Self) []Diagnostic {
    return self.diagnostics.items;
}

/// Push an error essage to the list of diagnostics.
pub fn emit(
    self: *Self,
    coord: types.Coordinate,
    comptime fmt: []const u8,
    fmt_args: anytype,
) std.mem.Allocator.Error!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostics.append(self.allocator, Diagnostic{
        .coord = coord,
        .message = message,
    });
}
