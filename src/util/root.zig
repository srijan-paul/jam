pub const offsets = @import("./offsets.zig");
pub const types = @import("./types.zig");
pub const StringPool = @import("./string_intern.zig");
pub const utf8 = @import("./utf8.zig");
pub const DiagnosticsBuilder = @import("diagnostics.zig");

const std = @import("std");

/// Copy `value` to a the heap and return a pointer.
/// This function is mostly used for debugging and pretty-printing,
/// and should be avoided in runtime code.
// TODO: get rid of this
pub fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
