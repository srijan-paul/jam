pub const offsets = @import("./offsets.zig");
pub const types = @import("./types.zig");
pub const string_pool = @import("./string_intern.zig");

test {
    _ = offsets;
    _ = string_pool;
}
