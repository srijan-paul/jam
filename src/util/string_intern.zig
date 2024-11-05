const std = @import("std");

const Self = @This();

pub const String = struct {
    start: u32,
    end: u32,
    pub fn eql(self: String, other: String) bool {
        return self.start == other.start and self.end == other.end;
    }
};

allocator: std.mem.Allocator,

/// A buffer containing all strings in the intern table.
/// All strings are `String`s that index into this buffer.
/// Only the `0..n_bytes_used` subslice of this buffer is valid.
chars: []u8,
/// The number of bytes used in `chars`.
n_bytes_used: u32 = 0,
/// Maps a string to its corresponding (start, end) span.
index_of_str: std.StringHashMap(String),

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
    return Self{
        .allocator = allocator,
        .index_of_str = std.StringHashMap(String).init(allocator),
        .chars = try allocator.alloc(u8, 256),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.chars);
    self.index_of_str.deinit();
}

fn ensureCapacity(self: *Self, n_bytes_wanted: u32) error{ OutOfMemory, Overflow }!void {
    const n_remaining_bytes = self.chars.len - self.n_bytes_used;
    if (n_remaining_bytes > n_bytes_wanted) return;

    const n_chars: u32 = @intCast(self.chars.len);
    const new_capacity = try std.math.ceilPowerOfTwo(u32, n_chars + n_bytes_wanted);
    self.chars = try self.allocator.realloc(self.chars, new_capacity);
}

/// If `string` exists in the intern table, return the associated span.
/// Otherwise, insert it and return the new span.
pub fn getInsert(self: *Self, string: []const u8) error{ OutOfMemory, Overflow }!String {
    const gop = try self.index_of_str.getOrPut(string);
    if (gop.found_existing) {
        return gop.value_ptr.*;
    }

    try self.ensureCapacity(@intCast(string.len));

    const string_start_index = self.n_bytes_used;
    const dst = self.chars[self.n_bytes_used .. self.n_bytes_used + string.len];
    @memcpy(dst, string);

    self.n_bytes_used += @intCast(string.len);
    const string_end_index = self.n_bytes_used;

    const inserted_string = String{
        .start = string_start_index,
        .end = string_end_index,
    };

    gop.value_ptr.* = inserted_string;
    return inserted_string;
}

pub fn toByteSlice(self: *const Self, string: String) []const u8 {
    return self.chars[string.start..string.end];
}

const t = std.testing;
test {
    var table = try Self.init(t.allocator);
    defer table.deinit();

    const span = try table.getInsert("hello");
    try t.expectEqual(0, span.start);
    try t.expectEqual(5, span.end);

    try t.expectEqual(5, table.n_bytes_used);
    try t.expectEqual(256, table.chars.len);

    const span2 = try table.getInsert("world");
    try t.expectEqual(5, span2.start);
    try t.expectEqual(10, span2.end);

    const span3 = try table.getInsert("hello");
    try t.expectEqualDeep(span, span3);
}
