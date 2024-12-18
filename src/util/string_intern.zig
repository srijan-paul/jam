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
/// Stores the raw strings in the order they were inserted.
/// We need this because `StringHashMap` does not own the strings,
/// but expect the caller to keep the strings alive for the lifetime of the map.
/// Whenever a new string is interned, we append it to this list.
raw_strings: std.ArrayList([]const u8),

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
    return Self{
        .allocator = allocator,
        .index_of_str = std.StringHashMap(String).init(allocator),
        .chars = try allocator.alloc(u8, 256),
        .raw_strings = try std.ArrayList([]const u8).initCapacity(allocator, 128),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.chars);
    self.index_of_str.deinit();

    for (self.raw_strings.items) |s|
        self.allocator.free(s);
    self.raw_strings.deinit();
}

fn ensureCapacity(self: *Self, n_bytes_wanted: u32) error{ OutOfMemory, Overflow }!void {
    const n_remaining_bytes = self.chars.len - self.n_bytes_used;
    if (n_remaining_bytes > n_bytes_wanted) return;

    const n_chars: u32 = @intCast(self.chars.len);
    const new_capacity = try std.math.ceilPowerOfTwo(u32, n_chars + n_bytes_wanted);
    self.chars = try self.allocator.realloc(self.chars, new_capacity);
}

/// Copy [key] into the internal buffer, and return the span
fn insertString(self: *Self, key: []const u8) error{ OutOfMemory, Overflow }!String {
    try self.ensureCapacity(@intCast(key.len));
    const string_start_index = self.n_bytes_used;

    const dst = self.chars[self.n_bytes_used .. self.n_bytes_used + key.len];
    @memcpy(dst, key);

    self.n_bytes_used += @intCast(key.len);
    const string_end_index = self.n_bytes_used;

    return String{
        .start = string_start_index,
        .end = string_end_index,
    };
}

/// Same as `getOrInsert`, but does not copy the key.
/// Instead, the caller is responsible for keeping the key alive for as long as the intern table is alive.
/// The key must not be modified after it is inserted into the intern table.
pub fn getOrInsertNoOwn(self: *Self, string: []const u8) error{ OutOfMemory, Overflow }!String {
    const gop = try self.index_of_str.getOrPut(string);
    if (gop.found_existing) return gop.value_ptr.*;

    const span = try self.insertString(string);
    gop.value_ptr.* = span;
    return span;
}

/// If `string` exists in the intern table, return the associated span.
/// Otherwise, insert it and return the new span.
pub fn getOrInsert(self: *Self, string: []const u8) error{ OutOfMemory, Overflow }!String {
    const gop = try self.index_of_str.getOrPut(string);
    if (gop.found_existing) return gop.value_ptr.*;

    // If the key isn't found, copy the key that was passed in.
    const key = try self.allocator.dupe(u8, string);
    try self.raw_strings.append(key);
    gop.key_ptr.* = key;

    const span = try self.insertString(key);
    gop.value_ptr.* = span;

    return span;
}

pub fn toByteSlice(self: *const Self, string: String) []const u8 {
    return self.chars[string.start..string.end];
}

const t = std.testing;
test {
    var table = try Self.init(t.allocator);
    defer table.deinit();

    const span = try table.getOrInsert("hello");
    try t.expectEqual(0, span.start);
    try t.expectEqual(5, span.end);

    try t.expectEqual(5, table.n_bytes_used);
    try t.expectEqual(256, table.chars.len);

    const span2 = try table.getOrInsert("world");
    try t.expectEqual(5, span2.start);
    try t.expectEqual(10, span2.end);

    const span3 = try table.getOrInsert("hello");
    try t.expectEqualDeep(span, span3);
}
