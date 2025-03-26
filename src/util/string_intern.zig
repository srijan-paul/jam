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

/// A vector containing all bytes of each string in the intern table.
str_bytes: std.ArrayListUnmanaged(u8),
/// Set of strings that have been interned
interned_strs_set: std.AutoHashMapUnmanaged(String, void),

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
    var self = Self{
        .allocator = allocator,
        .str_bytes = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 256),
        .interned_strs_set = std.AutoHashMapUnmanaged(String, void).empty,
    };

    try self.interned_strs_set.ensureTotalCapacity(allocator, 128);
    return self;
}

pub fn deinit(self: *Self) void {
    self.str_bytes.deinit(self.allocator);
    self.interned_strs_set.deinit(self.allocator);
}

/// If `string` exists in the intern table, return the associated span.
/// Otherwise, insert it and return the new span.
pub fn getOrInsert(self: *Self, string: []const u8) error{ OutOfMemory, Overflow }!String {
    // Helper struct that defines functions for hasing and comparing strings.
    // Used by the `AutoHashMap`'s `getOrPutAdapted` function.
    const KeyCtx = struct {
        bytes: []const u8,

        pub fn hash(_: @This(), key: []const u8) u64 {
            return std.hash.Wyhash.hash(0, key);
        }

        pub fn eql(this: @This(), bytes: []const u8, handle: String) bool {
            return std.mem.eql(u8, bytes, this.bytes[handle.start..handle.end]);
        }
    };

    const ctx = KeyCtx{ .bytes = self.str_bytes.items };
    // Look for an existing string with the same bytes
    const gop = try self.interned_strs_set.getOrPutAdapted(self.allocator, string, ctx);
    if (gop.found_existing) return gop.key_ptr.*;

    // If the key isn't found, copy the key that was passed in.
    const start: u32 = @intCast(self.str_bytes.items.len);
    // TODO: we can avoid this `appendSlice` call for short strings
    // if we store small strings inline in the `Handle`.
    // Look into SSO (small string optimization) for ideas.
    // See: libc++'s std::string and some rust implementations.
    self.str_bytes.appendSlice(self.allocator, string) catch {
        self.interned_strs_set.removeByPtr(gop.key_ptr);
        return error.OutOfMemory;
    };

    const end: u32 = @intCast(self.str_bytes.items.len);
    const handle = String{ .start = start, .end = end };
    gop.key_ptr.* = handle;
    return handle;
}

pub fn toByteSlice(self: *const Self, string: String) []const u8 {
    return self.str_bytes.items[string.start..string.end];
}

const t = std.testing;
test {
    var table = try Self.init(t.allocator);
    defer table.deinit();

    const span = try table.getOrInsert("hello");
    try t.expectEqual(0, span.start);
    try t.expectEqual(5, span.end);

    const span2 = try table.getOrInsert("world");
    try t.expectEqual(5, span2.start);
    try t.expectEqual(10, span2.end);

    const span3 = try table.getOrInsert("hello");
    try t.expectEqualDeep(span, span3);
}
