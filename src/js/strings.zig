// Helper for fast string interning and comparison.
const std = @import("std");
const Tokenizer = @import("./tokenize.zig");
const Token = @import("./token.zig").Token;
const util = @import("util");

const StringPool = util.StringPool;
const String = StringPool.String;

const Self = @This();

allocator: std.mem.Allocator,

/// Program source code.
source: []const u8,
/// Reusable buffer for temporary string storage.
buf: []u8,
/// Intern pool for strings.
string_pool: *StringPool,

pub fn init(
    allocator: std.mem.Allocator,
    source: []const u8,
    string_pool: *StringPool,
) std.mem.Allocator.Error!Self {
    return Self{
        .allocator = allocator,
        .source = source,
        .string_pool = string_pool,
        .buf = try allocator.alloc(u8, 128),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.buf);
}

/// Parse a token as a string, by resolving all escape codes.
pub fn stringValue(self: *Self, token: *const Token) error{ OutOfMemory, Overflow }!String {
    std.debug.assert(token.tag.isIdentifier() or token.tag.isKeyword());
    const str = token.toByteSlice(self.source);

    // If the string is entirely ASCII and has no escape codes,
    // we can just return the slice after interning it as is.
    // Non-ASCII identifiers are tagged with ".non_ascii_identifier"
    if (token.tag == .identifier or token.tag.isKeyword()) {
        @branchHint(.likely);
        return try self.string_pool.getOrInsert(str);
    }

    if (str.len > self.buf.len) {
        self.buf = try self.allocator.realloc(self.buf, str.len);
    }

    var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 0 };
    var strlen: usize = 0; // byte length of `str` after resolving escape codes.

    while (iter.i < str.len) {
        if (str[iter.i] == '\\') {
            const parsed_cp = util.utf8.parseUnicodeEscape(str[iter.i..]) orelse
                unreachable; // SAFETY: validated during tokenization.
            const cp = parsed_cp.codepoint;
            var cp_slice: [4]u8 = undefined;
            const cp_len = std.unicode.utf8Encode(cp, &cp_slice) catch
                unreachable; // SAFETY: validated during tokenization
            @memcpy(self.buf[strlen .. strlen + cp_len], cp_slice[0..cp_len]);
            strlen += cp_len;
            iter.i += parsed_cp.len;
            continue;
        }

        const cp_slice = iter.nextCodepointSlice() orelse
            unreachable; // SAFETY: already validated UTF-8 during tokenization
        const cplen = cp_slice.len;
        @memcpy(self.buf[strlen .. strlen + cplen], cp_slice);
        strlen += cplen;
    }

    const string = try self.string_pool.getOrInsert(self.buf[0..strlen]);
    return string;
}

pub fn toByteSlice(self: *Self, string: String) []const u8 {
    return self.string_pool.toByteSlice(string);
}

const t = std.testing;

fn testStringValue(id_str: []const u8, expected: []const u8) !void {
    var tokenizer = try Tokenizer.init(id_str, .{});
    const id_token = try tokenizer.next();

    var string_pool = try StringPool.init(t.allocator);
    defer string_pool.deinit();

    var strings = try Self.init(t.allocator, id_str, &string_pool);
    defer strings.deinit();

    const id_string = try strings.stringValue(&id_token);
    try t.expectEqualStrings(expected, strings.toByteSlice(id_string));
}

test stringValue {
    try testStringValue("identifier", "identifier");
    try testStringValue("id3entif$er", "id3entif$er");
    try testStringValue("ಠ_ಠ", "ಠ_ಠ");
    try testStringValue("\\u{0061}bc", "abc");
    try testStringValue("ev\\u{0061}l", "eval");
    try testStringValue("\\u{0061}\\u{5f}", "a_");
    try testStringValue("\\u{61}\\u005f", "a_");
}
