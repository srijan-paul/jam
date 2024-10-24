pub const offsets = @import("./offsets.zig");
pub const types = @import("./types.zig");
pub const StringPool = @import("./string_intern.zig");

const std = @import("std");

/// Parse a unicode escape sequence and return the codepoint along with the
/// length of the sequence in bytes.
pub fn parseUnicodeEscape(str: []const u8) ?struct { codepoint: u21, len: u32 } {
    if (str.len < 3 or !std.mem.startsWith(u8, str, "\\u")) {
        return null;
    }

    if (str[2] == '{') {
        const len = std.mem.indexOfScalar(u8, str[3..], '}') orelse return null;
        if (len < 1 or len > 6) return null;
        for (str[3..][0..len]) |ch| {
            if (!std.ascii.isHex(ch)) return null;
        }
        const code_point = std.fmt.parseInt(u24, str[3..][0..len], 16) catch unreachable;
        if (code_point > 0x10FFFF) return null;
        return .{ .codepoint = @intCast(code_point), .len = @intCast(len + 4) };
    }

    if (str.len < 6) return null;
    for (str[2..6]) |ch| {
        if (!std.ascii.isHex(ch)) return null;
    }
    const code_point = std.fmt.parseInt(u16, str[2..6], 16) catch unreachable;
    return .{ .codepoint = code_point, .len = 6 };
}

test {
    _ = offsets;
    _ = StringPool;
}
