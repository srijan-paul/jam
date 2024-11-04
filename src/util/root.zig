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
        const len = std.mem.indexOfScalar(u8, str, '}') orelse return null;
        if (len < 1) return null;
        for (str[3..len]) |ch| {
            if (!std.ascii.isHex(ch)) return null;
        }
        const code_point = std.fmt.parseInt(u24, str[3..len], 16) catch unreachable;
        if (code_point > 0x10FFFF) return null;
        return .{ .codepoint = @intCast(code_point), .len = @intCast(len + 1) };
    }

    var i: u32 = 2;
    while (i < str.len and std.ascii.isHex(str[i])) : (i += 1) {}
    if (i < 6) return null;

    const code_point = std.fmt.parseInt(u21, str[2..i], 16) catch
        return null;
    return .{ .codepoint = code_point, .len = i };
}

/// A UTF-8 code point.
pub const CodePoint = struct { len: u3, value: u21 };

/// Returns the UTF-8 codepoint in the string starting at the given index,
/// along with its length in bytes.
pub fn codePointAt(str: []const u8, i: usize) CodePoint {
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch unreachable;
    const codepoint = switch (len) {
        1 => str[i],
        2 => std.unicode.utf8Decode2(.{ str[i], str[i + 1] }),
        3 => std.unicode.utf8Decode3(.{ str[i], str[i + 1], str[i + 2] }),
        4 => std.unicode.utf8Decode4(.{ str[i], str[i + 1], str[i + 2], str[i + 3] }),
        else => unreachable,
    };
    return .{ .len = @intCast(len), .value = codepoint catch unreachable };
}

test {
    _ = offsets;
    _ = StringPool;
}
