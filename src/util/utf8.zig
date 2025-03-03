const std = @import("std");

const Utf8CodePoint = struct {
    codepoint: u21,
    len: u32,
};

/// Parse a unicode escape sequence and return the codepoint along with the
/// length of the sequence in bytes.
pub fn parseUnicodeEscape(str: []const u8) ?Utf8CodePoint {
    if (str.len < 3 or !std.mem.startsWith(u8, str, "\\u")) {
        return null;
    }

    if (str[2] == '{') {
        // TODO(@injuly): this can be optimized to not use 'indexOfScalar'
        const len = std.mem.indexOfScalar(u8, str, '}') orelse return null;
        if (len < 1) return null;
        for (str[3..len]) |ch| {
            if (!std.ascii.isHex(ch)) return null;
        }
        const code_point = std.fmt.parseInt(u24, str[3..len], 16) catch return null;
        if (code_point > 0x10FFFF) return null;
        return .{ .codepoint = @intCast(code_point), .len = @intCast(len + 1) };
    }

    var i: u32 = 2;
    // TODO(@injuly): this part can also be optimized to avoid `parseInt`
    while (i < str.len and i < 6 and
        std.ascii.isHex(str[i])) : (i += 1)
    {}
    if (i < 6) return null;

    const code_point = std.fmt.parseInt(u21, str[2..i], 16) catch
        return null;
    return .{ .codepoint = code_point, .len = i };
}

pub fn parseOctalEscape(str: []const u8) ?Utf8CodePoint {
    if (str.len < 2 or str[0] != '\\') return null;

    var i: u32 = 1; // skip /
    var escape_value: u21 = 0;
    while (i < str.len) {
        const ch = str[i];
        if (!('0' <= ch and ch <= '9')) {
            i += 1;
            break;
        }

        escape_value = escape_value * 8 + ch - '0';
        i += 1;
    }

    return .{ .codepoint = escape_value, .len = i };
}

test parseUnicodeEscape {
    try std.testing.expectEqual(parseOctalEscape("\\077").?.codepoint, @as(u8, '?'));
    try std.testing.expectEqual(parseOctalEscape("\\012").?.codepoint, @as(u8, '\n'));
}

/// A UTF-8 code point.
pub const CodePoint = struct { len: u3, value: u21 };

/// Returns the UTF-8 code point in the string starting at the given index,
/// along with its length in bytes.
/// TODO: get rid of the `unreachable`s and add an error annotation to the return type.
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

/// Returns `true` if the given string is ASCII-only.
/// Iterates over the string one byte at a time.
fn isAsciiOnlySimple(str: []const u8) bool {
    for (str) |ch| {
        if (!std.ascii.isAscii(ch)) return false;
    }
    return true;
}

/// Returns `true` if the entire string is ASCII characters only.
pub fn isAsciiOnly(str: []const u8) bool {
    if (std.simd.suggestVectorLength(u8)) |chunk_size| {
        if (chunk_size < str.len) return isAsciiOnlySimple(str);

        // On CPUs that support it, vectorize the check.
        // Iterate over the string in chunks of size N, and exit with `false` if
        // there is at-least one chunk that contains a non-ASCII character (< 128).
        const remainder_len = str.len % chunk_size;
        var i: usize = 0;
        const chunk_all_128s: @Vector(chunk_size, u8) = @splat(128);
        while (i < (str.len - remainder_len)) : (i += chunk_size) {
            const chunk: @Vector(chunk_size, u8) = str[i..][0..chunk_size].*;
            if (@reduce(.Or, chunk > chunk_all_128s)) return false;
        }

        // either no bytes remain, or the remaining bytes are all ASCII.
        return remainder_len == 0 or isAsciiOnlySimple(str[i..remainder_len]);
    }

    return isAsciiOnlySimple(str);
}

test {
    const str = "hello, world!";
    const str2 = "hello, world!🌍";

    const long_str =
        \\ Curse you Perry he Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry he Platypus.
        \\ Curse you Perry the latypus.
        \\ Curse you Perry the Platypus.
        \\ Curs you Perry the latypus.
        \\ Cure yerry the Platypus.
        \\ Cure you Perry the Platypus.
        \\ use you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ urse you Perry the Platypus.
        \\ urse you Perrhe Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curs you Per hPlatypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curs you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Cursyou Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curs y Perry the Platypus.
        \\ Curse yo Perry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you Perry the Platypus.
        \\ Curse you erry the Platypus.
        \\ Curse you Perry the Platpus.
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus.
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
        \\ Curse you Perry the Platypus
    ;

    const long_str_not_ascii = @embedFile("./utf8.zig");

    try std.testing.expect(isAsciiOnly(str));
    try std.testing.expect(isAsciiOnly(long_str));
    try std.testing.expect(!isAsciiOnly(long_str_not_ascii));
    try std.testing.expect(!isAsciiOnly(str2));
}
