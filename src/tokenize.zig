const std = @import("std");
const offset = @import("offsets.zig");
const unicode_id = @import("unicode-id");

const types = @import("types.zig");
const Coordinate = types.Coordinate;
const Range = types.Range;

// TODO: UTF-8 support in tokens.
const Token = struct {
    /// Index into the token array.
    /// Open ended enum for type safety.
    const Index = enum(u32) { _ };

    const Tag = enum(u32) {
        identifier,
        number_literal,
        big_int_literal,
        string_literal,

        eof,
    };

    tag: Tag,
    /// Byte index into the source string
    start: u32,
    /// Size of the token's text in bytes
    len: u32,

    /// Return the token's text as a byte slice.
    pub fn toByteSlice(self: Token, source: []const u8) []const u8 {
        return source[self.start .. self.start + self.len];
    }

    /// (line, column) position for the start of this token.
    pub fn startCoord(self: Token, source: []const u8) Coordinate {
        return offset.byteIndexToCoordinate(source, self.start);
    }

    /// (line, column) position for the end of this token.
    pub fn endCoord(self: Token, source: []const u8) Range {
        return offset.byteIndexToCoordinate(source, self.start + self.len);
    }

    /// (line, column) range for the end of this token.
    pub fn toRange(self: Token, source: []const u8) Range {
        return Range{
            .start = self.startCoord(source),
            .end = self.endCoord(source),
        };
    }
};

const TokenizeError = error{
    UnexpectedEof,
    InvalidUtf8,
    UnexpectedByte,
};

const Tokenizer = struct {
    const Self = @This();
    /// Source string to tokenize.
    source: []const u8,
    /// Byte offset into `source`
    index: u32 = 0,
    /// Current line number (0 indexed).
    line: u32 = 0,
    /// Current column number (0 indexed)
    col: u32 = 0,

    pub fn init(source: []const u8) !Self {
        return Self{
            .source = source,
        };
    }

    /// Return the next token.
    fn next(self: *Self) TokenizeError!Token {
        const byte = self.peekByte() catch {
            return Token{
                .tag = Token.Tag.eof,
                .start = self.index,
                .len = 0,
            };
        };

        switch (byte) {
            ' ', '\t' => {
                self.skipWhiteSpaces();
                return self.next();
            },
            '\n' => {
                self.skipNewlines();
                return self.next();
            },
            else => {
                return try self.identifier() orelse
                    TokenizeError.UnexpectedByte;
            },
        }
    }

    // Skip all newlines and update the current line number.
    fn skipNewlines(self: *Self) void {
        while (!self.eof()) : (self.index += 1) {
            const ch = self.source[self.index];
            if (ch == '\n') {
                self.line += 1;
            } else if (ch == '\r') {
                if (self.index + 1 < self.source.len and
                    self.source[self.index + 1] == '\n')
                {
                    self.index += 1; // skip \r
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Parse a unicode escape sequence and return the codepoint along with the
    /// length of the sequence in bytes.
    fn parseUnicodeEscape(str: []const u8) ?struct { codepoint: u21, len: u32 } {
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

    fn canCodepointStartId(cp: u21) bool {
        return cp == '_' or cp == '$' or unicode_id.canStartId(cp);
    }

    fn canCodepointContinueId(cp: u21) bool {
        return cp == '$' or unicode_id.canContinueId(cp);
    }

    /// Advance the iterator by one or more code points,
    /// and check if the code point(s) form a valid identifier start.
    /// https://tc39.es/ecma262/#prod-IdentifierStartChar
    fn matchIdentifierStart(
        self: *Self,
        it: *std.unicode.Utf8Iterator,
    ) TokenizeError!bool {
        if (parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
            if (canCodepointStartId(parsed.codepoint)) {
                self.index += parsed.len;
                return true;
            } else {
                return false;
            }
        }

        const codepoint = it.nextCodepoint() orelse
            return TokenizeError.InvalidUtf8; // bad utf-8 input.
        return canCodepointStartId(codepoint);
    }

    /// Advance the iterator by one or more code points,
    /// and check if the code point(s) form a valid identifier part.
    /// https://tc39.es/ecma262/#prod-IdentifierPartChar
    fn matchIdentifierContt(it: *std.unicode.Utf8Iterator) TokenizeError!bool {
        if (parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
            if (canCodepointContinueId(parsed.codepoint)) {
                it.i += parsed.len;
                return true;
            } else {
                return false;
            }
        }

        const code_point = it.nextCodepoint() orelse
            return TokenizeError.InvalidUtf8;
        return canCodepointContinueId(code_point);
    }

    fn identifier(self: *Self) TokenizeError!?Token {
        const start = self.index;
        const str = self.source[start..];

        const view = std.unicode.Utf8View.initUnchecked(str);
        var it = view.iterator();

        const is_valid_start = matchIdentifierStart(self, &it) catch |err|
            return err;
        if (!is_valid_start) return null;

        const len = blk: {
            while (it.i < str.len) {
                const oldlen = it.i;
                const is_valid_contt = matchIdentifierContt(&it) catch |err|
                    return err;
                if (!is_valid_contt) break :blk oldlen;
            }

            break :blk it.i;
        };

        self.index += @intCast(len);
        return Token{
            .start = start,
            .len = @intCast(len),
            .tag = Token.Tag.identifier,
        };
    }

    fn remaining(self: *const Self) []const u8 {
        return self.source[self.index..];
    }

    fn isIdentifierChar(b: u8) bool {
        return std.ascii.isAlphanumeric(b) or b == '_';
    }

    fn eof(self: *Tokenizer) bool {
        return self.index >= self.source.len;
    }

    /// Return the next u8 from the source string
    fn nextByte(self: *Self) TokenizeError!u8 {
        const byte = try self.peekByte();
        self.index += 1;
        return byte;
    }

    fn peekByte(self: *Self) TokenizeError!u8 {
        if (self.eof()) return TokenizeError.UnexpectedEof;
        return self.source[self.index];
    }

    /// Skip all consecutive white space characters.
    fn skipWhiteSpaces(self: *Self) void {
        while (!self.eof() and
            std.ascii.isWhitespace(self.source[self.index])) : (self.index += 1)
        {}
    }
};

const t = std.testing;

fn testIdentifier(src: []const u8) !void {
    // first, test that token followed by EOF
    {
        var tokenizer = try Tokenizer.init(src);
        const token = try tokenizer.next();

        try std.testing.expectEqualDeep(Token{
            .tag = .identifier,
            .start = 0,
            .len = @intCast(src.len),
        }, token);
    }

    // then, that token wrapped by whitespace
    {
        const source = try std.mem.concat(
            t.allocator,
            u8,
            &[_][]const u8{ "\n ", src, " " },
        );

        defer t.allocator.free(source);
        var tokenizer = try Tokenizer.init(source);
        const token = try tokenizer.next();

        try std.testing.expectEqualDeep(Token{
            .tag = .identifier,
            .start = 2,
            .len = @intCast(src.len),
        }, token);
    }
}

test "identifier" {
    const valid = [_][]const u8{
        "$one",
        "$two$",
        "$123",
        "fooobar",
        "ಠ_ಠ",
        "\\u{105}bc",
        "\\u{105}\\u{5f}",
        "\\u{105}\\u005f",
    };

    for (valid) |id| {
        testIdentifier(id) catch |err| {
            std.debug.print("failed to parse {s} as an identifier\n", .{id});
            return err;
        };
    }
}
