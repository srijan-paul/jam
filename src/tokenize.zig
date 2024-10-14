const std = @import("std");
const offset = @import("offsets.zig");
const unicode_id = @import("unicode-id");
const Token = @import("token.zig").Token;

const types = @import("types.zig");
const Coordinate = types.Coordinate;
const Range = types.Range;

const TokenizeError = error{
    UnexpectedEof,
    InvalidUtf8,
    UnexpectedByte,
    InvalidNumericLiteral,
    BadPunctuator,
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
        if (!std.unicode.utf8ValidateSlice(source)) {
            return TokenizeError.InvalidUtf8;
        }

        return Self{
            .source = source,
        };
    }

    /// Return the next token.
    fn next(self: *Self) TokenizeError!Token {
        const byte = self.peekByte() orelse {
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
            '}',
            '{',
            '[',
            ']',
            '(',
            ')',
            ',',
            ';',
            ':',
            '>',
            '<',
            '-',
            '+',
            '*',
            '/',
            '%',
            '^',
            '=',
            '!',
            '|',
            '&',
            '?',
            => {
                return try self.punctuator();
            },
            '0'...'9' => return self.numericLiteral(),
            '.' => {
                if (self.numericLiteral()) |tok| {
                    return tok;
                } else |_| {
                    return self.dot();
                }
            },
            else => {
                return try self.identifier() orelse
                    TokenizeError.UnexpectedByte;
            },
        }
    }

    fn dot(self: *Self) Token {
        const start = self.index;

        var len: u32 = 1;
        var tag: Token.Tag = .@".";
        if (std.mem.startsWith(u8, self.source[self.index..], "...")) {
            len = 3;
            tag = .@"...";
        }

        self.index += len;
        return Token{
            .tag = tag,
            .start = start,
            .len = len,
        };
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

    fn punctuator(self: *Self) !Token {
        const start = self.index;
        const str = self.source[start..];

        var len: u32 = 0;
        const tag: Token.Tag = blk: {
            len = 1;
            // huge switch case.
            // this is basically a hand-written trie.
            switch (str[0]) {
                '.' => {
                    if (str.len <= 3 and str[1] == '.' and str[2] == '.') {
                        len += 2;
                        break :blk .@"...";
                    }
                    break :blk .@".";
                },
                '+' => {
                    if (str.len > 1) {
                        if (str[1] == '+') {
                            len += 1;
                            break :blk .@"++";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"+=";
                        }
                    }
                    break :blk .@"+";
                },
                '-' => {
                    if (str.len > 1) {
                        if (str[1] == '-') {
                            len += 1;
                            break :blk .@"--";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"-=";
                        }
                    }
                    break :blk .@"-";
                },

                '{' => break :blk .@"{",
                '}' => break :blk .@"}",
                '(' => break :blk .@"(",
                ')' => break :blk .@")",
                '[' => break :blk .@"]",
                ':' => break :blk .@":",
                ';' => break :blk .@";",
                '~' => break :blk .@"~",
                ',' => break :blk .@",",

                '?' => {
                    if (str.len > 1 and str[1] == '?') {
                        len += 1;
                        if (str.len > 2 and str[2] == '=') {
                            len += 1;
                            break :blk .@"??=";
                        }
                        break :blk .@"??";
                    }
                    break :blk .@"?";
                },

                '^' => {
                    if (str.len > 1 and str[1] == '=') {
                        len += 1;
                        break :blk .@"^=";
                    }

                    break :blk .@"^";
                },
                '|' => {
                    if (str.len > 1) {
                        if (str[1] == '|') {
                            len += 1;
                            if (str.len > 2 and str[2] == '=') {
                                len += 1;
                                break :blk .@"||=";
                            }
                            break :blk .@"||";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"|=";
                        }
                    }
                    break :blk .@"|";
                },
                '&' => {
                    if (str.len > 1) {
                        if (str[1] == '&') {
                            len += 1;
                            if (str.len > 2 and str[2] == '=') {
                                len += 1;
                                break :blk .@"&&=";
                            }
                            break :blk .@"&&";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"&=";
                        }
                    }
                    break :blk .@"&";
                },
                '*' => {
                    if (str.len > 1) {
                        if (str[1] == '*') {
                            len += 1;
                            if (str.len > 2 and str[2] == '=') {
                                len += 1;
                                break :blk .@"**=";
                            }
                            break :blk .@"**";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"*=";
                        }
                    }
                    break :blk .@"*";
                },
                '/' => {
                    if (str.len > 1) {
                        if (str[1] == '/') {
                            len += 1;
                            if (str.len > 2 and str[2] == '=') {
                                len += 1;
                                break :blk .@"//=";
                            }
                            break :blk .@"//";
                        } else if (str[1] == '=') {
                            len += 1;
                            break :blk .@"/=";
                        }
                    }
                    break :blk .@"/";
                },

                '%' => {
                    if (str.len > 1 and str[1] == '=') {
                        len += 1;
                        break :blk .@"%=";
                    }
                    break :blk .@"%";
                },

                '!' => {
                    if (str.len > 1 and str[1] == '=') {
                        len += 1;
                        if (str.len > 2 and str[2] == '=') {
                            len += 1;
                            break :blk .@"!==";
                        }

                        break :blk .@"!=";
                    }
                    break :blk .@"!";
                },
                '=' => {
                    if (str.len > 1) {
                        if (str[1] == '=') {
                            len += 1;
                            if (str.len > 2 and str[2] == '=') {
                                len += 1;
                                break :blk .@"===";
                            }
                            break :blk .@"==";
                        } else if (str[1] == '>') {
                            len += 1;
                            break :blk .@"=>";
                        }
                    }

                    break :blk .@"=";
                },

                '<' => {
                    if (str.len <= 1) break :blk .@"<";

                    if (str[1] == '<') {
                        if (str.len > 2 and str[2] == '=') {
                            len += 2;
                            break :blk .@"<<=";
                        }
                        len += 1;
                        break :blk .@"<<";
                    }

                    if (str[1] == '=') {
                        len += 1;
                        break :blk .@"<=";
                    }

                    break :blk .@"<";
                },

                '>' => {
                    if (str.len > 1) {
                        switch (str[1]) {
                            '=' => {
                                len += 1;
                                break :blk .@">=";
                            },
                            '>' => {
                                len += 1;
                                if (str.len > 2) {
                                    if (str[2] == '=') {
                                        len += 1;
                                        break :blk .@">>=";
                                    } else if (str[2] == '>') {
                                        len += 1;
                                        if (str.len > 3 and str[3] == '=') {
                                            len += 1;
                                            break :blk .@">>>=";
                                        }
                                        break :blk .@">>>";
                                    }
                                }

                                break :blk .@">>";
                            },
                            else => break :blk .@">",
                        }
                    }

                    break :blk .@">";
                },

                else => return TokenizeError.BadPunctuator,
            }
        };

        return Token{
            .start = start,
            .len = len,
            .tag = tag,
        };
    }

    /// https://tc39.es/ecma262/#prod-DecimalIntegerLiteral
    fn matchDecimalIntegerLiteral(str: []const u8) ?u32 {
        if (str[0] == '0') return 1;

        var i: u32 = 0;
        // first character must be a non-zero digit.
        if (str[i] < '1' and str[i] > '9') return null;
        i += 1; // eat first char

        if (i < str.len and str[i] == '_')
            i += 1; // eat '_' after first char

        while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {
            if (i + 1 < str.len and str[i + 1] == '_')
                i += 1;
        }

        return i;
    }

    /// https://tc39.es/ecma262/#prod-DecimalLiteral
    fn matchDecimalPart(str: []const u8) ?u32 {
        if (str[0] != '.') return null;

        var i: u32 = 1;
        while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {}
        if (i == 1) return null;
        return i;
    }

    /// https://tc39.es/ecma262/#prod-NumericLiteral
    fn numericLiteral(self: *Self) !Token {
        const start = self.index;
        const str = self.source[start..];

        const len: u32 = blk: {
            if (str[0] == '.') {
                break :blk matchDecimalPart(str) orelse
                    return TokenizeError.InvalidNumericLiteral;
            }

            var decimal_len = matchDecimalIntegerLiteral(str) orelse
                return TokenizeError.InvalidNumericLiteral;
            if (decimal_len < str.len) {
                decimal_len += matchDecimalPart(str[decimal_len..]) orelse 0;
            }
            break :blk decimal_len;
        };

        self.index += len;

        // number must not be immediately followed by identifier
        if (self.peekByte()) |ch| {
            if (canCodepointStartId(ch)) {
                return TokenizeError.InvalidNumericLiteral;
            }
        }

        return Token{
            .tag = .numeric_literal,
            .start = start,
            .len = len,
        };
    }

    fn remaining(self: *const Self) []const u8 {
        return self.source[self.index..];
    }

    fn eof(self: *Tokenizer) bool {
        return self.index >= self.source.len;
    }

    /// Return the next u8 from the source string
    fn nextByte(self: *Self) TokenizeError!u8 {
        const byte = self.peekByte() orelse
            return TokenizeError.UnexpectedEof;
        self.index += 1;
        return byte;
    }

    fn peekByte(self: *Self) ?u8 {
        if (self.eof()) return null;
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

fn testToken(src: []const u8, tag: Token.Tag) !void {
    // first, test that token followed by EOF
    {
        var tokenizer = try Tokenizer.init(src);
        const token = try tokenizer.next();

        try std.testing.expectEqualDeep(Token{
            .tag = tag,
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
            .tag = tag,
            .start = 2,
            .len = @intCast(src.len),
        }, token);
    }
}

test "identifier" {
    const identifiers = [_]struct { []const u8, Token.Tag }{
        .{ "$one", .identifier },
        .{ "$two$", .identifier },
        .{ "$123", .identifier },
        .{ "fooobar", .identifier },
        .{ "ಠ_ಠ", .identifier },
        .{ "\\u{105}bc", .identifier },
        .{ "\\u{105}\\u{5f}", .identifier },
        .{ "\\u{105}\\u005f", .identifier },
        .{ "0", .numeric_literal },
        .{ "120", .numeric_literal },
        .{ "1_000_000", .numeric_literal },
        .{ "1_00_0_000", .numeric_literal },
        .{ "123", .numeric_literal },
        .{ "1.5", .numeric_literal },
        .{ "1.523", .numeric_literal },
        .{ "1_000_000.523", .numeric_literal },
        .{ ".1", .numeric_literal },
        .{ ".33", .numeric_literal },
        .{ "<<", .@"<<" },
        .{ "<", .@"<" },
        .{ ">>", .@">>" },
        .{ ">", .@">" },
        .{ ">>=", .@">>=" },
        .{ ">>>", .@">>>" },
        .{ ".", .@"." },
        .{ "...", .@"..." },
        .{ "=", .@"=" },
        .{ "=>", .@"=>" },
        .{ "==", .@"==" },
        .{ "===", .@"===" },
        .{ "!", .@"!" },
        .{ "!=", .@"!=" },
        .{ "!==", .@"!==" },
        .{ "|", .@"|" },
        .{ "||", .@"||" },
        .{ "&", .@"&" },
        .{ "&&", .@"&&" },
        .{ "++", .@"++" },
        .{ "--", .@"--" },
        .{ "-", .@"-" },
        .{ "+", .@"+" },
        .{ "%", .@"%" },
        .{ "%=", .@"%=" },
        .{ "/", .@"/" },
        .{ "//", .@"//" },
        .{ "/=", .@"/=" },
        .{ "//=", .@"//=" },
        .{ "??=", .@"??=" },
        .{ "?", .@"?" },
        .{ "??", .@"??" },
        .{ ",", .@"," },
    };

    for (identifiers) |case| {
        const text, const tag = case;
        testToken(text, tag) catch |err| {
            std.debug.print("failed to parse {s} as {s}\n", .{ text, @tagName(tag) });
            return err;
        };
    }
}
