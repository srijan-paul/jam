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
    BadEscapeSequence,
    NonTerminatedString,
    InvalidTokenizerState,
};

// zig fmt: off
const all_keywords = [_][]const u8{
  "await", "break",  "case",  "catch",
  "class", "const",  "continue", "debugger",
  "default",  "delete", "do", "else", "enum",
  "export", "extends", "false", "finally",
  "for", "function", "if", "import", "in",
  "instanceof", "new", "null", "return",
  "super", "switch", "this", "throw", "true",
  "try", "typeof", "var", "void", "while",
  "with", "yield"
};
// zig fmt: on

const all_kw_tags: [all_keywords.len]Token.Tag = makeKwTagArray();
fn makeKwTagArray() [all_keywords.len]Token.Tag {
    @setEvalBranchQuota(10_000);

    var tags: [all_keywords.len]Token.Tag = undefined;
    const enum_tags = std.meta.tags(Token.Tag);
    for (0.., all_keywords) |i, kw| {
        for (enum_tags) |tag| {
            const tagname = @tagName(tag);
            if (tagname.len < 3) continue;
            if (std.mem.eql(u8, tagname[3..], kw)) {
                tags[i] = tag;
                break;
            }
        }
    }

    return tags;
}

pub const Tokenizer = struct {
    pub const Error = TokenizeError;

    const Self = @This();
    /// Source string to tokenize.
    source: []const u8,
    /// Byte offset into `source`
    index: u32 = 0,
    /// Current line number (0 indexed).
    line: u32 = 0,
    /// Current column number (0 indexed)
    col: u32 = 0,

    next_token: ?Token = null,

    pub fn init(source: []const u8) TokenizeError!Self {
        if (!std.unicode.utf8ValidateSlice(source)) {
            return TokenizeError.InvalidUtf8;
        }

        var self = Self{ .source = source };
        self.next_token = try self.eatToken();
        return self;
    }

    /// Return the next token.
    pub fn next(self: *Self) TokenizeError!Token {
        if (self.next_token) |token| {
            self.next_token = try self.eatToken();
            return token;
        }
        return TokenizeError.InvalidTokenizerState;
    }

    /// Return the next token without consuming it.
    /// self.next() will return the same token.
    pub fn peek(self: *Self) ?Token {
        return self.next_token;
    }

    fn eatToken(self: *Self) TokenizeError!Token {
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
                return self.eatToken();
            },
            '\n' => {
                self.skipNewlines();
                return self.eatToken();
            },
            '/' => {
                if (try self.comment()) |tok| {
                    return tok;
                }
                return try self.punctuator();
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
            '%',
            '^',
            '=',
            '!',
            '|',
            '&',
            '?',
            => return try self.punctuator(),
            '"', '\'' => return try self.stringLiteral(),
            '#' => unreachable,
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

    /// tokenize a "." or a "..." token, assuming self.index is at '.'
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
            if (isNewline(ch) and ch != '\r') {
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

    fn isNewline(ch: u21) bool {
        return ch == '\n' or ch == '\r' or ch == '\u{2028}' or ch == '\u{2029}';
    }

    fn comment(self: *Self) TokenizeError!?Token {
        const start = self.index;
        const str = self.source[start..];
        if (str.len < 2) return null;

        var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 2 };
        if (std.mem.startsWith(u8, str, "//")) {
            // https://262.ecma-international.org/15.0/index.html#prod-SingleLineComment
            while (iter.nextCodepoint()) |ch| {
                if (isNewline(ch)) {
                    self.line += 1;
                    break;
                }
            }
        } else if (std.mem.startsWith(u8, str, "/*")) {
            // https://262.ecma-international.org/15.0/index.html#prod-MultiLineComment
            while (iter.nextCodepoint()) |ch| {
                if (ch == '*') {
                    if (iter.i < str.len and str[iter.i] == '/') {
                        iter.i += 1;
                        break;
                    }
                } else if (isNewline(ch)) {
                    self.line += 1;
                }
            }
        } else {
            return null;
        }

        self.index += @intCast(iter.i);
        return .{
            .start = start,
            .len = @intCast(iter.i),
            .tag = .comment,
        };
    }

    fn parseEscape(str: []const u8) ?usize {
        if (str.len < 2 or str[0] != '\\') return null;
        switch (str[1]) {
            'x' => {
                // \xXX
                if (str.len >= 4 and std.ascii.isHex(str[2]) and std.ascii.isHex(str[3]))
                    return 4
                else
                    return null;
            },
            'u' => {
                // \uXXXX or \u{X} - \u{XXXXXX}
                const parsed = parseUnicodeEscape(str) orelse return null;
                return parsed.len;
            },
            else => return 2,
        }
    }

    fn stringLiteral(self: *Self) TokenizeError!Token {
        const start = self.index;
        const str = self.source[start..];

        const quote_char: u21 = if (str[0] == '\'') '\'' else '"';

        var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 1 };
        var found_end_quote = false;
        while (iter.i < str.len) {
            if (str[iter.i] == '\\') {
                iter.i += parseEscape(str[iter.i..]) orelse
                    return TokenizeError.BadEscapeSequence;
                continue;
            }

            const codepoint = iter.nextCodepoint() orelse
                unreachable;
            if (codepoint == quote_char) {
                found_end_quote = true;
                break;
            }
        }

        if (!found_end_quote) {
            return TokenizeError.NonTerminatedString;
        }

        const len: u32 = @intCast(iter.i);
        return .{
            .tag = .string_literal,
            .start = start,
            .len = len,
        };
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

        const id_str = str[0..len];
        for (0.., all_keywords) |i, kw| {
            if (std.mem.eql(u8, id_str, kw)) {
                return Token{
                    .start = start,
                    .len = @intCast(len),
                    .tag = all_kw_tags[i],
                };
            }
        }

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
                    if (str.len > 1 and str[1] == '=') {
                        len += 1;
                        break :blk .@"/=";
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

        self.index += len;
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
            if (canCodepointStartId(ch) or ch == '.' or std.ascii.isDigit(ch)) {
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
        if (tag == .comment) return;

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

fn testTokenError(str: []const u8, err: anyerror) !void {
    var tokenizer = Tokenizer.init(str) catch |init_error| {
        try t.expectEqual(err, init_error);
        return;
    };
    const token_or_err = tokenizer.next();
    try t.expectError(err, token_or_err);
}

test Tokenizer {
    const test_cases = [_]struct { []const u8, Token.Tag }{
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
        .{ "/=", .@"/=" },
        .{ "??=", .@"??=" },
        .{ "?", .@"?" },
        .{ "??", .@"??" },
        .{ ",", .@"," },
        .{ "'hello, world!'", .string_literal },
        .{ "'\\u{95}world!'", .string_literal },
        .{ "'\\u{95}world\\{105}'", .string_literal },
        .{ "'\\xFFworld\\{105}'", .string_literal },
        .{ "\"\\xFFworld\\{105}\"", .string_literal },
        .{ "// test comment", .comment },
        .{ "//", .comment },
        .{ "// else", .comment },
        .{ "else", .kw_else },
        .{ "\\u0065lse", .identifier },
        .{ "elsey", .identifier },
        .{ "/* test comment */", .comment },
    };

    for (test_cases) |case| {
        const text, const tag = case;
        testToken(text, tag) catch |err| {
            std.debug.print("failed to parse {s} as {s}\n", .{ text, @tagName(tag) });
            return err;
        };
    }

    // test that tokenizer can handle empty input
    {
        var tokenizer = try Tokenizer.init("");
        try std.testing.expectEqualDeep(Token{
            .tag = .eof,
            .start = 0,
            .len = 0,
        }, try tokenizer.next());
    }

    const bad_cases = [_]struct { []const u8, anyerror }{
        .{ "'hello", TokenizeError.NonTerminatedString },
        .{ "1.5.5", TokenizeError.InvalidNumericLiteral },
        .{ "1.5aaaA", TokenizeError.InvalidNumericLiteral },
        .{ "1.5_1", TokenizeError.InvalidNumericLiteral },
    };

    for (bad_cases) |case| {
        const text, const expected_err = case;
        testTokenError(text, expected_err) catch |err| {
            std.debug.print("tokenizing {s} did not raise {any}\n", .{ text, err });
            return err;
        };
    }

    {
        var tokenizer = try Tokenizer.init("123.00 + .333");
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"+", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
    }

    {
        var tokenizer = try Tokenizer.init(
            \\/* this is a
            \\ multiline
            \\ comment */
        );
        const comment_token = try tokenizer.next();
        try t.expectEqual(.comment, comment_token.tag);
        try t.expectEqualDeep(
            tokenizer.source,
            comment_token.toByteSlice(tokenizer.source),
        );
        try t.expectEqual(2, tokenizer.line);
    }
}
