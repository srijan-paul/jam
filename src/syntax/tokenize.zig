const std = @import("std");
const unicode_id = @import("unicode-id");
const Token = @import("token.zig").Token;
const Context = @import("parser.zig").ParseContext;

const util = @import("util");

const TokenizeError = error{
    UnexpectedEof,
    InvalidUtf8,
    UnexpectedByte,
    InvalidNumericLiteral,
    InvalidHexLiteral,
    InvalidBinaryLiteral,
    BadPunctuator,
    BadEscapeSequence,
    NonTerminatedString,
    InvalidTokenizerState,
    MalformedIdentifier,
};

// zig fmt: off
const all_keywords = [_][]const u8{
  "await", "break",  "case",  "catch",
  "class", "const",  "continue", "debugger",
  "default",  "delete", "do", "else", "enum",
  "export", "extends", "false", "finally",
  "for", "function", "if", "import", "in",
  "instanceof", "let", "new", "null", "return",
  "super", "switch", "this", "throw", "true",
  "try", "typeof", "var", "void", "while",
  "with", "yield", "async", "of"
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
    /// The current context that is controlled by the parser (or caller).
    /// This controls how certain tokens are parsed (e.g: whether `await` is a keyword or an identifier).
    context: Context = .{},

    /// Can be used to restore the state of the tokenizer to a previous
    /// location in the input.
    pub const State = struct {
        index: u32,
        line: u32,
        col: u32,
    };

    pub fn saveState(self: *Self) State {
        return State{
            .index = self.index,
            .line = self.line,
            .col = self.col,
        };
    }

    pub fn restoreState(self: *Self, state: State) void {
        self.index = state.index;
        self.line = state.line;
        self.col = state.col;
    }

    pub fn init(source: []const u8) TokenizeError!Self {
        if (!std.unicode.utf8ValidateSlice(source)) {
            return TokenizeError.InvalidUtf8;
        }

        return Self{ .source = source };
    }

    pub fn next(self: *Self) TokenizeError!Token {
        const byte = self.peekByte() orelse {
            return Token{
                .tag = Token.Tag.eof,
                .start = self.index,
                .len = 0,
                .line = self.line,
            };
        };

        switch (byte) {
            ' ', '\t' => {
                self.skipWhiteSpaces();
                return self.next();
            },
            '\n', '\r' => {
                self.skipNewlines();
                return self.next();
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
            '#' => return try self.privateIdentifier(),
            '0'...'9' => return self.numericLiteral(),
            '.' => {
                if (self.numericLiteral()) |tok| {
                    return tok;
                } else |_| {
                    return self.dot();
                }
            },
            else => return try self.identifier(),
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
            .line = self.line,
        };
    }

    // Skip all newlines and update the current line number.
    fn skipNewlines(self: *Self) void {
        while (!self.eof()) : (self.index += 1) {
            const ch = self.source[self.index];
            if (isNewline(ch)) {
                self.line += 1;
                if (ch == '\r' and
                    self.index + 1 < self.source.len and
                    self.source[self.index + 1] == '\n')
                {
                    // skip /r/n
                    self.index += 1;
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
        const start_line = self.index;
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
                    // TODO: what if its a \r\n break?
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
            .line = start_line,
        };
    }

    fn parseEscape(self: *Self, iter: *std.unicode.Utf8Iterator) !void {
        const str = iter.bytes[iter.i..];
        if (str.len < 2 or str[0] != '\\')
            return TokenizeError.BadEscapeSequence;

        switch (str[1]) {
            'x' => {
                // \xXX
                if (str.len >= 4 and std.ascii.isHex(str[2]) and std.ascii.isHex(str[3])) {
                    iter.i += 4;
                } else {
                    return TokenizeError.BadEscapeSequence;
                }
            },
            'u' => {
                // \uXXXX or \u{X} - \u{XXXXXX}
                const parsed = util.utf8.parseUnicodeEscape(str) orelse
                    return TokenizeError.BadEscapeSequence;
                iter.i += parsed.len;
            },
            else => {
                iter.i += 1; // skip /
                const cp = iter.nextCodepoint() orelse return TokenizeError.InvalidUtf8;
                if (isNewline(cp)) self.line += 1;
            },
        }
    }

    fn stringLiteral(self: *Self) TokenizeError!Token {
        const start = self.index;
        const start_line = self.line;
        const str = self.source[start..];

        const quote_char: u21 = if (str[0] == '\'') '\'' else '"';

        var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 1 };
        var found_end_quote = false;
        while (iter.i < str.len) {
            if (str[iter.i] == '\\') {
                try self.parseEscape(&iter);
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
        self.index += len;
        return .{
            .tag = .string_literal,
            .start = start,
            .len = len,
            .line = start_line,
        };
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
        if (util.utf8.parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
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
        if (util.utf8.parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
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

    fn identifier(self: *Self) TokenizeError!Token {
        const start = self.index;
        const str = self.source[start..];

        const view = std.unicode.Utf8View.initUnchecked(str);
        var it = view.iterator();

        const is_valid_start = matchIdentifierStart(self, &it) catch |err|
            return err;
        if (!is_valid_start) return TokenizeError.MalformedIdentifier;

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
        if (len >= 2 and len <= 12) {
            for (0.., all_keywords) |i, kw| {
                if (std.mem.eql(u8, id_str, kw)) {
                    self.index += @intCast(len);
                    return Token{
                        .start = start,
                        .len = @intCast(len),
                        .tag = all_kw_tags[i],
                        .line = self.line,
                    };
                }
            }
        }

        self.index += @intCast(len);
        return Token{
            .start = start,
            .len = @intCast(len),
            .tag = Token.Tag.identifier,
            .line = self.line,
        };
    }

    /// https://262.ecma-international.org/15.0/index.html#prod-PrivateIdentifier
    fn privateIdentifier(self: *Self) !Token {
        const start = self.index;
        self.index += 1; // eat '#'
        const id = try self.identifier();
        return Token{
            .start = start,
            .len = id.len + 1,
            .tag = .private_identifier,
            .line = self.line,
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
                '[' => break :blk .@"[",
                ']' => break :blk .@"]",
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
                    } else if (str.len > 1 and str[1] == '.') {
                        len += 1;
                        break :blk .@"?.";
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
            .line = self.line,
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

    /// https://262.ecma-international.org/15.0/index.html#prod-grammar-notation-HexIntegerLiteral
    fn matchHexDigits(str: []const u8) ?u32 {
        var i: u32 = 0;
        while (i < str.len and std.ascii.isHex(str[i])) : (i += 1) {
            if (i + 1 < str.len and str[i + 1] == '_')
                i += 1;
        }

        return if (i > 0) i else null;
    }

    fn matchBinaryDigits(str: []const u8) ?u32 {
        var i: u32 = 0;
        while (i < str.len and (str[i] == '0' or str[i] == '1')) : (i += 1) {
            if (i + 1 < str.len and str[i + 1] == '_')
                i += 1;
        }

        return if (i > 0) i else null;
    }

    /// https://tc39.es/ecma262/#prod-DecimalLiteral
    fn matchDecimalPart(str: []const u8) ?u32 {
        if (str[0] != '.') return null;

        var i: u32 = 1;
        while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {}
        if (i == 1) return null;
        return i;
    }

    /// https://262.ecma-international.org/15.0/index.html#prod-ExponentPart
    fn matchExponentPart(str: []const u8) ?u32 {
        if (str.len < 2) return null;
        if (str[0] != 'e' and str[0] != 'E') return null;

        var i: u32 = 1;
        if (str[i] == '+' or str[i] == '-') {
            i += 1;
            if (i == str.len) return null;
        }

        while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {
            if (i + 1 < str.len and str[i + 1] == '_') {
                i += 1; // eat '_'
            }
        }

        // we should've consumed at least one digit after e/e+/e-
        if (!std.ascii.isDigit(str[i - 1])) {
            return null;
        }

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

            if (str.len > 2 and str[0] == '0' and (str[1] == 'x' or str[1] == 'X')) {
                const hex_len: u32 = matchHexDigits(str[2..]) orelse
                    return TokenizeError.InvalidHexLiteral;
                break :blk hex_len + 2;
            } else if (str.len > 2 and str[0] == '0' and (str[1] == 'b' or str[1] == 'B')) {
                const binary_len: u32 = matchBinaryDigits(str[2..]) orelse
                    return TokenizeError.InvalidBinaryLiteral;
                break :blk binary_len + 2;
            }

            var decimal_len = matchDecimalIntegerLiteral(str) orelse
                return TokenizeError.InvalidNumericLiteral;
            if (decimal_len < str.len) {
                decimal_len += matchDecimalPart(str[decimal_len..]) orelse 0;
            }

            if (decimal_len < str.len) {
                decimal_len += matchExponentPart(str[decimal_len..]) orelse 0;
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
            .line = self.line,
        };
    }

    fn remaining(self: *const Self) []const u8 {
        return self.source[self.index..];
    }

    pub fn eof(self: *Tokenizer) bool {
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

    fn isSimpleWhitespace(ch: u8) bool {
        switch (ch) {
            ' ',
            '\t',
            '\u{000B}',
            '\u{000C}',
            => return true,
            else => return false,
        }
    }

    /// Skip all consecutive white space characters.
    fn skipWhiteSpaces(self: *Self) void {
        while (!self.eof()) {
            const ch = self.source[self.index];
            // Whitespaces that are single byte UTF-8 code-points
            if (isSimpleWhitespace(ch)) {
                self.index += 1;
                continue;
            }

            // <ZWNSBP> (Zero Width No Break Space... yeah)
            if (ch == 0xEF and
                self.index + 2 < self.source.len and
                self.source[self.index + 1] == 0xBB and
                self.source[self.index + 2] == 0xBF)
            {
                self.index += 3;
                continue;
            }

            break;
        }
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
            .line = 0,
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
            .line = 1,
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
        .{ "#fooobar", .private_identifier },
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
        .{ "0x55ffee1", .numeric_literal },
        .{ "0b1011_0101", .numeric_literal },
        .{ "0b1", .numeric_literal },
        .{ "0x5", .numeric_literal },
        .{ "55e+1", .numeric_literal },
        .{ "55e-1", .numeric_literal },
        .{ "55e112", .numeric_literal },
        .{ "0XF", .numeric_literal },
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
        .{ "?.", .@"?." },
        .{ ",", .@"," },
        .{ "'hello, world!'", .string_literal },
        .{ "'\u{00000000078}'", .string_literal },
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
            .line = 0,
        }, try tokenizer.next());
    }

    const bad_cases = [_]struct { []const u8, anyerror }{
        .{ "'hello", TokenizeError.NonTerminatedString },
        .{ "1.5.5", TokenizeError.InvalidNumericLiteral },
        .{ "1.5aaaA", TokenizeError.InvalidNumericLiteral },
        .{ "1.5_1", TokenizeError.InvalidNumericLiteral },
        .{ "1__1", TokenizeError.InvalidNumericLiteral },
        .{ "0xg1", TokenizeError.InvalidHexLiteral },
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

    {
        var tokenizer = try Tokenizer.init("f([1])");
        try t.expectEqual(Token.Tag.identifier, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"(", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"[", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"]", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@")", (try tokenizer.next()).tag);
    }
    {
        var tokenizer = try Tokenizer.init(&.{ 40, 39, 92, 226, 128, 169, 39, 41 });
        try t.expectEqual(Token.Tag.@"(", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.string_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@")", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.eof, (try tokenizer.next()).tag);
    }
}
