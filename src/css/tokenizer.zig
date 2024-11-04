const std = @import("std");
const util = @import("util");

const codePointAt = util.codePointAt;

const Self = @This();

pub const Token = struct {
    pub const Tag = enum {
        eof,
        comment,
        identifier,
    };

    tag: Tag,
    /// Byte offset into the input source string
    start: u32,
    /// Length of the token in bytes
    len: u32,
    /// 0-indexed line number where the token starts.
    line: u32,
};

pub const Error = error{
    InvalidUtf8,
    UnexpectedChar,
    UnterminatedComment,
    NoMatchingToken,
};

/// Input source bytes.
/// Should be valid UTF-8 for `.init` to succeed.
source: []const u8,

/// Current line number in the input stream.
line: u32 = 0,

/// Current byte offset in the input stream.
index: u32 = 0,

/// Create a new tokenizer
pub fn init(source: []const u8) Error!Self {
    if (!std.unicode.utf8ValidateSlice(source)) {
        return Error.InvalidUtf8;
    }

    return Self{
        .source = source,
    };
}

/// Get the next token from the input stream.
/// Returns `.Eof` if the end of the input stream is reached.
pub fn next(self: *Self) Error!Token {
    const ch = self.peekByte() orelse return .{
        .tag = Token.Tag.eof,
        .start = self.index,
        .len = 0,
        .line = self.line,
    };

    switch (ch) {
        '/' => {
            if (try self.comment()) |comment_token| {
                return comment_token;
            }
        },
        '\n', '\r', '\t', ' ', 0x0c => {
            self.skipWhitespace();
            return try self.next();
        },
        else => {
            return self.identifierSequence() orelse Error.NoMatchingToken;
        },
    }

    return Error.UnexpectedChar;
}

/// Check if a UTF-8 code-point is a valid identifier character.
/// https://drafts.csswg.org/css-syntax-3/#non-ascii-ident-code-point
fn isNonAsciiIdentCodePoint(code_point: u21) bool {
    return switch (code_point) {
        0x00B7,
        0x00C0...0x00D6,
        0x00D8...0x00F6,
        0x00F8...0x037D,
        0x037F...0x1FFF,
        0x200C,
        0x200D,
        0x203F,
        0x2040,
        0x2070...0x218F,
        0x2C00...0x2FEF,
        0x3001...0xD7FF,
        0xF900...0xFDCF,
        0xFDF0...0xFFFD,
        0x10000...std.math.maxInt(u21),
        => true,
        else => false,
    };
}

fn isIdentStartCodePoint(code_point: u21) bool {
    return switch (code_point) {
        'a'...'z', 'A'...'Z', '_', '-' => true,
        else => isNonAsciiIdentCodePoint(code_point),
    };
}

fn isAsciiIdentifierPartChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '-';
}

/// Checks if the input stream starts with a valid identifier start character.
/// Returns the length of the identifier start sequence (in bytes), or `null` if no
/// valid identifier start sequence is matched.
///
/// Ref: https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
fn matchIdentStart(str: []const u8) ?u32 {
    std.debug.assert(str.len > 0);

    switch (str[0]) {
        'a'...'z', 'A'...'Z', '_' => return 1,
        '-' => {
            if (str.len < 2) return null;

            if (str[1] == '-') {
                // '--' was matched
                return 2;
            }

            // The next codepoint can be either an identifier codepoint or an escape sequence
            const cp = codePointAt(str, 1); // skip '-'
            if (isIdentStartCodePoint(cp.value)) {
                // '-'{ID codepoint}
                return 1 + cp.len;
            }

            if (cp.value == '\\') {
                // '-'{escape}
                if (str.len < 3) return null; // -\<eof> is invalid.
                const next_cp_len = std.unicode.utf8ByteSequenceLength(str[2]) catch unreachable;
                // check if \{codepoint} is a valid escape sequence
                const escape_len = matchEscape(str[1 .. 2 + next_cp_len]) orelse return null;
                return 1 + escape_len; // '-' + escape sequence length (includes '\')
            }

            return null;
        },
        else => {
            const cp = codePointAt(str, 0);
            if (isNonAsciiIdentCodePoint(cp.value)) {
                return cp.len;
            }

            return null;
        },
    }
}

/// Check if a character is a newline character.
fn isNewlineChar(ch: u8) bool {
    return ch == '\n' or ch == '\r' or ch == 0x0c;
}

/// Check if a character is a whitespace character.
/// NOTE: we cannot use std.ascii.isWhitespace because it includes vertical tabs.
fn isWhitespaceChar(ch: u8) bool {
    return ch == '\t' or ch == ' ' or isNewlineChar(ch);
}

/// Skip over any newline characters.
/// Returns `true` if any newline characters were found.
fn skipNewlines(self: *Self) bool {
    var found_newline = false;
    while (self.index < self.source.len) : (self.index += 1) {
        const ch = self.source[self.index];
        if (isNewlineChar(ch)) {
            found_newline = true;
            if (ch == '\r' and self.peekAhead(1) == '\n')
                self.index += 1;
            self.line += 1;
        } else {
            break;
        }
    }

    return found_newline;
}

/// Skip over any whitespace characters.
fn skipWhitespace(self: *Self) void {
    while (self.index < self.source.len) : (self.index += 1) {
        const ch = self.source[self.index];
        if (isNewlineChar(ch)) {
            if (ch == '\r' and self.peekAhead(1) == '\n')
                self.index += 1; // eat \r, \r\n is a single newline
            self.line += 1;
        } else if (ch == '\t' or ch == ' ') {
            // skip whitespace char
        } else {
            break;
        }
    }
}

/// Match a CSS escape sequence.
/// TODO: allow unicode codepints to be escaped.
/// TOOD: add a helper – matchEscape2, to check for just 2 codepoitns
fn matchEscape(str: []const u8) ?u32 {
    std.debug.assert(str[0] == '\\');
    if (str.len < 2) return null;

    if (std.ascii.isHex(str[1])) {
        var i: usize = 0;
        while (i < str.len and std.ascii.isHex(str[i])) : (i += 1) {}
        if (i >= 6) return null; // 1-6 hex digits allowed.
        // skip optional whitespace after escape sequence.
        if (i < str.len and isWhitespaceChar(str[i])) i += 1;
        return @intCast(i + 1); // include the leading '\'
    } else if (!isNewlineChar(str[1])) {
        return 2;
    }

    // Invalid escape sequence.
    return null;
}

/// Parse a CSS Identifier sequence
fn identifierSequence(self: *Self) ?Token {
    const start_idx = self.index;

    self.index += matchIdentStart(self.source[self.index..]) orelse
        return null;

    while (self.index < self.source.len) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (isAsciiIdentifierPartChar(byte)) {
                self.index += 1;
                continue;
            }

            if (byte == '\\') {
                const escape_len = matchEscape(self.source[self.index..]) orelse
                    return null;
                self.index += escape_len;
                continue;
            }

            // non-identifier ASCII character
            break;
        }

        const cp = codePointAt(self.source, self.index);
        if (isNonAsciiIdentCodePoint(cp.value)) {
            self.index += cp.len;
        } else {
            break;
        }
    }

    return Token{
        .tag = .identifier,
        .start = start_idx,
        .len = self.index - start_idx,
        .line = self.line,
    };
}

/// Parse a comment token.
fn comment(self: *Self) Error!?Token {
    if (!self.isAtCommentStart()) {
        return null;
    }

    const start = self.index;
    const line = self.line;

    while (!self.isAtCommentEnd()) {
        if (self.isAtEof()) {
            return Error.UnterminatedComment;
        }

        if (!self.skipNewlines()) {
            self.index += 1;
        }
    }

    self.index += 2; // skip the closing "*/"

    return .{
        .tag = .comment,
        .start = start,
        .len = self.index - start,
        .line = line,
    };
}

/// Returns true if the remaining input starts with "*/"
fn isAtCommentEnd(self: *const Self) bool {
    return self.index + 1 < self.source.len and
        self.source[self.index] == '*' and
        self.source[self.index + 1] == '/';
}

/// Returns true if the remaining input starts with "/*"
fn isAtCommentStart(self: *const Self) bool {
    return self.index + 1 < self.source.len and
        self.source[self.index] == '/' and
        self.source[self.index + 1] == '*';
}

/// Peek at the next byte in the input stream without consuming it.
fn peekByte(self: *const Self) ?u8 {
    if (self.index >= self.source.len) {
        return null;
    }
    return self.source[self.index];
}

/// Peek at the Nth byte in the input stream without consuming anything.
fn peekAhead(self: *const Self, n: u32) ?u8 {
    if (self.index + n >= self.source.len) {
        return null;
    }
    return self.source[self.index + n];
}

/// Consume the next byte of input and return it.
/// Returns `null` if the end of the input stream is reached.
fn eatChar(self: *Self) ?u8 {
    const ch = self.peekByte() orelse return null;
    self.index += 1;
    return ch;
}

/// Consume the next byte of input and return it.
/// Panics if the end of the input stream is reached.
fn eatCharUnsafe(self: *Self) u8 {
    self.index += 1;
    return self.source[self.index - 1];
}

fn isAtEof(self: *const Self) bool {
    return self.index >= self.source.len;
}

// unit tests

const t = std.testing;

fn testToken(src: []const u8, tag: Token.Tag) !void {
    // first, test that token followed by EOF
    {
        var tokenizer = try Self.init(src);
        const token = try tokenizer.next();

        try std.testing.expectEqualDeep(Token{
            .tag = tag,
            .start = 0,
            .len = @intCast(src.len),
            .line = 0,
        }, token);
    }

    // then, that token wrapped by whitespace
    // {
    //     if (tag == .comment) return;
    //
    //     const source = try std.mem.concat(
    //         t.allocator,
    //         u8,
    //         &[_][]const u8{ "\n ", src, " " },
    //     );
    //
    //     defer t.allocator.free(source);
    //     var tokenizer = try Self.init(source);
    //     const token = try tokenizer.next();
    //
    //     try std.testing.expectEqualDeep(Token{
    //         .tag = tag,
    //         .start = 2,
    //         .len = @intCast(src.len),
    //         .line = 1,
    //     }, token);
    // }
}

test {
    const PassingCase = struct { []const u8, Token.Tag };
    // these should parse correctly
    const good_cases = [_]PassingCase{
        .{ "", .eof },
        .{ "/**/", .comment },
        .{ "/* hello, hi */", .comment },
        .{ "/* hello, hi\n\r\n */", .comment },
        .{ "identifier", .identifier },
        .{ "--id", .identifier },
        .{ "-id", .identifier },
        .{ "-\\x", .identifier },
        .{ "-\\x\\yidentifier-of-my-choice", .identifier },
        .{ "--\\x\\yidentifier-of-my-choice", .identifier },
    };

    for (good_cases) |test_case| {
        const str, const expected_tag = test_case;
        testToken(str, expected_tag) catch {
            std.log.err("failed to parse {s} as {s}.", .{ str, @tagName(expected_tag) });
        };
    }

    const FailingCase = struct { []const u8, Error };
    const bad_cases = [_]FailingCase{
        .{ "/*", Error.UnterminatedComment },
        .{ "/*well, this is also incomplete\n", Error.UnterminatedComment },
    };

    for (bad_cases) |test_case| {
        const str, const expected_err = test_case;
        var tokenizer = try Self.init(str);
        try t.expectError(expected_err, tokenizer.next());
    }
}
