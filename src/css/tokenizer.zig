const std = @import("std");
const util = @import("util");
const types = util.types;
const offsets = util.offsets;

const codePointAt = util.utf8.codePointAt;
const StringPool = util.StringPool;
const Str = StringPool.String;

const Self = @This();

pub const Token = struct {
    pub const Index = enum(u32) { _ };

    pub const Data = union(enum) {
        eof,
        comment,

        // TODO: Store the escaped identifier string here.
        identifier,
        // Contains the escaped string value, including quotes
        string: Str,

        // operators and punctuators
        @")",
        @"(",
        @"{",
        @"}",
        @"[",
        @"]",

        // erroenous tokens

        /// A string with invalid escape sequence.
        malformed_string,
        /// A string that is missing the ending '"'.
        unterminated_string,
        /// A string that has a newline character in it.
        newline_string,
    };

    /// Byte offset into the input source string
    start: u32,
    /// Length of the token in bytes
    len: u32,
    /// 0-indexed line number where the token starts.
    line: u32,

    /// Type tag to identify the kind of token,
    /// along with some meta-data (e.g: Numeric value)
    data: Data,

    /// Get the tag to identify this token.
    pub fn tag(self: *const Token) std.meta.Tag(Data) {
        return std.meta.activeTag(self.data);
    }

    /// Get the source bytes of this token.
    pub fn toByteSlice(self: *const Token, source: []const u8) []const u8 {
        return source[self.start .. self.start + self.len];
    }

    /// (line, column) position for the start of this token.
    pub fn startCoord(self: *const Token, source: []const u8) types.Coordinate {
        return offsets.byteIndexToCoordinate(source, self.start);
    }
};

/// Value of a CSS number token.
/// Ref: https://drafts.csswg.org/css-syntax-3/#consume-number
pub const NumericLiteral = struct {
    pub const Kind = enum(u8) { integer, float };
    value: f64,
    kind: Kind,
    has_sign: bool,
};

pub const Error = error{
    InvalidUtf8,
    UnexpectedChar,
    UnterminatedComment,
    NoMatchingToken,
    /// string pool got too big to represent with u32.
    Overflow,
    OutOfMemory,
};

allocator: std.mem.Allocator,

/// Input source bytes.
/// Should be valid UTF-8 for `.init` to succeed.
source: []const u8,

/// Current line number in the input stream.
line: u32 = 0,

/// Current byte offset in the input stream.
index: u32 = 0,

/// Temporary buffer for storing intermediate values.
scratch: []u8,

/// Intern strings to de-duplicate them and compare in O(1) time.
/// See: src/util/string_intern.zig.
string_pool: StringPool,

/// Create a new tokenizer
pub fn init(allocator: std.mem.Allocator, source: []const u8) error{ InvalidUtf8, OutOfMemory }!Self {
    if (!std.unicode.utf8ValidateSlice(source)) {
        return Error.InvalidUtf8;
    }

    return Self{
        .allocator = allocator,
        .source = source,
        // make the buffer as big as the input so we never run out of memory,
        // or reallocate.
        .scratch = try allocator.alloc(u8, source.len),
        .string_pool = try StringPool.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.scratch);
    self.string_pool.deinit();
}

/// Get the next token from the input stream.
/// Returns `.Eof` if the end of the input stream is reached.
pub fn next(self: *Self) Error!Token {
    const ch = self.peekByte() orelse return .{
        .data = .{ .eof = {} },
        .start = self.index,
        .len = 0,
        .line = self.line,
    };

    switch (ch) {
        '\n', '\r', '\t', ' ', 0x0c => {
            self.skipWhitespace();
            return try self.next();
        },

        '(' => return self.singleCharToken(.{ .@"(" = {} }),
        ')' => return self.singleCharToken(.{ .@")" = {} }),
        '[' => return self.singleCharToken(.{ .@"[" = {} }),
        ']' => return self.singleCharToken(.{ .@"]" = {} }),
        '{' => return self.singleCharToken(.{ .@"{" = {} }),
        '}' => return self.singleCharToken(.{ .@"}" = {} }),

        '/' => {
            if (try self.comment()) |comment_token| {
                return comment_token;
            }
        },
        '"', '\'' => return try self.stringLiteral(),
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
                // check if \{codepoint} is a valid escape sequence
                const escape_len = matchSingleEscape(str[1..]) orelse return null;
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

/// Match an escape code that starts with "/", and has a single code-point
fn matchEscapeCodeSingleCodepoint(str: []const u8) ?u32 {
    std.debug.assert(str[0] == '\\');
    if (str.len < 2) return null;

    return null;
}

/// Match a CSS escape sequence.
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
        const cp_len = std.unicode.utf8ByteSequenceLength(str[1]) catch unreachable;
        return 1 + cp_len; // '\' + length of codepoint
    }

    // Invalid escape sequence.
    return null;
}

// Match a single escaped codepoint after a "/".
fn matchSingleEscape(str: []const u8) ?u32 {
    std.debug.assert(str[0] == '\\');
    if (str.len < 2) return null;

    if (std.ascii.isAscii(str[1])) {
        return if (isNewlineChar(str[1])) null else 2;
    }

    const cp_len =
        std.unicode.utf8ByteSequenceLength(str[1]) catch
            unreachable; // input has already been validated at this point.

    return 1 + cp_len; // '\' + length of codepoint
}

/// Parse a CSS Identifier sequence
fn identifierSequence(self: *Self) ?Token {
    const start_idx = self.index;

    self.index += matchIdentStart(self.source[self.index..]) orelse return null;

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
        .data = .{ .identifier = {} },
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
        .data = .{ .comment = {} },
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

/// Returns a tuple where the first item is the length of the string literal,
/// and the second item is the tag for that type of string.
///
/// the tag returned is one of: string, malformed_string, newline_string, unterminated_string
///
/// Ref: https://drafts.csswg.org/css-syntax-3/#consume-string-token
fn matchStringLiteral(self: *Self, str: []const u8) Error!struct { u32, Token.Data } {
    std.debug.assert(str.len > 0 and (str[0] == '"' or str[0] == '\''));
    const quote = str[0];
    self.scratch[0] = quote;

    // Length of the string after processing all escapes.
    var escaped_len: usize = 1;

    var i: usize = 1;
    while (i < str.len) {
        const byte = str[i];
        if (!std.ascii.isAscii(byte)) {
            // Found a non-ASCII UTF-8 codepoint,
            // Copy it into the scratch buffer.
            const cp_len = codePointAt(str, i).len;

            const dst = self.scratch[escaped_len .. escaped_len + cp_len];
            const src = str[i .. i + cp_len];
            @memcpy(dst, src);

            i += cp_len;
            escaped_len += cp_len;
        } else if (byte == quote) {
            // Found the closing quote.
            // Copy the quote into the scratch buffer and exit.
            self.scratch[escaped_len] = quote;
            escaped_len += 1;

            const interned = try self.string_pool.getOrInsert(self.scratch[0..escaped_len]);
            return .{ @intCast(i + 1), .{ .string = interned } };
        } else if (byte == '\\') {
            // Found an escape sequence.
            // Process the escape, then copy to the buffer without the leading '\'.
            const escape_seq_len = matchSingleEscape(str[i..]) orelse return .{
                @intCast(i),
                .{ .malformed_string = {} },
            };

            const len_without_bslash = escape_seq_len - 1; // skip '/'
            const dst = self.scratch[escaped_len .. escaped_len + len_without_bslash];
            const src = str[i + 1 .. i + escape_seq_len];
            @memcpy(dst, src);

            i += escape_seq_len;
            escaped_len += escape_seq_len - 1;
        } else if (isNewlineChar(byte)) {
            // Newlines are not allowed in CSS strings.
            // Return an erroenous token.
            return .{ @intCast(i), .{ .malformed_string = {} } };
        } else {
            // A regular ASCII character.
            // copy it into the scratch buffer.
            self.scratch[escaped_len] = byte;

            i += 1;
            escaped_len += 1;
        }
    }

    // We never found the closing quote.
    return .{ @intCast(i), .{ .unterminated_string = {} } };
}

/// Ref: https://drafts.csswg.org/css-syntax-3/#consume-string-token
fn stringLiteral(self: *Self) Error!Token {
    const start = self.index;

    const len, const data = try self.matchStringLiteral(self.source[self.index..]);
    self.index += len;
    return .{
        .data = data,
        .start = start,
        .len = len,
        .line = self.line,
    };
}

// Helper functions:

fn singleCharToken(self: *Self, data: Token.Data) Token {
    self.index += 1;
    return .{
        .data = data,
        .start = self.index - 1,
        .len = 1,
        .line = self.line,
    };
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

fn testToken(src: []const u8, tag: std.meta.Tag(Token.Data)) !void {
    // first, test that token followed by EOF
    {
        var tokenizer = try Self.init(t.allocator, src);
        defer tokenizer.deinit();

        const token = try tokenizer.next();
        try t.expectEqual(tag, token.tag());
        try t.expectEqual(@as(u32, @intCast(src.len)), token.len);
        try t.expectEqual(0, token.start);
        try t.expectEqual(0, token.line);
    }

    // then, that token wrapped by whitespace
    {
        if (tag == .comment or tag == .eof or tag == .unterminated_string)
            return;

        const source = try std.mem.concat(
            t.allocator,
            u8,
            &[_][]const u8{ "\n ", src, " " },
        );

        defer t.allocator.free(source);
        var tokenizer = try Self.init(t.allocator, source);
        defer tokenizer.deinit();

        const token = try tokenizer.next();

        try t.expectEqual(tag, token.tag());
        try t.expectEqual(@as(u32, @intCast(src.len)), token.len);
        try t.expectEqual(2, token.start);
        try t.expectEqual(1, token.line);
    }
}

test {
    const PassingCase = struct { []const u8, std.meta.Tag(Token.Data) };
    // these should parse correctly
    const good_cases = [_]PassingCase{
        .{ "", .eof },
        .{ "(", .@"(" },
        .{ ")", .@")" },
        .{ "{", .@"{" },
        .{ "}", .@"}" },
        .{ "/**/", .comment },
        .{ "/* hello, hi */", .comment },
        .{ "/* hello, hi\n\r\n */", .comment },
        .{ "identifier", .identifier },
        .{ "--id", .identifier },
        .{ "--", .identifier },
        .{ "-id", .identifier },
        .{ "-\\x", .identifier },
        .{ "-\\x\\yidentifier-of-my-choic\\e", .identifier },
        .{ "--\\x\\yidentifier-of-my-choice", .identifier },
        .{ "😃", .identifier },
        .{ "--😃", .identifier },
        .{ "-😃", .identifier },
        .{ "-\\😃", .identifier },
        .{ "-_", .identifier },
        .{ "-\\e", .identifier },
        .{ "-\\😃", .identifier },
        .{ "'A string'", .string },
        .{ "\"Double quoted string\"", .string },
        .{ "\"Double quoted \\string\"", .string },
        .{ "'A string", .unterminated_string },
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
        var tokenizer = try Self.init(t.allocator, str);
        defer tokenizer.deinit();

        try t.expectError(expected_err, tokenizer.next());
    }

    {
        var tokenizer = try Self.init(t.allocator, "'esc\\aped' 'escaped'");
        defer tokenizer.deinit();

        const token1 = try tokenizer.next();
        try t.expectEqual(.string, token1.tag());
        try t.expectEqualStrings("'escaped'", tokenizer.string_pool.toByteSlice(token1.data.string));
        try t.expectEqualStrings("'esc\\aped'", token1.toByteSlice(tokenizer.source));

        const token2 = try tokenizer.next();
        try t.expectEqual(.string, token2.tag());
        try t.expectEqualStrings("'escaped'", tokenizer.string_pool.toByteSlice(token2.data.string));
        try t.expectEqualStrings("'escaped'", token2.toByteSlice(tokenizer.source));
    }
}
