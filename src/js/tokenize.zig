const std = @import("std");
const unicode_id = @import("unicode-id");
const util = @import("util");

const Token = @import("token.zig").Token;
const parser = @import("parser.zig");

const utf8 = util.utf8;
const assert = std.debug.assert;

const Self = @This();

pub const Error = error{
    UnexpectedEof,
    InvalidUtf8,
    UnexpectedByte,
    InvalidNumericLiteral,
    InvalidHexLiteral,
    InvalidBinaryLiteral,
    BadPunctuator,
    BadRegexLiteral,
    UnterminatedRegexClass,
    UnterminatedRegexLiteral,
    UnterminatedComment,
    BadEscapeSequence,
    OctalEscapeInStrictMode,
    UnterminatedString,
    NonTerminatedTemplateLiteral,
    MalformedIdentifier,
};

// zig fmt: off
pub const js_keywords = [_][]const u8{
  "await", "break",  "case",  "catch",
  "class", "const",  "continue", "debugger",
  "default",  "delete", "do", "else", "enum",
  "export", "extends", "false", "finally",
  "for", "function", "if", "import", "in",
  "instanceof", "let", "new", "null", "return",
  "super", "switch", "this", "throw", "true",
  "try", "typeof", "var", "void", "while",
  "with", "yield", "async", "of", "static",
  "constructor", "extends", "as", "from",
};
// zig fmt: on

pub const all_kw_tags: [js_keywords.len]Token.Tag = makeKwTagArray();
fn makeKwTagArray() [js_keywords.len]Token.Tag {
    @setEvalBranchQuota(10_000);

    var tags: [js_keywords.len]Token.Tag = undefined;
    const enum_tags = std.meta.tags(Token.Tag);
    for (0.., js_keywords) |i, kw| {
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

/// Source string to tokenize.
source: []const u8,
/// Byte offset into `source`
index: u32 = 0,
/// Current line number (0 indexed).
line: u32 = 0,
/// When `true`, the tokenizer assumes that a '}' character is the part of a template
/// literal after an interpolated expression, and not a "}" token.
/// e.g:
/// ```js
/// `foo${bar}baz`
///          ^
/// ```
/// This property is set by the parser when it expects the closing '}' of a template literal.
assume_rbrace_is_template_part: bool = false,

/// Whether the tokenizer is parsing a module or a script.
is_parsing_module: bool = false,

/// Whether the tokenizer is parsing code in strict mode.
/// This is set by the parser when it encounters a "use strict" directive.
/// In strict mode, numeric literals starting with '0' are not allowed.
is_in_strict_mode: bool = false,

config: parser.Config,

/// Whether the current line only contains whitespaces and comments so far.
/// This is needed because HTML close comments are only allowed in trivial lines.
/// e.g: `;/* foo */ --> bar` is invalid, but `/* foo */ --> bar` is valid.
is_trivial_line: bool = true,

pub fn init(source: []const u8, config: parser.Config) Error!Self {
    if (!std.unicode.utf8ValidateSlice(source))
        return Error.InvalidUtf8;

    return Self{
        .source = source,
        .config = config,
        .is_in_strict_mode = config.strict_mode or config.source_type == .module,
    };
}

/// Returns the next token, a token may be whitespaces or comments.
pub fn next(self: *Self) Error!Token {
    const token = try @call(.always_inline, consumeToken, .{self});
    const is_trivia = (token.tag == .comment or token.tag == .whitespace);
    self.is_trivial_line = self.is_trivial_line and is_trivia;
    return token;
}

/// Regex literals can be ambiguous with "/" or "/=" from the tokenizer's perspective,
/// as "/a/g" can mean either ['/', 'a', '/', 'g'], or ['/a/g' (regex) ].
///
/// When the parser sees a "/" or "/=" token, but it expects an expression, it should
/// call this function to re-scan the source code starting from the '/' character.
///
/// [div_token]: The token that was previously returned by the tokenizer when it saw the '/' character
/// (must be a "/" or "/=" token).
pub fn reScanRegexLiteral(self: *Self, div_token: *const Token) Error!Token {
    assert(div_token.tag == .@"/" or div_token.tag == .@"/=");
    self.rewind(div_token.start, div_token.line);
    return self.regexLiteral();
}

/// Inside template literals, a "}" should be treated as the part of a template string,
/// instead of a lone '}' token.
/// So '`foo${bar}baz`' should be tokenized as: '`foo${', 'bar', '}baz`'.
///
/// When the parser receives a '}' token after 'baz', it should call this function
/// to rescan the source code starting from the '}' character, and tokenize it as a template part.
///
/// [rbrace_token]: The token that was previously returned by the tokenizer when it saw the '}' character.
pub fn reScanTemplatePart(self: *Self, rbrace_token: *const Token) Error!Token {
    assert(rbrace_token.tag == .@"}");
    self.rewind(rbrace_token.start, rbrace_token.line);
    return self.templateAfterInterpolation();
}

/// Returns the next token that starts a JSX child.
/// The token returned is one of: '<', '{', or JSX text.
/// To tokenize JS expressions inside JSX (e.g: prop values), the 'next' function should be used instead.
/// The caller (parser) must know when to call `nextJsxChild` or `next` based on surrounding context.
pub fn nextJsxChild(self: *Self) Error!Token {
    const byte = self.peekByte() orelse {
        return Token{
            .tag = Token.Tag.eof,
            .start = self.index,
            .len = 0,
            .line = self.line,
        };
    };

    switch (byte) {
        '<' => {
            self.index += 1;
            return Token{
                .start = self.index - 1,
                .len = 1,
                .line = self.line,
                .tag = .@"<",
            };
        },

        '{' => {
            self.index += 1;
            return Token{
                .start = self.index - 1,
                .len = 1,
                .line = self.line,
                .tag = .@"{",
            };
        },

        else => {
            const start = self.index;
            const jsx_text_end_pos: u32 = @intCast(util.indexOfCh2(
                self.source,
                self.index,
                '<',
                '{',
            ) orelse self.source.len);

            self.index = jsx_text_end_pos;
            return Token{
                .tag = Token.Tag.jsx_text,
                .start = start,
                .len = jsx_text_end_pos - start,
                .line = self.line,
            };
        },
    }
}

/// Once an identifier or keyword has been eaten, this function can be called to
/// "continue" the previously eaten identifier as a JSXIdentifier and return the
/// full token.
///
/// [start] MUST be the token that was returned by `next` or `nextJsx`
/// immediately before calling this function.
///
/// JSXIdentifier :
///     IdentifierStart
///     JSXIdentifier IdentifierPart
///     JSXIdentifier [no WhiteSpace or Comment here] -
pub fn continueJsxIdentifier(self: *Self, start: *const Token) Error!Token {
    assert((start.tag == .identifier or
        start.tag == .non_ascii_identifier or
        start.tag.isKeyword()) and
        self.line == start.line and
        self.index == start.start + start.len);

    try eatJsxIdentifierTail(self);

    return Token{
        .tag = .jsx_identifier,
        .start = start.start,
        .len = self.index - start.start,
        .line = start.line,
    };
}

/// Consume all characters that form a part of a JSXIdentifier.
fn eatJsxIdentifierTail(self: *Self) Error!void {
    while (!self.eof() and
        isAsciiJsxIdentifierContt(self.source[self.index])) : (self.index += 1)
    {}

    if (!self.eof() and try self.matchIdentifierContt()) {
        @branchHint(.cold);
        while (!self.eof() and try self.matchJsxIdentifierContt()) {}
    }
}

/// If the current character can continue a JSXIdentifier,
/// return true and advance the index.
fn matchJsxIdentifierContt(self: *Self) Error!bool {
    if (self.source[self.index] == '-') {
        self.index += 1;
        return true;
    }

    return self.matchIdentifierContt();
}

/// Returns whether the ASCII byte [ch] can appear in the middle of a JSX Identifer
fn isAsciiJsxIdentifierContt(ch: u8) bool {
    return isAsciiIdentifierContt(ch) or ch == '-';
}

/// Return the next token that may be whitespaces or comments.
fn consumeToken(self: *Self) Error!Token {
    const byte = self.peekByte() orelse {
        return Token{
            .tag = Token.Tag.eof,
            .start = self.index,
            .len = 0,
            .line = self.line,
        };
    };

    switch (byte) {
        '/' => {
            if (try self.comment()) |tok|
                return tok;
            return try self.punctuator();
        },
        ' ',
        '\t',
        '\n',
        '\r',
        '\u{00A0}',
        '\u{000B}',
        '\u{000C}',
        => return self.whiteSpaces(),
        '}' => {
            self.index += 1;
            return Token{
                .start = self.index - 1,
                .len = 1,
                .line = self.line,
                .tag = .@"}",
            };
        },

        '{' => {
            self.index += 1;
            return Token{
                .start = self.index - 1,
                .len = 1,
                .line = self.line,
                .tag = .@"{",
            };
        },

        '<' => {
            if (self.config.source_type == .script) {
                if (try self.singleLineHtmlCommentOpen()) |tok|
                    return tok;
            }

            return self.punctuator();
        },

        '-' => {
            if (self.config.source_type == .script) {
                if (try self.singleLineHtmlCommentClose()) |tok|
                    return tok;
            }

            return self.punctuator();
        },

        '[',
        ']',
        '(',
        ')',
        ',',
        ';',
        ':',
        '>',
        '+',
        '*',
        '%',
        '^',
        '=',
        '!',
        '|',
        '&',
        '?',
        '~',
        => return try self.punctuator(),
        '"', '\'' => return try self.stringLiteral(),
        '`' => return try self.startTemplateLiteral(),
        '#' => return try self.privateIdentifier(),
        '0'...'9' => return self.numericLiteral(),
        '.' => {
            if (self.numericLiteral()) |tok| {
                return tok;
            } else |_| {
                return self.dot();
            }
        },
        else => return (try self.checkNonAsciiWhitespace()) orelse self.identifier(),
    }
}

/// If the next byte starts a whitespace sequence that starts with a non-ascii space,
/// consume it and return a whitespace token.
/// Otherwise, return null.
fn checkNonAsciiWhitespace(self: *Self) Error!?Token {
    const code_point_len = std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch
        return Error.InvalidUtf8;

    if (code_point_len > 1) {
        const code_point = utf8.codePointAt(self.source, self.index);
        if (isNewline(code_point.value) or isMultiByteSpace(code_point.value)) {
            return try self.whiteSpaces();
        }
    }

    return null;
}

/// Rewind the tokenizer to a previous location in the source code.
/// NOTE: the location given must be a valid index into the source code.
/// Rewinding the tokenizer to an invalid location, such as the middle of UTF-8 codepoint
/// results in undefined behavior.
pub fn rewind(self: *Self, index: u32, line: u32) void {
    self.index = index;
    self.line = line;

    if (self.eof()) return;
    _ = std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch {
        // TODO: make this return an error instead.
        std.debug.panic("Invalid rewind location", .{});
    };
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

fn isNewline(ch: u21) bool {
    return ch == '\n' or ch == '\r' or ch == '\u{2028}' or ch == '\u{2029}';
}

/// Return a comment token.
/// If the '/' does not start a comment at all, return null.
fn comment(self: *Self) Error!?Token {
    const start = self.index;
    const start_line = self.index;

    const str = self.source[start..];
    if (str.len < 2) return null;
    if (std.mem.startsWith(u8, str, "//")) {
        // https://262.ecma-international.org/15.0/index.html#prod-SingleLineComment
        try self.consumeSingleLineCommentChars();
    } else if (std.mem.startsWith(u8, str, "/*")) {
        // https://262.ecma-international.org/15.0/index.html#prod-MultiLineComment
        try self.consumeMultiLineCommentChars();
    } else {
        return null;
    }

    if (self.config.source_type == .script) {
        // After eating the comment:
        // 1. Eat all trailing whitespaces.
        // 2. Then, look for an HTML comment close "-->"
        const ws_len = matchWhiteSpaces(self.source[self.index..]);
        const remaining = self.source[self.index + ws_len ..];
        if ((self.line > start_line or self.is_trivial_line) and std.mem.startsWith(u8, remaining, "-->")) {
            self.index += ws_len; // consume whitespaces
            self.index += 3; // consume "-->"
            try self.consumeSingleLineCommentChars();
            return .{
                .start = start,
                .len = self.index - start,
                .tag = .comment,
                .line = start_line,
            };
        }
    }

    return .{
        .start = start,
        .len = self.index - start,
        .tag = .comment,
        .line = start_line,
    };
}

fn matchWhiteSpaces(str: []const u8) u32 {
    var i: usize = 0;
    while (i < str.len) {
        const b = str[i];
        if (std.ascii.isAscii(b)) {
            if (isSimpleWhitespace(b)) {
                i += 1;
            } else {
                break;
            }
        } else {
            const code_point = utf8.codePointAt(str, i);
            if (isMultiByteSpace(code_point.value)) {
                i += code_point.len;
            } else {
                break;
            }
        }
    }
    return @intCast(i);
}

/// If the remaining source starts with a "<!--",
/// consume a single line HTML comment and return it.
/// Otherwise, return null.
/// https://tc39.es/ecma262/#prod-annexB-SingleLineHTMLOpenComment
fn singleLineHtmlCommentOpen(self: *Self) Error!?Token {
    assert(self.source[self.index] == '<');
    const start = self.index;

    if (std.mem.startsWith(u8, self.source[self.index..], "<!--")) {
        self.index += 4;
        try self.consumeSingleLineCommentChars();
        return .{
            .start = start,
            .len = self.index - start,
            .tag = .comment,
            .line = self.line,
        };
    }

    return null;
}

/// If the remaining source starts with a "-->",
/// consume a single line HTML comment and return it.
/// Otherwise, return null.
/// https://tc39.es/ecma262/#prod-annexB-SingleLineHTMLCloseComment
fn singleLineHtmlCommentClose(self: *Self) Error!?Token {
    assert(self.source[self.index] == '-');
    // "-->" is only allowed on a line if all preceding characters
    // are whitespaces or part of a comment.
    if (!self.is_trivial_line) return null;

    if (std.mem.startsWith(u8, self.source[self.index..], "-->")) {
        const start = self.index;
        self.index += 3;
        try self.consumeSingleLineCommentChars();
        return .{
            .start = start,
            .len = self.index - start,
            .tag = .comment,
            .line = self.line,
        };
    }

    return null;
}

/// Consume all characters until a newline or EOF is seen.
fn consumeSingleLineCommentChars(self: *Self) Error!void {
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (isNewline(byte)) {
                self.index += 1;
                self.bumpLine();
                break;
            }
            self.index += 1;
        } else {
            const code_point = utf8.codePointAt(self.source, self.index);
            self.index += code_point.len;
            if (isNewline(code_point.value)) {
                self.bumpLine();
                break;
            }
        }
    }
}

/// Consume all source characters until EOF or a '*/' sequence is found.
/// returns `Error.UnterminatedComment` on EOF.
fn consumeMultiLineCommentChars(self: *Self) Error!void {
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            self.index += 1;
            if (byte == '*' and self.peekByte() == '/') {
                self.index += 1; // eat the '/'
                return;
            } else if (isNewline(byte)) {
                // TODO: what if its a /r/n?
                self.bumpLine();
            }
        } else {
            const code_point = utf8.codePointAt(self.source, self.index);
            self.index += code_point.len;
            if (isNewline(code_point.value)) {
                self.bumpLine();
            }
        }
    }

    return Error.UnterminatedComment;
}

/// Consume a UTF-8 codepoint from the source string.
/// This function advances the `self.index` pointer by the length of the next code-point.
fn consumeUtf8CodePoint(self: *Self) Error!void {
    const code_point_len =
        std.unicode.utf8ByteSequenceLength(self.source[self.index]) catch
            return Error.BadEscapeSequence;
    self.index += code_point_len;
}

/// Eat an escaped character inside a regex literal.
/// Implements the RegularExpressionBackslashSequence production.
/// https://tc39.es/ecma262/#prod-RegularExpressionBackslashSequence
/// This function advances the `self.index` pointer.
fn consumeRegexEscape(self: *Self) Error!void {
    assert(self.source[self.index] == '\\');
    if (self.index + 1 >= self.source.len)
        return Error.UnexpectedEof;

    self.index += 1; // skip '\'
    const byte = self.source[self.index];
    if (std.ascii.isAscii(byte)) {
        if (isNewline(byte))
            return Error.BadRegexLiteral;
        self.index += 1;
    } else {
        try self.consumeUtf8CodePoint();
    }
}

/// Consume a regex character class like "[a-zA-Z_]".
/// This function advances the `self.index` pointer.
fn consumeRegexCharacterClass(self: *Self) Error!void {
    assert(self.source[self.index] == '[');

    while (!self.eof()) {
        // Consume a RegularExpressionClassChar:
        // https://tc39.es/ecma262/#prod-RegularExpressionClassChar
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (isNewline(byte)) {
                return Error.BadRegexLiteral;
            }

            if (byte == '\\') {
                try self.consumeRegexEscape();
            } else {
                self.index += 1;
                // reached end of character class.
                if (byte == ']') return;
            }
        } else {
            try self.consumeUtf8CodePoint();
        }
    }

    return Error.UnterminatedRegexClass;
}

/// Consume part of a regex.
/// Implements 'RegularExpressionFirstChar' in the lexical grammar:
/// https://tc39.es/ecma262/#prod-RegularExpressionFirstChar
/// This function advances the `self.index` pointer.
fn consumeRegexPart(self: *Self) Error!void {
    const byte = self.source[self.index];
    if (std.ascii.isAscii(byte)) {
        @branchHint(.likely);
        if (isNewline(byte))
            return Error.BadRegexLiteral;

        if (byte == '\\') {
            try self.consumeRegexEscape();
        } else if (byte == '[') {
            // regex character class.
            try self.consumeRegexCharacterClass();
        } else {
            self.index += 1; // regular ASCII char.
        }
    } else {
        try self.consumeUtf8CodePoint();
    }
}

/// Consume regex flag characters after the closing '/', if any
/// https://tc39.es/ecma262/#prod-RegularExpressionFlags
fn consumeRegexFlags(self: *Self) Error!void {
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (canCodepointContinueId(byte))
                self.index += 1
            else
                break;
        } else {
            const code_point = utf8.codePointAt(self.source, self.index);
            if (canCodepointContinueId(code_point.value)) {
                self.index += code_point.len;
            } else {
                break;
            }
        }
    }
}

/// Parse a regex literal.
/// https://tc39.es/ecma262/#prod-RegularExpressionLiteral
fn regexLiteral(self: *Self) Error!Token {
    assert(self.source[self.index] == '/');

    const start = self.index;
    const line = self.line;

    self.index += 1; // eat '/'
    if (self.eof()) return Error.UnexpectedEof;

    while (!self.eof()) {
        // closing '/'
        if (self.source[self.index] == '/') {
            self.index += 1;
            try self.consumeRegexFlags();
            break;
        }

        try self.consumeRegexPart();
    } else {
        return Error.UnterminatedRegexLiteral;
    }

    return Token{
        .tag = .regex_literal,
        .start = start,
        .len = self.index - start,
        .line = line,
    };
}

fn consumeEscape(self: *Self) !void {
    if (self.index + 1 >= self.source.len or
        self.source[self.index] != '\\')
        return Error.BadEscapeSequence;

    switch (self.source[self.index + 1]) {
        'x' => {
            // \xXX
            if (self.index + 4 < self.source.len and
                (std.ascii.isHex(self.source[self.index + 2]) and
                    std.ascii.isHex(self.source[self.index + 3])))
            {
                self.index += 4;
            } else {
                return Error.BadEscapeSequence;
            }
        },
        'u' => {
            // \uXXXX or \u{X} - \u{XXXXXX}
            const parsed = utf8.parseUnicodeEscape(self.source[self.index..]) orelse
                return Error.BadEscapeSequence;
            self.index += parsed.len;
        },
        else => {
            self.index += 1; // skip '/'
            const byte = self.source[self.index];
            if (std.ascii.isAscii(byte)) {
                self.index += 1;
                switch (byte) {
                    '\n' => self.bumpLine(),
                    '\r' => {
                        self.bumpLine();
                        if (self.peekByte() == '\n') self.index += 1; // eat '\n' after '\r'
                    },
                    // Check for legacy octal escape sequences,
                    // and disallow them in strict mode.
                    // https://tc39.es/ecma262/#prod-LegacyOctalEscapeSequence
                    //
                    //  LegacyOctalEscapeSequence :
                    //     0 [lookahead ∈ { 8, 9 }]
                    //     NonZeroOctalDigit [lookahead ∉ OctalDigit]
                    //     ZeroToThree OctalDigit [lookahead ∉ OctalDigit]
                    //     FourToSeven OctalDigit
                    //     ZeroToThree OctalDigit OctalDigit
                    '0' => {
                        // 0 [lookahead ∈ { 8, 9 }]
                        const lookahead = self.peekByte() orelse return;
                        if ('0' <= lookahead and lookahead <= '7') {
                            if (self.is_in_strict_mode)
                                return Error.OctalEscapeInStrictMode;
                            return;
                        }
                    },

                    '1'...'7' => {
                        // NonZeroOctalDigit [lookahead ∉ OctalDigit]
                        const lookahead = self.peekByte() orelse return;
                        if (!('0' <= lookahead and lookahead <= '7')) {
                            if (self.is_in_strict_mode)
                                return Error.OctalEscapeInStrictMode;
                        }
                    },

                    else => {},
                }
                // Legacy octal escape sequence
            } else {
                try self.consumeUtf8CodePoint();
            }
        },
    }
}

fn stringLiteral(self: *Self) Error!Token {
    const start = self.index;
    const start_line = self.line;

    const quote_char: u8 = if (self.source[self.index] == '\'')
        '\''
    else
        '"';

    self.index += 1; // eat opening quote
    var found_end_quote = false;
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (!std.ascii.isAscii(byte)) {
            // most code will be ASCII
            @branchHint(.cold);
            try self.consumeUtf8CodePoint();
            continue;
        }

        // ASCII char.
        if (byte == '\\') {
            try self.consumeEscape();
        } else {
            self.index += 1;
            if (byte == quote_char) {
                found_end_quote = true;
                break;
            } else if (isNewline(byte)) {
                return Error.UnterminatedString;
            }
        }
    }

    if (!found_end_quote) {
        return Error.UnterminatedString;
    }

    return .{
        .tag = .string_literal,
        .start = start,
        .len = self.index - start,
        .line = start_line,
    };
}

fn templateAfterInterpolation(self: *Self) Error!Token {
    if (self.eof()) return Error.NonTerminatedTemplateLiteral;
    if (self.source[self.index] != '}')
        return Error.UnexpectedByte;

    const start = self.index;
    const line = self.line;

    self.index += 1; // eat '}'
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (!std.ascii.isAscii(byte)) {
            try self.consumeUtf8CodePoint();
            continue;
        }

        switch (byte) {
            '$' => {
                self.index += 1; // $
                if (!self.eof() and self.source[self.index] == '{') {
                    self.index += 1; // {
                    return Token{
                        .tag = .template_literal_part,
                        .start = start,
                        .len = self.index - start,
                        .line = line,
                    };
                }
            },

            '`' => {
                self.index += 1;
                return Token{
                    .tag = .template_literal_part,
                    .start = start,
                    .len = self.index - start,
                    .line = line,
                };
            },
            '\\' => try self.consumeEscape(),
            else => self.index += 1,
        }
    }

    return Error.NonTerminatedTemplateLiteral;
}

/// Parses the beginning of a template literal,
/// starting from the opening '`' character.
fn startTemplateLiteral(self: *Self) Error!Token {
    assert(self.source[self.index] == '`');
    const start = self.index;
    const line = self.line;

    self.index += 1; // eat '`'
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (!std.ascii.isAscii(byte)) {
            try self.consumeUtf8CodePoint();
            continue;
        }

        switch (byte) {
            '$' => {
                self.index += 1; // $
                if (!self.eof() and self.source[self.index] == '{') {
                    self.index += 1; // {
                    return Token{
                        .tag = .template_literal_part,
                        .start = start,
                        .len = self.index - start,
                        .line = line,
                    };
                }
            },

            '`' => {
                self.index += 1;
                return Token{
                    .tag = .template_literal_part,
                    .start = start,
                    .len = self.index - start,
                    .line = line,
                };
            },
            '\\' => try self.consumeEscape(),
            else => self.index += 1,
        }
    }

    return Error.NonTerminatedTemplateLiteral;
}

fn canCodepointStartId(cp: u21) bool {
    return cp == '_' or cp == '$' or unicode_id.canStartId(cp);
}

fn canCodepointContinueId(cp: u21) bool {
    return cp == '$' or unicode_id.canContinueId(cp);
}

/// A character in an identifier
const IdCharKind = enum {
    /// An ASCII byte
    ascii,
    /// An escape code or a UTF-8 code point
    non_ascii,
    /// Not a valid identifier character
    invalid,
};

/// If the next ASCII char or UTF-8 code point is a valid identifier
/// start, consume it and return whether its an ASCII char.
/// Otherwise, return "invalid" and do not advance in the input.
/// https://tc39.es/ecma262/#prod-IdentifierStartChar
fn matchIdentifierStart(self: *Self) Error!IdCharKind {
    const byte = self.source[self.index];
    if (std.ascii.isAscii(byte)) {
        if (canCodepointStartId(byte)) {
            self.index += 1;
            return .ascii;
        }

        if (byte != '\\') return .invalid;

        const s = self.source[self.index..];
        const parsed = utf8.parseUnicodeEscape(s) orelse
            return .invalid;

        if (canCodepointStartId(parsed.codepoint)) {
            self.index += parsed.len;
            return .non_ascii;
        }

        return .invalid;
    }

    const cp = utf8.codePointAt(self.source, self.index);
    if (canCodepointStartId(cp.value)) {
        self.index += cp.len;
        return .non_ascii;
    }

    return .invalid;
}

/// If the code point form a valid identifier part, consume
/// it and advance the `self.index` pointer.
///
/// https://tc39.es/ecma262/#prod-IdentifierPartChar
///
/// Returns `true` if a code point was consumed.
fn matchIdentifierContt(self: *Self) Error!bool {
    const byte = self.source[self.index];
    if (std.ascii.isAscii(byte)) {
        // A-Za-z0-9$_
        if (canCodepointContinueId(byte)) {
            self.index += 1;
            return true;
        }

        // unicode escape sequence.
        if (byte != '\\') return false;
        const s = self.source[self.index..];
        const parsed = utf8.parseUnicodeEscape(s) orelse
            return Error.MalformedIdentifier;

        if (canCodepointContinueId(parsed.codepoint)) {
            self.index += parsed.len;
            return true;
        }

        return false;
    } else {
        const cp = utf8.codePointAt(self.source, self.index);
        if (canCodepointContinueId(cp.value)) {
            self.index += cp.len;
            return true;
        }

        return false;
    }
}

/// Parse an identifier token.
/// When [check_for_keywords] is `true`, this function will check if the identifier
/// is a reserved keyword and return the appropriate token tag instead.
///
/// Otherwise, it will treat all lexemes as identifiers (with the tag set to .non_ascii_identifier
/// or .identifier).
fn identifier(self: *Self) Error!Token {
    const start = self.index;
    const str = self.source[start..];

    const start_char_kind = try matchIdentifierStart(self);
    if (start_char_kind == .invalid) {
        return Error.MalformedIdentifier;
    }

    const token_tag = try self.matchIdentifierTail(start_char_kind);
    const len = self.index - start;

    // TODO: ensure its not an escaped keyword.
    // To do this well, add a 'escaped_code_point' member in the IdCharKind
    // enum.
    const id_str = str[0..len];
    if (len >= 2 and len <= 12) {
        for (0.., js_keywords) |i, kw| {
            if (std.mem.eql(u8, id_str, kw)) {
                return Token{
                    .start = start,
                    .len = @intCast(len),
                    .tag = all_kw_tags[i],
                    .line = self.line,
                };
            }
        }
    }

    return Token{
        .start = start,
        .len = @intCast(len),
        .tag = token_tag,
        .line = self.line,
    };
}

/// After the first character of an identifier has been consumed, this function
/// eats the rest of the identifier lexeme.
///
/// [first_char_kind]: Kind of the first character (.ascii or .non_ascii).
///
/// Returns the token kind of the identifier.
fn matchIdentifierTail(self: *Self, first_char_kind: IdCharKind) Error!Token.Tag {
    var token_kind = if (first_char_kind == .ascii)
        Token.Tag.identifier
    else
        Token.Tag.non_ascii_identifier;

    // First, eat all the non-escaped ascii chars in the identifier name
    // in one loop. This is the "hot" path, as most identifiers will be normal ascii.
    if (first_char_kind == .ascii) {
        @branchHint(.likely);
        while (!self.eof() and
            isAsciiIdentifierContt(self.source[self.index])) : (self.index += 1)
        {}
    }

    // Then, eat the rest of the identifier name which may contain
    // non-ascii characters and escape sequences.
    if (!self.eof() and try self.matchIdentifierContt()) {
        @branchHint(.cold);
        token_kind = .non_ascii_identifier;
        while (!self.eof() and try self.matchIdentifierContt()) {}
    }

    return token_kind;
}

/// Assuming the current character is a '\',
/// consume an escaped character inside an identifier.
fn matchIdentifierEscape(self: *Self) Error!void {
    const s = self.source[self.index..];
    const parsed = utf8.parseUnicodeEscape(s) orelse
        return Error.MalformedIdentifier;

    if (canCodepointContinueId(parsed.codepoint)) {
        self.index += parsed.len;
    } else {
        return Error.MalformedIdentifier;
    }
}

/// Returns whether the ASCII byte [ch] can appear in the middle of an identifier.
fn isAsciiIdentifierContt(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '$';
}

/// https://262.ecma-international.org/15.0/index.html#prod-PrivateIdentifier
fn privateIdentifier(self: *Self) Error!Token {
    const start = self.index;
    self.index += 1; // eat '#'
    const id = try self.identifier();
    const tag: Token.Tag = if (id.tag == .identifier)
        .private_identifier
    else
        .private_non_ascii_identifier;

    return Token{
        .start = start,
        .len = id.len + 1,
        .tag = tag,
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

            else => return Error.BadPunctuator,
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
/// Also returns a boolean flag indicating whether the literal is an octal number
/// (only contains numbers 0-7, and starts with a 0)
fn matchDecimalIntegerLiteral(str: []const u8) ?struct {
    is_octal: bool,
    len: u32,
} {
    if (str.len == 0) return null;

    var i: u32 = 0;
    var is_octal = true;
    while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {
        if (i + 2 < str.len and str[i + 1] == '_' and std.ascii.isDigit(str[i + 2])) {
            i += 1;
        }
        is_octal = is_octal and str[i] <= '7';
    }

    // Check if:
    // 1. The first digit was '0'.
    // 2. The number is not just a single '0'.
    // 3. All other digits are in the octal range.
    is_octal = is_octal and str[0] == '0' and i > 1;
    return .{ .is_octal = is_octal, .len = i };
}

/// https://262.ecma-international.org/15.0/index.html#prod-grammar-notation-HexIntegerLiteral
fn matchHexDigits(str: []const u8) ?u32 {
    var i: u32 = 0;
    while (i < str.len and std.ascii.isHex(str[i])) : (i += 1) {
        if (i + 2 < str.len and
            str[i + 1] == '_' and
            std.ascii.isHex(str[i + 2]))
        {
            i += 1;
        }
    }

    return if (i > 0) i else null;
}

fn isOctalDigit(ch: u8) bool {
    return ch >= '0' and ch <= '7';
}

fn matchOctalDigits(str: []const u8) ?u32 {
    var i: u32 = 0;
    while (i < str.len and isOctalDigit(str[i])) : (i += 1) {
        if (i + 2 < str.len and
            str[i + 1] == '_' and
            isOctalDigit(str[i + 2]))
        {
            i += 1;
        }
    }

    return if (i > 0) i else null;
}

fn matchBinaryDigits(str: []const u8) ?u32 {
    var i: u32 = 0;
    while (i < str.len and (str[i] == '0' or str[i] == '1')) : (i += 1) {
        if (i + 2 < str.len and
            str[i + 1] == '_' and
            (str[i + 2] == '0' or str[i + 2] == '1'))
            i += 1;
    }

    return if (i > 0) i else null;
}

/// https://tc39.es/ecma262/#prod-DecimalLiteral
fn matchDecimalPart(str: []const u8) ?u32 {
    if (str[0] != '.') return null;

    var i: u32 = 1;
    while (i < str.len and std.ascii.isDigit(str[i])) : (i += 1) {
        if (i + 2 < str.len and str[i + 1] == '_' and std.ascii.isDigit(str[i + 2]))
            i += 1;
    }
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
    if (!std.ascii.isDigit(str[i - 1])) return null;

    return i;
}

/// https://tc39.es/ecma262/#prod-NumericLiteral
fn numericLiteral(self: *Self) !Token {
    const start = self.index;
    const str = self.source[start..];

    const len: u32, const toktype: Token.Tag = blk: {
        if (str[0] == '.') {
            var decimal_len = matchDecimalPart(str) orelse
                return Error.InvalidNumericLiteral;
            decimal_len += matchExponentPart(str[decimal_len..]) orelse 0;

            // Just "." is not a valid numeric literal.
            if (decimal_len == 1) return Error.InvalidNumericLiteral;
            break :blk .{ decimal_len, .decimal_literal };
        }

        if (str.len > 2 and str[0] == '0') {
            switch (str[1]) {
                'x', 'X' => {
                    const hex_len: u32 = matchHexDigits(str[2..]) orelse
                        return Error.InvalidHexLiteral;
                    break :blk .{ hex_len + 2, .hex_literal };
                },

                'o', 'O' => {
                    const octal_len: u32 = matchOctalDigits(str[2..]) orelse
                        return Error.InvalidNumericLiteral;
                    break :blk .{ octal_len + 2, .octal_literal };
                },

                'b', 'B' => {
                    const binary_len: u32 = matchBinaryDigits(str[2..]) orelse
                        return Error.InvalidBinaryLiteral;
                    break :blk .{ binary_len + 2, .binary_literal };
                },

                else => {},
            }
        }

        const decimal_match = matchDecimalIntegerLiteral(str) orelse
            return Error.InvalidNumericLiteral;

        var decimal_len = decimal_match.len;
        if (decimal_match.is_octal) {
            // Octal literals starting with "0" are not allowed to have a decimal part.
            // 0012.5 is invalid (octal literal followed by decimal part)
            // 0088.5 is valid (decimal literal followed by decimal part)
            break :blk .{ decimal_len, .legacy_octal_literal };
        }

        if (decimal_len < str.len) // '.' decimal_digits
            decimal_len += matchDecimalPart(str[decimal_len..]) orelse 0;
        if (decimal_len < str.len) // 'e' exponent_digits
            decimal_len += matchExponentPart(str[decimal_len..]) orelse 0;

        break :blk .{ decimal_len, .decimal_literal };
    };

    self.index += len;

    // number must not be immediately followed by identifier
    if (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (canCodepointStartId(byte) or std.ascii.isDigit(byte)) {
                return Error.InvalidNumericLiteral;
            }
        } else {
            const code_point = utf8.codePointAt(self.source, self.index);
            if (canCodepointStartId(code_point.value)) {
                return Error.InvalidNumericLiteral;
            }
        }
    }

    return Token{
        .tag = toktype,
        .start = start,
        .len = len,
        .line = self.line,
    };
}

pub fn eof(self: *const Self) bool {
    return self.index >= self.source.len;
}

pub fn bumpLine(self: *Self) void {
    self.line += 1;
    // We haven't seen any non-whitespace and non-comment
    // tokens on this new line yet.
    self.is_trivial_line = true;
}

/// Return the next u8 from the source string
fn nextByte(self: *Self) Error!u8 {
    const byte = self.peekByte() orelse
        return Error.UnexpectedEof;
    self.index += 1;
    return byte;
}

fn peekByte(self: *Self) ?u8 {
    if (self.eof()) return null;
    return self.source[self.index];
}

fn isSimpleWhitespace(ch: u8) bool {
    return switch (ch) {
        ' ',
        '\t',
        '\u{000B}',
        '\u{000C}',
        '\u{00A0}',
        => true,
        else => false,
    };
}

fn isMultiByteSpace(ch: u21) bool {
    return switch (ch) {
        '\u{FEFF}',
        '\u{00A0}',
        '\u{2000}',
        '\u{2001}'...'\u{200A}',
        '\u{202F}',
        '\u{205F}',
        '\u{3000}',
        '\u{1680}',
        => true,
        else => false,
    };
}

/// Consume all whitespace characters, including newlines,
/// and return a token representing that span.
/// Whitespace tokens are preserved in the Jam toolkit to
/// re-construct the original source code.
fn whiteSpaces(self: *Self) Error!Token {
    const start = self.index;
    const start_line = self.line;

    while (!self.eof()) {
        const ch = self.source[self.index];

        if (!std.ascii.isAscii(ch)) {
            // check for Non-ASCII UTF-8 newline or whitespace char.
            const cp = utf8.codePointAt(self.source, self.index);
            if (cp.value == '\u{2028}' or cp.value == '\u{2029}') {
                self.index += cp.len;
                self.bumpLine();
                continue;
            }

            // <ZWNSBP>, and all UTF-8 code-points
            // with the property 'White_Space=yes'
            if (isMultiByteSpace(cp.value)) {
                self.index += cp.len;
                continue;
            }
            break;
        }

        // Whitespaces that are single byte UTF-8 code-points
        if (isSimpleWhitespace(ch)) {
            self.index += 1;
        } else if (isNewline(ch)) {
            self.index += 1;
            self.bumpLine();

            if (ch == '\r' and
                self.index + 1 < self.source.len and
                self.source[self.index + 1] == '\n')
            {
                // skip /r/n
                self.index += 1;
            }

            continue;
        } else {
            // reached a non-whitespace character.
            break;
        }
    }

    return Token{
        .tag = .whitespace,
        .start = start,
        .len = self.index - start,
        .line = start_line,
    };
}

const t = std.testing;

fn testToken(src: []const u8, tag: Token.Tag) !void {
    // first, test that token followed by EOF
    {
        var tokenizer = try Self.init(src, .{});
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
        var tokenizer = try Self.init(source, .{});
        var token = try tokenizer.next(); // skip leading whitespace
        token = try tokenizer.next();

        try std.testing.expectEqualDeep(Token{
            .tag = tag,
            .start = 2,
            .len = @intCast(src.len),
            .line = 1,
        }, token);
    }
}

fn testTokenError(str: []const u8, err: anyerror) !void {
    var tokenizer = Self.init(str, .{}) catch |init_error| {
        try t.expectEqual(err, init_error);
        return;
    };
    const token_or_err = tokenizer.next();
    try t.expectError(err, token_or_err);
}

fn testTokenList(source: []const u8, expected_tokens: []const Token.Tag) !void {
    var tokenizer = try Self.init(source, .{});
    for (0.., expected_tokens) |i, expected| {
        const token = try tokenizer.next();
        t.expectEqual(expected, token.tag) catch |err| {
            std.debug.print("Failed to match token #{d} on input: {s}", .{ i, source });
            return err;
        };
    }
    try t.expectEqual(.eof, (try tokenizer.next()).tag);
}

test Self {
    const test_cases = [_]struct { []const u8, Token.Tag }{
        .{ "$one", .identifier },
        .{ "$two$", .identifier },
        .{ "$123", .identifier },
        .{ "fooobar", .identifier },
        .{ "#fooobar", .private_identifier },
        .{ "ಠ_ಠ", .non_ascii_identifier },
        .{ "\\u{105}bc", .non_ascii_identifier },
        .{ "\\u{105}\\u{5f}", .non_ascii_identifier },
        .{ "\\u{105}\\u005f", .non_ascii_identifier },
        .{ "0", .decimal_literal },
        .{ "120", .decimal_literal },
        .{ "1_000_000", .decimal_literal },
        .{ "1.5_1_1", .decimal_literal },
        .{ "1_00_0_000", .decimal_literal },
        .{ "123", .decimal_literal },
        .{ "1.5", .decimal_literal },
        .{ "1.523", .decimal_literal },
        .{ "1_000_000.523", .decimal_literal },
        .{ "0x55ffee1", .hex_literal },
        .{ "0b1011_0101", .binary_literal },
        .{ "0b1", .binary_literal },
        .{ "0x5", .hex_literal },
        .{ "55e+1", .decimal_literal },
        .{ "55e-1", .decimal_literal },
        .{ "55e112", .decimal_literal },
        .{ "055", .legacy_octal_literal },
        .{ "0XF", .hex_literal },
        .{ ".1", .decimal_literal },
        .{ ".33", .decimal_literal },
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
        .{ "'\\\n'", .string_literal },
        .{ "'\u{00000000078}'", .string_literal },
        .{ "'\\u{95}world!'", .string_literal },
        .{ "'\\u{95}world\\{105}'", .string_literal },
        .{ "'\\xFFworld\\{105}'", .string_literal },
        .{ "\"\\xFFworld\\{105}\"", .string_literal },
        .{ "`hello`", .template_literal_part },
        .{ "// test comment", .comment },
        .{ "//", .comment },
        .{ "// else", .comment },
        .{ "else", .kw_else },
        .{ "\\u0065lse", .non_ascii_identifier },
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
        var tokenizer = try Self.init("", .{});
        try std.testing.expectEqualDeep(Token{
            .tag = .eof,
            .start = 0,
            .len = 0,
            .line = 0,
        }, try tokenizer.next());
    }

    const bad_cases = [_]struct { []const u8, anyerror }{
        .{ "'hello", Error.UnterminatedString },
        .{ "1._5", Error.InvalidNumericLiteral },
        .{ "1.5_", Error.InvalidNumericLiteral },
        .{ "1.5aaaA", Error.InvalidNumericLiteral },
        .{ "1__1", Error.InvalidNumericLiteral },
        .{ "0xg1", Error.InvalidHexLiteral },
    };

    for (bad_cases) |case| {
        const text, const expected_err = case;
        testTokenError(text, expected_err) catch |err| {
            std.debug.print("tokenizing {s} did not raise {any}\n", .{ text, err });
            return err;
        };
    }

    {
        var tokenizer = try Self.init("123.00 + .333", .{});
        try t.expectEqual(Token.Tag.decimal_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"+", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.decimal_literal, (try tokenizer.next()).tag);
    }

    {
        var tokenizer = try Self.init(
            \\/* this is a
            \\ multiline
            \\ comment */
        , .{});
        const comment_token = try tokenizer.next();
        try t.expectEqual(.comment, comment_token.tag);
        try t.expectEqualDeep(
            tokenizer.source,
            comment_token.toByteSlice(tokenizer.source),
        );
        try t.expectEqual(2, tokenizer.line);
    }

    {
        var tokenizer = try Self.init("f([1])", .{});
        try t.expectEqual(Token.Tag.identifier, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"(", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"[", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.decimal_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"]", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@")", (try tokenizer.next()).tag);
    }
    {
        var tokenizer = try Self.init(&.{ 40, 39, 92, 226, 128, 169, 39, 41 }, .{});
        try t.expectEqual(Token.Tag.@"(", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.string_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@")", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.eof, (try tokenizer.next()).tag);
    }

    try testTokenList("f([1])", &[_]Token.Tag{
        Token.Tag.identifier,
        Token.Tag.@"(",
        Token.Tag.@"[",
        Token.Tag.decimal_literal,
        Token.Tag.@"]",
        Token.Tag.@")",
    });

    {
        var tokenizer = try Self.init(" /a\\(bc[some_character_class]/g //foo", .{});
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);

        // by default  '/' is interpreted as a division operator.
        const div_token = try tokenizer.next();
        try t.expectEqual(.@"/", div_token.tag);

        // Then it can be re-scanned as a regex literal.
        const regex_token = try tokenizer.reScanRegexLiteral(&div_token);
        try t.expectEqual(.regex_literal, regex_token.tag);

        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.comment, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.eof, (try tokenizer.next()).tag);
    }

    {
        var tokenizer = try Self.init("`hello ${'world'}`", .{});
        try t.expectEqual(.template_literal_part, (try tokenizer.next()).tag);
        try t.expectEqual(.string_literal, (try tokenizer.next()).tag);
        const rb_token = try tokenizer.next();
        try t.expectEqual(.@"}", rb_token.tag);
        const template_part = try tokenizer.reScanTemplatePart(&rb_token);
        try t.expectEqual(.template_literal_part, template_part.tag);
        try t.expectEqual(.eof, (try tokenizer.next()).tag);
    }
}
