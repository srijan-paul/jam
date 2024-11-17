const std = @import("std");
const unicode_id = @import("unicode-id");
const Token = @import("token.zig").Token;

const util = @import("util");
const parser = @import("parser.zig");

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
    BadEscapeSequence,
    NonTerminatedString,
    NonTerminatedTemplateLiteral,
    MalformedIdentifier,
};

// zig fmt: off
pub const all_keywords = [_][]const u8{
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

pub const all_kw_tags: [all_keywords.len]Token.Tag = makeKwTagArray();
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

/// Source string to tokenize.
source: []const u8,
/// Byte offset into `source`
index: u32 = 0,
/// Current line number (0 indexed).
line: u32 = 0,
/// Current column number (0 indexed)
col: u32 = 0,
/// When `true`, the tokenizer will attempt to parse any remaining input
/// starting with '/' (that isn't a comment starter) as a regex literal (e.g: '/[a-zA-Z0-9]/').
/// Otherwise, '/' (when not starting a comment) is assumed to be either the '/' or '/=' operator.
/// This property is used to dis-ambiguate between division operators and regex literals.
assume_bslash_starts_regex: bool = false,

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

pub fn init(source: []const u8, config: parser.Config) Error!Self {
    if (!std.unicode.utf8ValidateSlice(source))
        return Error.InvalidUtf8;

    return Self{
        .source = source,
        .config = config,
        .is_in_strict_mode = config.strict_mode or config.source_type == .module,
    };
}

pub fn next(self: *Self) Error!Token {
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

            // Parsing regex literals is awkward.
            // A '/abc' can either be the start of a regex literal,
            // or a '/' (division) token followed by an 'abc' (identifier) token.
            //
            // The parser has to tell the tokenizer what it expects
            // to see next. If it expects to see a literal, then
            // we want to try tokenizing a regex literal.
            // Otherwise, we look for '/' or '/='.
            if (self.assume_bslash_starts_regex)
                return try self.regexLiteral();
            return try self.punctuator();
        },
        ' ', '\t', '\n', '\r' => return self.whiteSpaces(),
        '}' => {
            if (self.assume_rbrace_is_template_part)
                return try self.templateAfterInterpolation();

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

        '[',
        ']',
        '(',
        ')',
        ',',
        ';',
        ':',
        '>',
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
        else => return try self.identifier(),
    }
}

/// Rewind the tokenizer to a previous location in the source code.
/// NOTE: the location given must be a valid index into the source code.
/// Rewinding the tokenizer to an invalid location, such as the middle of UTF-8 codepoint
/// results in undefined behavior.
pub fn rewind(self: *Self, index: u32, line: u32) void {
    self.index = index;
    self.line = line;
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

    return .{
        .start = start,
        .len = self.index - start,
        .tag = .comment,
        .line = start_line,
    };
}

/// If the remaining source starts with a "<!--",
/// consume a single line HTML comment and return it.
/// Otherwise, return null.
/// https://tc39.es/ecma262/#prod-annexB-SingleLineHTMLOpenComment
fn singleLineHtmlCommentOpen(self: *Self) Error!?Token {
    std.debug.assert(self.source[self.index] == '<');
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

/// Consume all characters until a newline or EOF is seen.
fn consumeSingleLineCommentChars(self: *Self) Error!void {
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (isNewline(byte)) {
                self.line += 1;
                break;
            }
            self.index += 1;
        } else {
            const code_point = util.utf8.codePointAt(self.source, self.index);
            self.index += code_point.len;
            if (isNewline(code_point.value)) {
                self.line += 1;
                break;
            }
        }
    }
}

/// Consume all source characters until EOF or a '*/' sequence is found.
fn consumeMultiLineCommentChars(self: *Self) Error!void {
    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            if (byte == '*') {
                if (self.index + 1 < self.source.len and self.source[self.index + 1] == '/') {
                    self.index += 2;
                    break;
                }
            } else if (isNewline(byte)) {
                // TODO: what if its a /r/n?
                self.line += 1;
            }

            self.index += 1;
        } else {
            const code_point = util.utf8.codePointAt(self.source, self.index);
            self.index += code_point.len;
            if (isNewline(code_point.value)) {
                self.line += 1;
            }
        }
    }
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
    std.debug.assert(self.source[self.index] == '\\');
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
    std.debug.assert(self.source[self.index] == '[');

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
            const code_point = util.utf8.codePointAt(self.source, self.index);
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
    std.debug.assert(self.source[self.index] == '/');

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
            const parsed = util.utf8.parseUnicodeEscape(self.source[self.index..]) orelse
                return Error.BadEscapeSequence;
            self.index += parsed.len;
        },
        else => {
            self.index += 1; // skip '/'
            const byte = self.source[self.index];
            if (std.ascii.isAscii(byte)) {
                self.index += 1;
                if (isNewline(byte)) self.line += 1;
            } else {
                try self.consumeUtf8CodePoint();
            }
        },
    }
}

fn parseEscape(self: *Self, iter: *std.unicode.Utf8Iterator) !void {
    const str = iter.bytes[iter.i..];
    if (str.len < 2 or str[0] != '\\')
        return Error.BadEscapeSequence;

    switch (str[1]) {
        'x' => {
            // \xXX
            if (str.len >= 4 and std.ascii.isHex(str[2]) and std.ascii.isHex(str[3])) {
                iter.i += 4;
            } else {
                return Error.BadEscapeSequence;
            }
        },
        'u' => {
            // \uXXXX or \u{X} - \u{XXXXXX}
            const parsed = util.utf8.parseUnicodeEscape(str) orelse
                return Error.BadEscapeSequence;
            iter.i += parsed.len;
        },
        else => {
            iter.i += 1; // skip /
            const cp = iter.nextCodepoint() orelse return Error.InvalidUtf8;
            if (isNewline(cp)) self.line += 1;
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
            }
        }
    }

    if (!found_end_quote) {
        return Error.NonTerminatedString;
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
    std.debug.assert(self.source[self.index] == '`');
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

/// Advance the iterator by one or more code points,
/// and check if the code point(s) form a valid identifier start.
/// https://tc39.es/ecma262/#prod-IdentifierStartChar
fn matchIdentifierStart(
    self: *Self,
    it: *std.unicode.Utf8Iterator,
) Error!bool {
    if (util.utf8.parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
        if (canCodepointStartId(parsed.codepoint)) {
            self.index += parsed.len;
            return true;
        } else {
            return false;
        }
    }

    const codepoint = it.nextCodepoint() orelse
        return Error.InvalidUtf8; // bad utf-8 input.
    return canCodepointStartId(codepoint);
}

/// Advance the iterator by one or more code points,
/// and check if the code point(s) form a valid identifier part.
/// https://tc39.es/ecma262/#prod-IdentifierPartChar
fn matchIdentifierContt(it: *std.unicode.Utf8Iterator) Error!bool {
    if (util.utf8.parseUnicodeEscape(it.bytes[it.i..])) |parsed| {
        if (canCodepointContinueId(parsed.codepoint)) {
            it.i += parsed.len;
            return true;
        } else {
            return false;
        }
    }

    const code_point = it.nextCodepoint() orelse
        return Error.InvalidUtf8;
    return canCodepointContinueId(code_point);
}

fn identifier(self: *Self) Error!Token {
    const start = self.index;
    const str = self.source[start..];

    const view = std.unicode.Utf8View.initUnchecked(str);
    var it = view.iterator();

    const is_valid_start = try matchIdentifierStart(self, &it);
    if (!is_valid_start) return Error.MalformedIdentifier;

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
fn privateIdentifier(self: *Self) Error!Token {
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

    const len: u32 = blk: {
        if (str[0] == '.') {
            var decimal_len = matchDecimalPart(str) orelse
                return Error.InvalidNumericLiteral;
            decimal_len += matchExponentPart(str[decimal_len..]) orelse 0;

            // Just "." is not a valid numeric literal.
            if (decimal_len == 1) return Error.InvalidNumericLiteral;
            break :blk decimal_len;
        }

        if (str.len > 2 and str[0] == '0') {
            switch (str[1]) {
                'x', 'X' => {
                    const hex_len: u32 = matchHexDigits(str[2..]) orelse
                        return Error.InvalidHexLiteral;
                    break :blk hex_len + 2;
                },

                'o', 'O' => {
                    const octal_len: u32 = matchOctalDigits(str[2..]) orelse
                        return Error.InvalidNumericLiteral;
                    break :blk octal_len + 2;
                },

                'b', 'B' => {
                    const binary_len: u32 = matchBinaryDigits(str[2..]) orelse
                        return Error.InvalidBinaryLiteral;
                    break :blk binary_len + 2;
                },

                else => {},
            }
        }

        var decimal_len = matchDecimalIntegerLiteral(str) orelse
            return Error.InvalidNumericLiteral;
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
    // TODO(@injuly): what if there is a multi-byte UTF-8 codepoint here?
    if (self.peekByte()) |ch| {
        if (canCodepointStartId(ch) or std.ascii.isDigit(ch)) {
            return Error.InvalidNumericLiteral;
        }
    }

    return Token{
        .tag = .numeric_literal,
        .start = start,
        .len = len,
        .line = self.line,
    };
}

pub fn eof(self: *const Self) bool {
    return self.index >= self.source.len;
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
        => true,
        else => false,
    };
}

/// Consume all whitespace characters, including newlines,
/// and return a token representing that span.
/// Whitespace tokens are preserved in the Jam toolkit to
/// re-construct the original source code.
fn whiteSpaces(self: *Self) Token {
    const start = self.index;
    const line = self.line;

    while (!self.eof()) {
        const ch = self.source[self.index];

        if (!std.ascii.isAscii(ch)) {
            // check for Non-ASCII UTF-8 newline or whitespace char.
            const cp = util.utf8.codePointAt(self.source, self.index);
            if (cp.value == '\u{2028}' or cp.value == '\u{2029}') {
                self.index += cp.len;
                self.line += 1;
                continue;
            }

            // <ZWNSBP> (Zero Width No Break Space... yeah)
            if (cp.value == '\u{FEFF}') {
                self.index += cp.len;
                continue;
            }
            break;
        }

        // Whitespaces that are single byte UTF-8 code-points
        if (isSimpleWhitespace(ch)) {
            self.index += 1;
        } else if (isNewline(ch)) {
            self.line += 1;
            self.index += 1;

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
        .line = line,
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
        .{ "ಠ_ಠ", .identifier },
        .{ "\\u{105}bc", .identifier },
        .{ "\\u{105}\\u{5f}", .identifier },
        .{ "\\u{105}\\u005f", .identifier },
        .{ "0", .numeric_literal },
        .{ "120", .numeric_literal },
        .{ "1_000_000", .numeric_literal },
        .{ "1.5_1_1", .numeric_literal },
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
        var tokenizer = try Self.init("", .{});
        try std.testing.expectEqualDeep(Token{
            .tag = .eof,
            .start = 0,
            .len = 0,
            .line = 0,
        }, try tokenizer.next());
    }

    const bad_cases = [_]struct { []const u8, anyerror }{
        .{ "'hello", Error.NonTerminatedString },
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
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.@"+", (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
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
        try t.expectEqual(Token.Tag.numeric_literal, (try tokenizer.next()).tag);
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
        Token.Tag.numeric_literal,
        Token.Tag.@"]",
        Token.Tag.@")",
    });

    {
        var tokenizer = try Self.init(" /a\\(bc[some_character_class]/g //foo", .{});
        tokenizer.assume_bslash_starts_regex = true; // '/' is now interpreted as regex literal start marker.
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.regex_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.comment, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.eof, (try tokenizer.next()).tag);
    }

    {
        var tokenizer = try Self.init(" /a\\(bc[some_character_class]/g //foo", .{});
        tokenizer.assume_bslash_starts_regex = true; // '/' is now interpreted as regex literal start marker.
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.regex_literal, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.comment, (try tokenizer.next()).tag);
        try t.expectEqual(Token.Tag.eof, (try tokenizer.next()).tag);
    }

    {
        var tokenizer = try Self.init("`hello ${'world'}`", .{});
        try t.expectEqual(.template_literal_part, (try tokenizer.next()).tag);
        try t.expectEqual(.string_literal, (try tokenizer.next()).tag);
        tokenizer.assume_rbrace_is_template_part = true;
        try t.expectEqual(.template_literal_part, (try tokenizer.next()).tag);
        tokenizer.assume_rbrace_is_template_part = false;
        try t.expectEqual(.eof, (try tokenizer.next()).tag);
    }
}
