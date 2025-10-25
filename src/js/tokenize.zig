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

/// Returns a struct containing helpers for SIMD tokenizing, *if* the current target
/// supports SIMD instructions.
pub fn SimdContext() ?type {
    const simd = std.simd;
    if (simd.suggestVectorLength(u8)) |VectorLen| {
        return struct {
            /// Size of a []u8 block that should be loaded into a SIMD register at once.
            pub const block_size = @min(VectorLen, 16);
            /// Vector(u8, block_size) type.
            pub const TBlock = @Vector(block_size, u8);
            const all_ascii0s: TBlock = @splat('0');
            const all_ascii9s: TBlock = @splat('9');
            const all_as: TBlock = @splat('a');
            const all_zs: TBlock = @splat('z');
            const all_underscores: TBlock = @splat('_');
            const all_dollar_signs: TBlock = @splat('$');
            const all_32s: TBlock = @splat(32);
            const all_ascii_mask: TBlock = @splat(128);
            const all_stars: TBlock = @splat('*');
            const all_fslashes: TBlock = @splat('/');
            const all_lfs: TBlock = @splat('\n');
            const all_crs: TBlock = @splat('\r');
            const all_0s: TBlock = @splat(0);

            /// Return a block where a bit is set if the corresponding byte is a valid identifier character.
            pub fn isIdBlock(block: TBlock) TBlock {
                // Check if in ranges: '0'-'9', 'A'-'Z', 'a'-'z', or equals '_' or '$'
                const is_alphanumeric =
                    // Is between '0' and '9'
                    (@as(TBlock, @intFromBool(block >= all_ascii0s)) &
                        @as(TBlock, @intFromBool(block <= all_ascii9s))) |
                    // Is between 'A' and 'Z' or between 'a' and 'z'
                    (@as(TBlock, @intFromBool((block | all_32s) >= all_as)) &
                        @as(TBlock, @intFromBool((block | all_32s) <= all_zs)));

                return is_alphanumeric |
                    @as(TBlock, @intFromBool(block == all_underscores)) |
                    @as(TBlock, @intFromBool(block == all_dollar_signs));
            }

            ///  Return `true` if any u8 in [block] is a non ASCII character
            pub fn blockHasNonAsciiChars(block: TBlock) bool {
                const masked: TBlock = block & all_ascii_mask;
                return @reduce(.Or, masked) > 0;
            }
        };
    }
    return null;
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
    // TODO: do not pre-validate UTF-8 like this.
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

/// When scanning the opening tag of a JSX fragment or element, a '>' token might
/// be mistaken for a '>>=' or '>>' in cases like: `<div>>=</div>`.
///
/// In such cases, the parser should call this function to
/// re-scan the ">>" or ">>=" token as a single ">" token.
pub fn reScanJsxGt(self: *Self, token: *const Token) Token {
    assert(token.tag == .@">>" or
        token.tag == .@">>>" or
        token.tag == .@">=" or
        token.tag == .@">>=" or
        token.tag == .@">>>=");

    self.rewind(token.start, token.line);
    self.index += 1; // eat the '>'

    return Token{
        .tag = .@">",
        .start = token.start,
        .len = 1,
        .line = token.line,
    };
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
    const start_line = self.line;

    const str = self.source[start..];
    if (str.len < 2) return null;
    if (std.mem.startsWith(u8, str, "//")) {
        // https://262.ecma-international.org/15.0/index.html#prod-SingleLineComment
        try self.matchSingleLineCommentChars();
    } else if (std.mem.startsWith(u8, str, "/*")) {
        // https://262.ecma-international.org/15.0/index.html#prod-MultiLineComment
        try self.matchMultiLineCommentChars();
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
            try self.matchSingleLineCommentChars();
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
        try self.matchSingleLineCommentChars();
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
        try self.matchSingleLineCommentChars();
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
fn matchSingleLineCommentChars(self: *Self) Error!void {
    if (self.matchSingleLineCommentAsciiCharsSimd()) {
        return;
    }

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

/// Use vectorized instructions to consume all ASCII characters in a single line comment.
/// Returns 'true' if the whole single line comment was consumed, 'false' if it early exited
/// because it encountered a non-ASCII character, or if it had remaining characters that wouldn't
/// fully occupy a SIMD register.
fn matchSingleLineCommentAsciiCharsSimd(self: *Self) bool {
    var i = self.index;

    if (SimdContext()) |Simd| {
        // When possible, vectorize this part to check multiple ASCII characters at once.
        while (i + Simd.block_size < self.source.len) : (i += Simd.block_size) {
            const block: Simd.TBlock = self.source[i..][0..Simd.block_size].*;
            if (Simd.blockHasNonAsciiChars(block)) {
                self.index = i;
                // caller should continue lexing the rest of this comment
                return false;
            }

            const masked_crs = block == Simd.all_crs;
            const masked_lfs = block == Simd.all_lfs;
            const maybe_lf_index = std.simd.firstTrue(masked_lfs);

            if (std.simd.firstTrue(masked_crs)) |first_cr_index| {
                if (maybe_lf_index) |first_lf_index| {
                    if (first_cr_index + 1 == first_lf_index or
                        first_lf_index < first_cr_index)
                    {
                        // comment ends with '\r\n'
                        self.bumpLine();
                        self.index = i + first_lf_index + 1;
                        return true;
                    }
                }

                // comment ends with '\r'
                self.index = i + first_cr_index + 1;
                self.bumpLine();
                return true;
            }

            if (maybe_lf_index) |first_lf_index| {
                // comment ends with '\n'
                self.index = i + first_lf_index + 1;
                self.bumpLine();
                return true;
            }
        }
    }

    self.index = i;
    return false;
}

/// Consume all source characters until EOF or a '*/' sequence is found.
/// returns `Error.UnterminatedComment` on EOF.
fn matchMultiLineCommentChars(self: *Self) Error!void {
    self.index += 2; // eat '/*'

    if (try self.matchMultiLineCommentAsciiSimd()) {
        return;
    }

    while (!self.eof()) {
        const byte = self.source[self.index];
        if (std.ascii.isAscii(byte)) {
            self.index += 1;
            if (byte == '*' and self.peekByte() == '/') {
                self.index += 1; // eat the '/'
                return;
            } else if (byte == '\r') {
                self.bumpLine();
                if (self.peekByte() == '\n') {
                    // \r\n
                    self.index += 1;
                }
            } else if (byte == '\n') {
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

/// Assuming we're past the '/*' in a multi line comment, this function
/// will eat all the ASCII characters in the comment (along with the ending */)
/// using vectorized instructions.
///
/// Note: This function currently bails out if it encounters a non-ASCII character.
/// The caller should continue lexing using scalar instructions when this happens.
///
/// returns `true` if it found the end of the comment, `false` otherwise.
fn matchMultiLineCommentAsciiSimd(self: *Self) Error!bool {
    var i = self.index;

    if (SimdContext()) |Simd| {
        // When possible, vectorize this part to check multiple ASCII characters at once.
        while (i + Simd.block_size < self.source.len) : (i += Simd.block_size) {
            const block: Simd.TBlock = self.source[i..][0..Simd.block_size].*;
            if (Simd.blockHasNonAsciiChars(block)) {
                // TODO: We should vectorize this too instead of straight up giving up...
                // fallback to scalar mode.
                break;
            }

            // Find the first index of '*' in this block that is followed by a '/'
            const end_comment_index: ?u32 = blk: {
                // 'F' '*' 'C' 'K' '!' '*' '/'
                // 00  FF  00  00  00  FF  00  <- is_star
                // 00  00  00  00  00  00  FF  <- is_fslash
                // 00  00  00  00  00  FF  00  <- is_end_comment  (is_star & shift(is_flash, 1))
                const is_star = ~(block ^ Simd.all_stars);
                const is_fslash = ~(block ^ Simd.all_fslashes);
                const is_end_comment = is_star & (std.simd.shiftElementsLeft(is_fslash, 1, 0));
                if (std.simd.firstIndexOfValue(is_end_comment, 0xff)) |index| {
                    break :blk index;
                }

                // If the last byte is '*', we need to check the next block.
                // This is because the '*/' sequence might be split between two blocks.
                if (block[Simd.block_size - 1] == '*') {
                    if (i + Simd.block_size < self.source.len and
                        self.source[i + Simd.block_size] == '/')
                    {
                        break :blk Simd.block_size - 1;
                    }
                }

                break :blk null;
            };

            // Found a '*/'
            if (end_comment_index) |star_index_in_block| {
                // count all newlines between i and end_index
                // TODO: can I vectorize this as well? Most likely yeah
                var j: u32 = i;
                var num_newlines: u32 = 0;
                const star_index = i + star_index_in_block;
                while (j != star_index) : (j += 1) {
                    const byte = self.source[j];
                    if (byte == '\r') {
                        num_newlines += 1;
                        // \r\n
                        if (j + 1 < self.source.len and self.source[j + 1] == '\n') {
                            j += 1;
                        }
                    } else if (byte == '\n') {
                        num_newlines += 1;
                    }
                }

                self.bumpLineBy(num_newlines);
                self.index = star_index + 2; // current pos is one past index of '/'
                return true;
            }

            const masked_crs = ~(block ^ Simd.all_crs);
            const masked_lfs = ~(block ^ Simd.all_lfs);

            // Check for a '\r\n', create a vector containing 0xff for every cr
            // that is followed by an lf
            // \r  \n  \n  'o' 'o' \r
            // 00  FF  FF  00  00  00 <- masked_lfs
            // FF  00  00  00  00  FF <- masked_crs
            // FF  00  00  00  00  00 <- crlfs = masked_crs & shift(masked_lfs, 1)
            const crlfs = masked_crs & std.simd.shiftElementsLeft(masked_lfs, 1, 0);

            // count all '\n's that DO NOT appear immediately after a \r
            const all_0xffs: Simd.TBlock = @splat(0xff);
            const lone_lfs = @select(
                u8,
                std.simd.shiftElementsRight(crlfs, 1, 0) != all_0xffs,
                masked_lfs,
                Simd.all_0s,
            );

            const lone_crs = @select(u8, crlfs == all_0xffs, Simd.all_0s, masked_crs);

            // count all '\r\n's, '\r's, and '\n's in this block.
            const num_crs = std.simd.countElementsWithValue(lone_crs, 0xff);
            const num_lfs = std.simd.countElementsWithValue(lone_lfs, 0xff);
            const num_crlfs = std.simd.countElementsWithValue(crlfs, 0xff);
            self.bumpLineBy(num_crs + num_lfs + num_crlfs);
        }
    }

    self.index = i;
    return false;
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

    var token_tag = try self.matchIdentifierTail(start_char_kind);
    const len = self.index - start;

    // TODO: ensure its not an escaped keyword.
    // To do this well, add a 'escaped_code_point' member in the IdCharKind
    // enum.
    const id_str = str[0..len];
    if (token_tag == .identifier and len >= 2 and len <= 11) {
        token_tag = kwOrIdentifierTag(id_str);
    }

    return Token{
        .start = start,
        .len = @intCast(len),
        .tag = token_tag,
        .line = self.line,
    };
}

/// Get the 'tag' associated with a keyword or identifier.
/// For identifiers lexemes, just return .identifier, for keywords,
/// return the tag for the specific keyword
fn kwOrIdentifierTag(lexeme: []const u8) Token.Tag {
    // This massive switch statement is actually faster than comparing the lexeme against all keywords.
    // There is a measurable difference in large files.
    // For instance, in the 'typescript.js' bundle, this gave us a 2ms speedup (85 -> 83ms).
    //
    // An alternative approach is to use a (very cheap) perfect hash function and the 'pext' instruction
    // (on architectures where it is supported) as described here:
    // http://0x80.pl/notesen/2023-04-30-lookup-in-strings.html

    // List of all JS keywords sorted by length, for my reference:
    //  2: "as", "do", "if", "in", "of",
    //  3: "for", "let", "new", "try", "var",
    //  4: "case", "else", "enum", "this", "true", "void", "with", "null", "from",
    //  5: "await", "break", "catch", "class", "const", "false", "super", "throw", "while", "yield", "async",
    //  6: "import", "return", "switch", "typeof", "delete", "export", "static",
    //  7: "default", "extends", "finally",
    //  8: "continue", "function", "debugger",
    //  10: "instanceof",
    //  11: "constructor",

    switch (lexeme.len) {
        2 => {
            switch (lexeme[1]) {
                'f' => {
                    switch (lexeme[0]) {
                        'i' => return .kw_if,
                        'o' => return .kw_of,
                        else => return .identifier,
                    }
                },
                'n' => if (lexeme[0] == 'i') return .kw_in,
                'o' => if (lexeme[0] == 'd') return .kw_do,
                's' => if (lexeme[0] == 'a') return .kw_as,
                else => return .identifier,
            }
        },
        3 => {
            //"for", "let", "new", "try", "var",
            switch (lexeme[0]) {
                'f' => if (lexeme[1] == 'o' and lexeme[2] == 'r') return .kw_for,
                'l' => if (lexeme[1] == 'e' and lexeme[2] == 't') return .kw_let,
                'n' => if (lexeme[1] == 'e' and lexeme[2] == 'w') return .kw_new,
                't' => if (lexeme[1] == 'r' and lexeme[2] == 'y') return .kw_try,
                'v' => if (lexeme[1] == 'a' and lexeme[2] == 'r') return .kw_var,
                else => return .identifier,
            }

            return .identifier;
        },

        4 => {
            // "case", "else", "enum", "this", "true", "void", "with", "null", "from"
            switch (lexeme[1]) {
                'a' => {
                    if (lexeme[0] == 'c' and lexeme[2] == 's' and lexeme[3] == 'e') return .kw_case;
                },

                'l' => {
                    if (lexeme[0] == 'e' and lexeme[2] == 's' and lexeme[3] == 'e') return .kw_else;
                },

                'n' => {
                    if (lexeme[0] == 'e' and lexeme[2] == 'u' and lexeme[3] == 'm') return .kw_enum;
                },

                'h' => {
                    if (lexeme[0] == 't' and lexeme[2] == 'i' and lexeme[3] == 's') return .kw_this;
                },

                'r' => {
                    if (std.mem.eql(u8, lexeme, "true")) return .kw_true;
                    if (std.mem.eql(u8, lexeme, "from")) return .kw_from;
                },

                'o' => {
                    if (lexeme[0] == 'v' and lexeme[2] == 'i' and lexeme[3] == 'd') return .kw_void;
                },

                'i' => {
                    if (lexeme[0] == 'w' and lexeme[2] == 't' and lexeme[3] == 'h') return .kw_with;
                },

                'u' => {
                    if (lexeme[0] == 'n' and lexeme[2] == 'l' and lexeme[3] == 'l') return .kw_null;
                },

                else => return .identifier,
            }
            return .identifier;
        },

        5 => {
            //  "async", await", "break", "catch", "class", "const", "false", "super", "throw", "while", "yield",
            switch (lexeme[2]) {
                'a' => {
                    if (std.mem.eql(u8, lexeme, "await")) return .kw_await;
                    if (std.mem.eql(u8, lexeme, "class")) return .kw_class;
                },

                'e' => {
                    if (std.mem.eql(u8, lexeme, "break")) return .kw_break;
                    if (std.mem.eql(u8, lexeme, "yield")) return .kw_yield;
                },

                'p' => if (std.mem.eql(u8, lexeme, "super")) return .kw_super,
                'r' => if (std.mem.eql(u8, lexeme, "throw")) return .kw_throw,
                't' => if (std.mem.eql(u8, lexeme, "catch")) return .kw_catch,
                'n' => if (std.mem.eql(u8, lexeme, "const")) return .kw_const,
                'l' => if (std.mem.eql(u8, lexeme, "false")) return .kw_false,
                'i' => if (std.mem.eql(u8, lexeme, "while")) return .kw_while,
                'y' => if (std.mem.eql(u8, lexeme, "async")) return .kw_async,
                else => return .identifier,
            }

            return .identifier;
        },

        6 => {
            //  "import", "return", "switch", "typeof", "delete", "export",  "static",
            switch (lexeme[0]) {
                'i' => if (std.mem.eql(u8, lexeme, "import")) return .kw_import,
                'r' => if (std.mem.eql(u8, lexeme, "return")) return .kw_return,
                's' => {
                    if (std.mem.eql(u8, lexeme, "switch")) return .kw_switch;
                    if (std.mem.eql(u8, lexeme, "static")) return .kw_static;
                },
                't' => if (std.mem.eql(u8, lexeme, "typeof")) return .kw_typeof,
                'd' => if (std.mem.eql(u8, lexeme, "delete")) return .kw_delete,
                'e' => if (std.mem.eql(u8, lexeme, "export")) return .kw_export,
                'f' => if (std.mem.eql(u8, lexeme, "finally")) return .kw_finally,
                else => return .identifier,
            }
        },

        7 => {
            switch (lexeme[0]) {
                'd' => if (std.mem.eql(u8, lexeme, "default")) return .kw_default,
                'e' => if (std.mem.eql(u8, lexeme, "extends")) return .kw_extends,
                'f' => if (std.mem.eql(u8, lexeme, "finally")) return .kw_finally,
                else => return .identifier,
            }
        },
        8 => {
            // "continue", "function", "debugger",
            switch (lexeme[0]) {
                'c' => if (std.mem.eql(u8, lexeme, "continue")) return .kw_continue,
                'f' => if (std.mem.eql(u8, lexeme, "function")) return .kw_function,
                'd' => if (std.mem.eql(u8, lexeme, "debugger")) return .kw_debugger,
                else => return .identifier,
            }
        },
        10 => if (std.mem.eql(u8, lexeme, "instanceof")) return .kw_instanceof,
        11 => if (std.mem.eql(u8, lexeme, "constructor")) return .kw_constructor,
        else => return .identifier,
    }

    return .identifier;
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
        self.matchAsciiIdentifierChars();
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

/// Consume all ASCII characters that can appear in an identifier.
inline fn matchAsciiIdentifierChars(self: *Self) void {
    var i = self.index;
    if (SimdContext()) |Simd| {
        // When possible, vectorize this part to check multiple ASCII characters at once.
        while (i + Simd.block_size < self.source.len) : (i += Simd.block_size) {
            const block: Simd.TBlock = self.source[i..][0..Simd.block_size].*;
            const id_mask_result = Simd.isIdBlock(block);
            if (std.simd.firstIndexOfValue(id_mask_result, 0)) |j| {
                // j is the index of first NON identifier character
                i += j;
                self.index = i;
                return;
            }
        }
    } else {
        return self.matchAsciiIdentifierCharsRegular();
    }

    // If we reach here, we have less than Simd.block_size characters left.
    while (i < self.source.len and isAsciiIdentifierContt(self.source[i])) : (i += 1) {}
    self.index = i;
}

/// Non-vectorized version of `matchAsciiIdentifierChars`
inline fn matchAsciiIdentifierCharsRegular(self: *Self) void {
    while (!self.eof() and
        isAsciiIdentifierContt(self.source[self.index])) : (self.index += 1)
    {}
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

pub fn bumpLineBy(self: *Self, n: u32) void {
    self.line += n;
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

            // <ZWNBSP>, and all UTF-8 code-points
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
        var tokenizer = try Self.init("/* this is a\r\nmultiline\r\r\ncomment */", .{});
        const comment_token = try tokenizer.next();
        try t.expectEqual(.comment, comment_token.tag);
        try t.expectEqualDeep(
            tokenizer.source,
            comment_token.toByteSlice(tokenizer.source),
        );
        try t.expectEqual(3, tokenizer.line);
    }

    {
        var tokenizer = try Self.init("/*\r\r\r\r\n\n\r\n*/", .{});
        const comment_token = try tokenizer.next();
        try t.expectEqual(.comment, comment_token.tag);
        try t.expectEqualDeep(
            tokenizer.source,
            comment_token.toByteSlice(tokenizer.source),
        );
        try t.expectEqual(6, tokenizer.line);
    }

    {
        var tokenizer = try Self.init("/*\r\r\r\r\n\n\r\n\r\n\r\r\r\r\n\n\r\r\r\r\r\r\r\n*/", .{});
        const comment_token = try tokenizer.next();
        try t.expectEqual(.comment, comment_token.tag);
        try t.expectEqualDeep(
            tokenizer.source,
            comment_token.toByteSlice(tokenizer.source),
        );
        try t.expectEqual(19, tokenizer.line);
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

    {
        var tokenizer = try Self.init("   \n   \n  ", .{});
        try t.expectEqual(.whitespace, (try tokenizer.next()).tag);
        try t.expectEqual(.eof, (try tokenizer.next()).tag);
    }
}
