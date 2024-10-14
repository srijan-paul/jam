const offsets = @import("offsets.zig");

const Range = offsets.Range;
const Coordinate = offsets.Coordinate;

pub const Token = struct {
    /// Index into the token array.
    /// Open ended enum for type safety.
    pub const Index = enum(u32) { _ };

    pub const Tag = enum(u32) {
        identifier,
        numeric_literal,
        string_literal,

        @"{",
        @"}",
        @"(",
        @")",
        @"[",
        @"]",
        @".",
        @"...",
        @";",
        @",",
        @"<",
        @">",
        @"<=",
        @">=",
        @"==",
        @"!=",
        @"===",
        @"!==",
        @"+",
        @"-",
        @"\"",
        @"'",
        @"*",
        @"/",
        @"%",
        @"**",
        @"//",
        @"++",
        @"--",
        @"<<",
        @">>",
        @">>>",
        @"&",
        @"|",
        @"^",
        @"!",
        @"~",
        @"&&",
        @"||",
        @"??",
        @"?",
        @":",
        @"+=",
        @"-=",
        @"*=",
        @"//=",
        @"%=",
        @"=",
        @"<<=",
        @">>=",
        @">>>=",
        @"&=",
        @"|=",
        @"^=",
        @"&&=",
        @"||=",
        @"/=",
        @"??=",
        @"**=",
        @"=>",

        // regular keywords
        kw_true,
        kw_false,
        kw_null,
        kw_var,
        kw_let,
        kw_const,
        kw_break,
        kw_case,
        kw_catch,
        kw_class,
        kw_continue,
        kw_debugger,
        kw_default,
        kw_do,
        kw_else,
        kw_export,
        kw_extends,
        kw_finally,
        kw_for,
        kw_function,
        kw_if,
        kw_import,
        kw_new,
        kw_return,
        kw_super,
        kw_switch,
        kw_this,
        kw_throw,
        kw_try,
        kw_while,
        kw_with,
        kw_typeof,
        kw_void,
        kw_delete,
        kw_in,
        kw_instanceof,

        // strict mode keywords
        kw_implements,
        kw_interface,
        kw_package,
        kw_private,
        kw_protected,
        kw_public,
        kw_static,
        kw_yield,

        // contextual keywords.
        kw_as,
        kw_async,
        kw_await,
        kw_constructor,
        kw_get,
        kw_set,
        kw_from,
        kw_of,
        kw_enum,

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
        return offsets.byteIndexToCoordinate(source, self.start);
    }

    /// (line, column) position for the end of this token.
    pub fn endCoord(self: Token, source: []const u8) Range {
        return offsets.byteIndexToCoordinate(source, self.start + self.len);
    }

    /// (line, column) range for the end of this token.
    pub fn toRange(self: Token, source: []const u8) Range {
        return Range{
            .start = self.startCoord(source),
            .end = self.endCoord(source),
        };
    }
};
