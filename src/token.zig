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

        kw_true,
        kw_false,
        kw_null,

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
