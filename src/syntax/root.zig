const std = @import("std");
const util = @import("util");
const offsets = util.offsets;
const types = util.types;

/// The token data-type is used to represent spans in the source text.
/// concatenating tokens in order should give yield the original source text.
/// This allows syntax trees to preserve whitespaces and comments.
/// Nodes need only store two properties: `start: Token.Index` and `end: Token.Index`,
/// and we can reliably reconstruct the original source text with whitespaces.
///
/// `TagType` must be an enum type.
pub fn Token(TagType: type) type {
    if (std.meta.activeTag(@typeInfo(TagType)) != .@"enum") {
        std.debug.panic("Tag type a Token must be an enum");
    }

    return struct {
        const Self = @This();

        /// This number is used to index into a list of enums.
        /// A parser that consumes tokens would usually maintain
        /// a list of tokens, and nodes can reference tokens by using
        /// a member of this type.
        /// e.g:
        /// ```zig
        /// pub const BinaryExpression = struct {
        ///     operator: Token.Index, // index of a token in a token stream.
        ///     left: Node.Index,
        ///     right: Node.Index,
        /// };
        /// ```
        pub const Index = enum(u32) { _ };
        pub const Tag = TagType;

        /// Identifies the token's kind.
        tag: Tag,
        /// Byte index into the source string.
        start: u32,
        /// Size of the token's text in bytes
        len: u32,
        /// 0-indexed line location of the token.
        /// For multi-line tokens like template literals, this only stores the start line.
        line: u32,

        /// Return the token's text as a byte slice.
        pub fn toByteSlice(self: Self, source: []const u8) []const u8 {
            return source[self.start .. self.start + self.len];
        }

        /// (line, column) position for the start of this token.
        pub fn startCoord(self: Self, source: []const u8) types.Coordinate {
            return offsets.byteIndexToCoordinate(source, self.start);
        }

        /// (line, column) position for the end of this token.
        pub fn endCoord(self: Self, source: []const u8) types.Coordinate {
            return offsets.byteIndexToCoordinate(source, self.start + self.len);
        }

        /// start and end (line, column) range for the end of this token.
        pub fn toRange(self: Self, source: []const u8) types.Range {
            // TODO: end-coord can be inferred from start-coord.
            // optimize this.
            return types.Range{
                .start = self.startCoord(source),
                .end = self.endCoord(source),
            };
        }
    };
}
