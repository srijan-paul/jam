const std = @import("std");

const types = @import("types.zig");
const Coordinate = types.Coordinate;

/// Convert a byte index into a Coordinate.
pub fn byteIndexToCoordinate(text: []const u8, byte_index: u32) Coordinate {
    const slice = text[0..byte_index];
    var line_number: usize = 0;
    // index of the '\n' character that precedes the char.
    var index_of_preceding_newline: usize = 0;

    for (0.., slice) |i, byte| {
        if (byte == '\n') {
            line_number += 1;
            index_of_preceding_newline = i;
        }
    }

    var column = byte_index - index_of_preceding_newline;
    if (column > 0) column -= 1;
    return Coordinate{
        .line = @intCast(line_number),
        .column = @intCast(column),
    };
}

const TestError = error{
    InvalidIndex,
};

pub fn testByteIndexToCoordinate(src: []const u8, line: u32, col: u32) !void {
    const index: u32 = @intCast(std.mem.indexOfScalar(u8, src, '$') orelse
        return TestError.InvalidIndex);
    const coord = byteIndexToCoordinate(src, index);
    try std.testing.expectEqual(line, coord.line);
    try std.testing.expectEqual(col, coord.column);
}

test byteIndexToCoordinate {
    try testByteIndexToCoordinate(
        \\ foo bar
        \\ baz
        \\
        \\ $
    ,
        3,
        1,
    );

    try testByteIndexToCoordinate(
        \\ foo bar
        \\ baz
        \\
        \\ $baz
    ,
        3,
        1,
    );

    try testByteIndexToCoordinate("$foo\n\n", 0, 0);
    try testByteIndexToCoordinate("foo\nfoo$bar\n", 1, 3);
    try testByteIndexToCoordinate("foo\nfoo$\n", 1, 3);
    try testByteIndexToCoordinate("foo\nbar\n$\n", 2, 0);
}
