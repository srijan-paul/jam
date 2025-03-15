pub const offsets = @import("./offsets.zig");
pub const types = @import("./types.zig");
pub const StringPool = @import("./string_intern.zig");
pub const utf8 = @import("./utf8.zig");
pub const DiagnosticsBuilder = @import("diagnostics.zig");

const std = @import("std");

/// Copy `value` to a the heap and return a pointer.
/// This function is mostly used for debugging and pretty-printing,
/// and should be avoided in runtime code.
// TODO: get rid of this
pub fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

/// Find the first index of either [chr1] or [chr2] in the string [str], starting from [start_index].
/// If both characters are present, return the index of the first match.
///
/// TODO: benchmark this
pub fn indexOfCh2(str: []const u8, start_index: usize, chr1: u8, chr2: u8) ?usize {
    var i = start_index;
    if (std.simd.suggestVectorLength(u8)) |block_size| {
        const TBlock = @Vector(block_size, u8);
        const mask1: TBlock = @splat(chr1);
        const mask2: TBlock = @splat(chr2);

        while (i + block_size < str.len) : (i += block_size) {
            const block: TBlock = str[i..][0..block_size].*;
            const mask1_result = block == mask1;
            const mask2_result = block == mask2;

            const found_ch1 = @reduce(.Or, mask1_result);
            const found_ch2 = @reduce(.Or, mask2_result);

            if (found_ch1) {
                const ch1_index = i + std.simd.firstTrue(mask1_result).?;
                // If both chars are found, return the index of of the first match.
                if (found_ch2) {
                    const ch2_index = i + std.simd.firstTrue(mask2_result).?;
                    return @min(ch1_index, ch2_index);
                }

                return ch1_index;
            }

            if (found_ch2) {
                const ch2_index = i + std.simd.firstTrue(mask2_result).?;
                return ch2_index;
            }
        }
    }

    for (str[i..], i..) |c, j| {
        if (c == chr1 or c == chr2) return j;
    }

    return null;
}

test {
    std.testing.refAllDeclsRecursive(@This());

    {
        const strs = [_][]const u8{
            "this is a # test st$ring",
            "This is a test$",
            "This is a test#",
            "This is a t$est#",
            "$This is a t$est#",
            "$This is a test string ## -- #-# -$",
            "$$",
            "#",
            "012410429134281421742819471284214921741284612491274-12847-1241284124-12#$",
            \\ $This is a random wall of text that is u$sed to test the indexOfCh2 function.
            \\ This is a random wall of text #that is used to test the index$OfCh2 function.
            \\ This is a $random wall of text that is used t#o test the indexOfCh2 function.
            \\ This is a random wall of text that is use#d to test the indexOfCh2 function.
            \\ This is a random wall of text that is used to test the indexOfCh2 function.
            \\ This is a random w#all of text that is used to test the indexOfCh2 function.
            \\ Th#is is a $random wall of te$xt$ that is used to test the in$dexOfCh2 function.
            \\ This is a ra#ndom wall of text that is used to $test the indexOfCh2 function.
            \\ This is a random wall of text th#at is used to test# the indexOfCh2 fu#nctio#n.
            \\ This is a random #wall of text that is us#ed to test the indexOfCh2 fu#nctio#n.
            \\ This is a random wall of text$ that is #used to test the indexOfCh2 fu#nctio#n.
            \\ This is a random wall of text that is used to test the indexOfCh2 fu#nctio#n.
            \\ Thi$s is a random wall of text that is used to #test the indexOfCh2 fu#nctio#n.
            \\ This is a random #wall of text that$ is used to test the indexOfCh2 fu#nctio#n.
            \\ This is a rand#om wall of text $that is use#d to test the indexOfCh2 fu#nctio#n.
            \\ This is a random wall of text that is u$sed to test the indexOfCh2 function.
            ,
        };

        for (strs) |str| {
            for (0..str.len) |i| {
                const index = std.mem.indexOfAnyPos(u8, str, i, &.{ '$', '#' });
                const index2 = indexOfCh2(str, i, '$', '#');
                if (index != index2) {
                    std.debug.print("expected: {any}, got: {any}\nstring: {s}\n", .{ index, index2, str[i..] });
                    return error.Failed;
                }
            }
        }
    }
}
