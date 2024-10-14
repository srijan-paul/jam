const std = @import("std");
const Tokenizer = @import("./tokenize.zig").Tokenizer;

pub fn main() !void {
    var t = try Tokenizer.init("'x' ");
    std.debug.print("{any}", .{try t.next()});
}
