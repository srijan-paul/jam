const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

const source =
    \\ a | a ?? b | c
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer std.debug.assert(gpa.deinit() == .ok);

    var parser = try Parser.init(allocator, source, "test.js");
    defer parser.deinit();

    const node_idx = parser.parse() catch {
        for (parser.diagnostics.items) |d| {
            std.debug.print("{s}", .{d.message});
        }
        return;
    };

    const s = try syntax.pretty.toJsonString(allocator, &parser, node_idx);
    defer allocator.free(s);

    const io = std.io.getStdOut();
    try io.writeAll(s);
}
