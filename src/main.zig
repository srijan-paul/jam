const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

const source =
    \\ { a, b = c } = 123
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer std.debug.assert(gpa.deinit() == .ok);

    var parser = try Parser.init(allocator, source, "test.js");
    defer parser.deinit();

    const node_idx = parser.parse() catch {
        for (parser.diagnostics.items) |d| {
            std.log.err("{s}", .{d.message});
        }

        std.log.err("\nfound {d} errors.\n", .{parser.diagnostics.items.len});
        return;
    };

    const s = try syntax.pretty.toJsonString(allocator, &parser, node_idx);
    defer allocator.free(s);

    const io = std.io.getStdOut();
    try io.writeAll(s);
}
