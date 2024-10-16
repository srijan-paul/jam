const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    defer std.debug.assert(gpa.deinit() == .ok);

    const source = "foo.bar(a = b++)";

    var parser = try Parser.init(allocator, source, "test.js");
    defer parser.deinit();

    const node_idx = parser.parse() catch {
        for (parser.diagnostics.items) |d| {
            std.debug.print("{s}", .{d.message});
        }
        return;
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();
    defer arena.deinit();

    const pretty_node = try parser.toPretty(al, node_idx);
    var io = std.io.getStdOut();
    try io.writeAll(try std.json.stringifyAlloc(al, pretty_node, .{}));
}
