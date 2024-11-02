const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var args = std.process.args();
    const file_name = blk: {
        _ = args.next(); // skip the program name
        if (args.next()) |arg| {
            break :blk arg;
        }
        std.log.err("expected a file name\n", .{});
        return;
    };

    const source = std.fs.cwd().readFileAlloc(
        allocator,
        file_name,
        std.math.maxInt(u32),
    ) catch {
        std.log.err("failed to read file: {s}\n", .{file_name});
        return;
    };

    defer allocator.free(source);

    var parser = try Parser.init(allocator, source, file_name);
    defer parser.deinit();

    const node_idx = parser.parse() catch |err| {
        for (parser.diagnostics.items) |d| {
            std.log.err("{d}:{d} {s}", .{ d.coord.line + 1, d.coord.column, d.message });
        }

        const n_errors = parser.diagnostics.items.len;
        std.log.err("found {d} error{c}", .{ n_errors, @as(u8, if (n_errors == 1) ' ' else 's') });
        return err;
    };

    // for (0.., parser.nodes.items) |i, value| {
    //     std.debug.print("{d}: {s}\n", .{ i, @tagName(value.data) });
    // }
    //
    // for (parser.node_lists.items) |idx| {
    //     std.debug.print("{d}\n", .{ idx });
    // }

    const s = try syntax.pretty.toJsonString(allocator, &parser, node_idx);
    defer allocator.free(s);

    const io = std.io.getStdOut();
    try io.writeAll(s);
}
