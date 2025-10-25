const std = @import("std");
const js = @import("js");
const css = @import("css");

/// Parse a javascript file and return a stringified JSON representation of the AST.
/// The returned slice is owned by the caller.
fn jsFileToJsonAst(
    allocator: std.mem.Allocator,
    file_name: []const u8,
    mode: js.Parser.SourceType,
) ![]const u8 {
    const source = std.fs.cwd().readFileAlloc(
        file_name,
        allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    ) catch |err| {
        std.log.err("failed to read file: {s}\n", .{file_name});
        return err;
    };

    defer allocator.free(source);

    const Parser = js.Parser;
    var parser = try Parser.init(allocator, source, .{ .source_type = mode });
    defer parser.deinit();

    var result = parser.parse() catch |err| {
        for (parser.diagnostics.items) |d| {
            std.log.err("{d}:{d} {s}", .{ d.coord.line + 1, d.coord.column, d.message });
        }

        const n_errors = parser.diagnostics.items.len;
        std.log.err("found {d} error{c}", .{ n_errors, @as(u8, if (n_errors == 1) ' ' else 's') });
        return err;
    };
    defer result.deinit();

    return try js.estree.toJsonString(allocator, result.tree, .{ .start_end_locs = true });
}

/// Parse a javascript file and return a stringified JSON representation of the AST.
/// The returned slice is owned by the caller.
fn cssFileToJsonAst(allocator: std.mem.Allocator, file_name: []const u8) ![]const u8 {
    const source = std.fs.cwd().readFileAlloc(
        file_name,
        allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    ) catch |err| {
        std.log.err("failed to read file: {s}\n", .{file_name});
        return err;
    };

    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var parser = try css.Parser.init(&arena, source);
    defer parser.deinit();

    const node_idx = parser.parse() catch |err| {
        for (parser.diagnostics.items()) |d| {
            std.log.err("{d}:{d} {s}", .{ d.coord.line + 1, d.coord.column, d.message });
        }

        const n_errors = parser.diagnostics.items().len;
        std.log.err("found {d} error{c}", .{ n_errors, @as(u8, if (n_errors == 1) ' ' else 's') });
        return err;
    };

    return try css.ast.toJsonString(allocator, &parser, node_idx);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var args = std.process.args();
    var js_parse_mode: js.Parser.SourceType = .script;
    const file_name = blk: {
        _ = args.next(); // skip the program name
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "--esm")) {
                js_parse_mode = .module;
            } else {
                break :blk arg;
            }
        }
        std.log.err("Usage: jam [--esm] path-to-file.js\n", .{});
        return;
    };

    const pretty_ast_str = blk: {
        const file_ext = std.fs.path.extension(file_name);
        if (std.mem.eql(u8, file_ext, ".js") or
            std.mem.eql(u8, file_ext, ".mjs") or
            std.mem.eql(u8, file_ext, ".cjs") or
            std.mem.eql(u8, file_ext, ".jsx"))
        {
            break :blk try jsFileToJsonAst(allocator, file_name, js_parse_mode);
        } else if (std.mem.eql(u8, file_ext, ".css")) {
            break :blk try cssFileToJsonAst(allocator, file_name);
        }
        std.log.err("Unknown file extension {s}\n", .{file_ext});
        return error.BadFileExtension;
    };

    defer allocator.free(pretty_ast_str);

    _ = try std.posix.write(std.posix.STDOUT_FILENO, pretty_ast_str);
}
