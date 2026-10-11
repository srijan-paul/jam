const std = @import("std");
const js = @import("js");

const assert = std.debug.assert;
const json = std.json;

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const al = arena.allocator();

    // parse cli flags
    var args_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, al);
    defer args_iter.deinit();

    var out_file_path: ?[]const u8 = null;

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--out")) {
            out_file_path = args_iter.next() orelse {
                try std.Io.File.stderr().writeStreamingAll(io, "--out needs a file path as argument\n");
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--compare")) {
            const file_old: []const u8 = args_iter.next() orelse {
                try std.Io.File.stderr().writeStreamingAll(io, "Usage: --compare <old_file_path> <new_file_path>\n");
                std.process.exit(1);
            };

            const file_new: []const u8 = args_iter.next() orelse {
                try std.Io.File.stderr().writeStreamingAll(io, "Usage: --compare <old_file_path> <new_file_path>\n");
                std.process.exit(1);
            };

            try compareResults(
                io,
                al,
                file_old,
                file_new,
            );

            std.process.exit(0);
        }
    }

    const cwd = std.Io.Dir.cwd();

    // If it doesn't already exist, clone "typescript.js"
    try downloadTypeScriptSource(
        io,
        al,
        cwd,
        try std.Io.Dir.path.join(al, &.{ "benchmark", "typescript.js" }),
    );

    const avg_time_ms = try benchmarkParser(
        io,
        al,
        cwd,
        "typescript.js",
        out_file_path,
    );

    const output = try al.print("{d}", .{avg_time_ms});
    _ = try std.Io.File.stdout().writeStreamingAll(io, output);
}

/// Run the parser on the file present at [js_file_path],
/// and store the results in a file called [results_file_name] under the
/// 'benchmark' directory.
///
/// [js_source_path] is relative to the 'benchmark' directory.
fn benchmarkParser(
    io: std.Io,
    allocator: std.mem.Allocator,
    cwd: std.Io.Dir,
    js_file_path: []const u8,
    maybe_results_file_name: ?[]const u8,
) !f64 {
    var dir = try cwd.openDir(io, "benchmark", .{ .access_sub_paths = true });
    defer dir.close(io);

    const js_source = try dir.readFileAlloc(io, js_file_path, allocator, std.Io.Limit.limited(std.math.maxInt(u32)));
    defer allocator.free(js_source);

    // Run the parser 100 times and measure the average time per run
    const num_runs: usize = 100;
    const timer_start = std.Io.Clock.awake.now(io);
    for (0..num_runs) |_| {
        var parser = try js.Parser.init(std.heap.page_allocator, js_source, .{});
        defer parser.deinit();

        var parse_result = parser.parse() catch |e| {
            for (parser.diagnostics.items) |d| {
                std.log.err("{d}:{d} {s}", .{ d.coord.line + 1, d.coord.column, d.message });
            }
            return e;
        };

        defer parse_result.deinit();
    }

    const timer_end = std.Io.Clock.awake.now(io);
    const time_taken_ns: f64 = @floatFromInt(timer_end.toNanoseconds() - timer_start.toNanoseconds());
    const avg_time_ms = (time_taken_ns / std.time.ns_per_ms) / @as(f64, @floatFromInt(num_runs));

    // Create a JSON object and put the average time in there
    var result_obj = json.ObjectMap.empty;
    const avg_time_ms_str = try allocator.print("{d:.3}", .{avg_time_ms});
    defer allocator.free(avg_time_ms_str);

    try result_obj.put(
        allocator,
        "average_run_time_ms",
        json.Value{ .number_string = avg_time_ms_str },
    );

    const result_json = try json.Stringify.valueAlloc(
        allocator,
        json.Value{ .object = result_obj },
        .{ .whitespace = .indent_2 },
    );

    // Serialize the JSON and write to results file
    if (maybe_results_file_name) |results_file_name| {
        var result_file = if (results_file_name[0] == '/')
            try std.Io.Dir.createFileAbsolute(io, results_file_name, .{ .truncate = true })
        else
            try cwd.createFile(io, results_file_name, .{ .truncate = true });
        defer result_file.close(io);

        try result_file.writeStreamingAll(io, result_json);
    }
    return avg_time_ms;
}

/// Download the 'typescript.js' bundle from cludflare CDN, and plop it in the
/// given [dst_path] file.
fn downloadTypeScriptSource(
    io: std.Io,
    allocator: std.mem.Allocator,
    cwd: std.Io.Dir,
    dst_path: []const u8,
) !void {
    if (cwd.access(io, dst_path, .{})) {
        // file has already been downloaded.
        return;
    } else |_| {
        // if access failed, that's fine.
        // It means that (most likely) the file
        // doesn't already exist - so we download it again
    }

    var client = std.http.Client{ .allocator = allocator, .io = io };
    defer client.deinit();

    std.log.info("Downloading 'typescript.js'...", .{});

    const out_file = try cwd.createFile(io, dst_path, .{});
    defer out_file.close(io);

    var write_buf: [8192]u8 = undefined;
    var file_writer = out_file.writer(io, &write_buf);

    const result = try client.fetch(.{
        .location = .{ .url = "https://cdnjs.cloudflare.com/ajax/libs/typescript/5.8.2/typescript.js" },
        .method = .GET,
        .response_writer = &file_writer.interface,
    });

    if (result.status != .ok) {
        std.log.err("Failed to download typescript.js: HTTP {d}", .{@backingInt(result.status)});
        return error.DownloadFailed;
    }

    try file_writer.end();

    std.log.info("downloaded 'typescript.js' in {s}", .{dst_path});
}

const BenchmarkResult = struct {
    average_run_time_ms: f64,
};

/// Compare two JSON encoded benchmark results and print a message
/// to stdout indicating the difference in average run times.
///
/// Exit with status code 1 if the new parser is slower than the old one.
fn compareResults(
    io: std.Io,
    allocator: std.mem.Allocator,
    file_path_old: []const u8,
    file_path_new: []const u8,
) !void {
    const cwd = std.Io.Dir.cwd();

    const f1_contents = try cwd.readFileAlloc(io, file_path_old, allocator, std.Io.Limit.limited(std.math.maxInt(u32)));
    const f2_contents = try cwd.readFileAlloc(io, file_path_new, allocator, std.Io.Limit.limited(std.math.maxInt(u32)));

    defer allocator.free(f1_contents);
    defer allocator.free(f2_contents);

    const result_old = try json.parseFromSlice(
        BenchmarkResult,
        allocator,
        f1_contents,
        .{},
    );
    const result_new = try json.parseFromSlice(
        BenchmarkResult,
        allocator,
        f2_contents,
        .{},
    );

    const difference = result_new.value.average_run_time_ms - result_old.value.average_run_time_ms;
    const percent_difference = difference / result_old.value.average_run_time_ms * 100;

    const message = try allocator.print("New parser is {d:.2}% faster\n", .{percent_difference});
    defer allocator.free(message);

    _ = try std.Io.File.stdout().writeStreamingAll(io, message);

    if (result_new.value.average_run_time_ms < result_old.value.average_run_time_ms) {
        std.process.exit(1);
    }
}
