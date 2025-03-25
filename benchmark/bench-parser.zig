const std = @import("std");
const js = @import("js");

const assert = std.debug.assert;
const json = std.json;
const meta = std.meta;
const fs = std.fs;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const result = gpa.deinit();
        assert(result == .ok);
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const al = arena.allocator();

    // parse cli flags
    var args_iter = try std.process.argsWithAllocator(al);
    defer args_iter.deinit();

    var stderr = std.io.getStdErr();

    var out_file_path: ?[]const u8 = null;

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--out")) {
            out_file_path = args_iter.next() orelse {
                try stderr.writeAll("--out needs a file path as argument\n");
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--compare")) {
            const file_old: []const u8 = args_iter.next() orelse {
                try stderr.writeAll("Usage: --compare <old_file_path> <new_file_path>\n");
                std.process.exit(1);
            };

            const file_new: []const u8 = args_iter.next() orelse {
                try stderr.writeAll("Usage: --compare <old_file_path> <new_file_path>\n");
                std.process.exit(1);
            };

            const out = std.io.getStdErr();
            try compareResults(
                al,
                file_old,
                file_new,
                out,
            );

            std.process.exit(0);
        }
    }

    var cwd = fs.cwd();
    defer cwd.close();

    // If it doesn't already exist, clone "typescript.js"
    try downloadTypeScriptSource(
        al,
        cwd,
        try fs.path.join(al, &.{ "benchmark", "typescript.js" }),
    );

    const avg_time_ms = try benchmarkParser(
        al,
        cwd,
        "typescript.js",
        out_file_path,
    );

    var io = std.io.getStdOut();
    _ = try io.writeAll(try std.fmt.allocPrint(al, "{d}", .{avg_time_ms}));
}

/// Run the parser on the file present at [js_file_path],
/// and store the results in a file called [results_file_name] under the
/// 'benchmark' directory.
///
/// [js_source_path] is relative to the 'benchmark' directory.
fn benchmarkParser(
    allocator: std.mem.Allocator,
    cwd: fs.Dir,
    js_file_path: []const u8,
    maybe_results_file_name: ?[]const u8,
) !f64 {
    var dir = try cwd.openDir("benchmark", .{ .access_sub_paths = true });
    defer dir.close();

    const js_source = try dir.readFileAlloc(allocator, js_file_path, std.math.maxInt(u32));
    defer allocator.free(js_source);

    // Run the parser 100 times and measure the average time per run
    var total_time_ms: f64 = 0;
    const num_runs: usize = 100;
    for (0..num_runs) |_| {
        const start_ns = std.time.nanoTimestamp();
        {
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

        const end_ns = std.time.nanoTimestamp();
        total_time_ms += @as(f64, @floatFromInt(@as(u64, @intCast(end_ns - start_ns)))) /
            @as(f64, @floatFromInt(std.time.ns_per_ms));
    }

    const avg_time_ms = total_time_ms / @as(f64, @floatFromInt(num_runs));

    // Create a JSON object and put the average time in there
    var result_obj = json.ObjectMap.init(allocator);
    const avg_time_ms_str = try std.fmt.allocPrint(allocator, "{d:.3}", .{avg_time_ms});
    defer allocator.free(avg_time_ms_str);

    try result_obj.put(
        "average_run_time_ms",
        json.Value{ .number_string = avg_time_ms_str },
    );

    const result_json = try json.stringifyAlloc(
        allocator,
        json.Value{ .object = result_obj },
        .{ .whitespace = .indent_2 },
    );

    // Serialize the JSON and write to results file
    if (maybe_results_file_name) |results_file_name| {
        var result_file = if (results_file_name[0] == '/')
            try fs.createFileAbsolute(results_file_name, .{ .truncate = true })
        else
            try cwd.createFile(results_file_name, .{ .truncate = true });
        defer result_file.close();

        try result_file.writeAll(result_json);
    }
    return avg_time_ms;
}

/// Download the 'typescript.js' bundle from cludflare CDN, and plop it in the
/// given [dst_path] file.
fn downloadTypeScriptSource(
    allocator: std.mem.Allocator,
    cwd: fs.Dir,
    dst_path: []const u8,
) !void {
    if (cwd.access(dst_path, .{})) {
        // file has already been downloaded.
        return;
    } else |_| {
        // if access failed, that's fine.
        // It means that (most likely) the file
        // doesn't already exist - so we download it again
    }

    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse("https://cdnjs.cloudflare.com/ajax/libs/typescript/5.8.2/typescript.js");

    var hd_buf: [2048]u8 = undefined;
    var req = try std.http.Client.open(&client, .GET, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .server_header_buffer = &hd_buf,
    });

    defer req.deinit();

    std.log.info("Downloading 'typescript.js'...", .{});
    try req.send();
    try req.finish();

    try req.wait();

    const out_file = try cwd.createFile(dst_path, .{});
    defer out_file.close();

    const fwriter = out_file.writer();
    const req_reader = req.reader();

    var buf = [_]u8{0} ** 1024;
    while (true) {
        const n_bytes = try req_reader.read(&buf);
        if (n_bytes == 0) break;
        std.debug.assert(try fwriter.write(buf[0..n_bytes]) == n_bytes);
    }

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
    allocator: std.mem.Allocator,
    file_path_old: []const u8,
    file_path_new: []const u8,
    out: fs.File,
) !void {
    var cwd = fs.cwd();
    defer cwd.close();

    const f1_contents = try cwd.readFileAlloc(allocator, file_path_old, std.math.maxInt(u32));
    const f2_contents = try cwd.readFileAlloc(allocator, file_path_new, std.math.maxInt(u32));

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
        f1_contents,
        .{},
    );

    const difference = result_new.value.average_run_time_ms - result_old.value.average_run_time_ms;
    const percent_difference = difference / result_old.value.average_run_time_ms * 100;

    const message = try std.fmt.allocPrint(allocator, "New parser is {d:.2}% faster", .{percent_difference});
    defer allocator.free(message);

    try out.writeAll(message);

    if (result_new.value.average_run_time_ms < result_old.value.average_run_time_ms) {
        std.process.exit(1);
    }
}
