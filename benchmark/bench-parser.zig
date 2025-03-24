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

    var cwd = fs.cwd();
    defer cwd.close();

    // If it doesn't already exist, clone "typescript.js"
    try downloadTypeScriptSource(
        al,
        cwd,
        try fs.path.join(al, &.{ "benchmark", "typescript.js" }),
    );

    try benchmarkParser(
        al,
        cwd,
        "typescript.js",
        "parser-bench.json",
    );
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
    results_file_name: []const u8,
) !void {
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
    std.log.info("time taken to parse '{s}': {d}ms\n", .{ js_file_path, avg_time_ms });

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
    var result_file = try dir.createFile(results_file_name, .{ .truncate = true });
    defer result_file.close();

    try result_file.writeAll(result_json);
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
