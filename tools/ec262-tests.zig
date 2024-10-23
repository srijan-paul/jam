const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

const TestResult = enum {
    pass,
    parse_error,
    ast_no_match,
};

const TestOutput = struct {
    fail_percent: f32,
    pass_percent: f32,
    unmatching_ast_count: f32,

    test_cases: std.json.ObjectMap,
};

fn testOnPassingFile(
    allocator: std.mem.Allocator,
    pass_dir: *std.fs.Dir,
    pass_explicit_dir: *std.fs.Dir,
    file_name: []const u8,
) !TestResult {
    const source = try pass_dir.readFileAlloc(allocator, file_name, std.math.maxInt(u32));
    defer allocator.free(source);

    // parse the program
    var parser = try Parser.init(allocator, source, file_name);
    defer parser.deinit();

    const program1 = try parser.parse();

    const source_explicit = try pass_explicit_dir.readFileAlloc(
        allocator,
        file_name,
        std.math.maxInt(u32),
    );
    defer allocator.free(source_explicit);

    var parser2 = try Parser.init(allocator, source_explicit, file_name);
    defer parser2.deinit();

    const program2 = try parser2.parse();

    const json1 = try syntax.pretty.toJsonString(allocator, &parser, program1);
    defer allocator.free(json1);

    const json2 = try syntax.pretty.toJsonString(allocator, &parser2, program2);
    defer allocator.free(json2);

    return if (std.mem.eql(u8, json1, json2)) .pass else .ast_no_match;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    const tests_dir = try std.process.getEnvVarOwned(allocator, "JAM_TESTS_262_DIR");
    defer allocator.free(tests_dir);

    var d = try std.fs.openDirAbsolute(tests_dir, .{});
    defer d.close();

    var pass_dir = try d.openDir("pass", .{});
    defer pass_dir.close();

    var pass_explicit_dir = try d.openDir("pass-explicit", .{});
    defer pass_explicit_dir.close();

    var pass_dir_iter = pass_explicit_dir.iterate();

    var n_total: f32 = 0.0;
    var n_pass: f32 = 0.0;
    var n_fail: f32 = 0.0;
    var n_ast_no_match: f32 = 0.0;

    var test_cases = std.json.ObjectMap.init(allocator);

    while (try pass_dir_iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (n_total >= 10) break;

        n_total += 1.0;
        const result = testOnPassingFile(
            allocator,
            &pass_dir,
            &pass_explicit_dir,
            entry.name,
        ) catch .parse_error;

        switch (result) {
            .pass => n_pass += 1.0,
            .parse_error => n_fail += 1.0,
            .ast_no_match => n_ast_no_match += 1.0,
        }

        try test_cases.put(entry.name, .{ .string = @tagName(result) });
    }

    const fail_rate = ((n_fail + n_ast_no_match) / n_total) * 100.0;
    const pass_rate = (n_pass / n_total) * 100.0;

    _ = fail_rate;
    _ = pass_rate;

    const s = try std.json.stringifyAlloc(
        allocator,
        std.json.Value{ .object = test_cases },
        .{ .whitespace = .indent_2 },
    );
    defer allocator.free(s);

    _ = try std.io.getStdOut().write(s);
}
