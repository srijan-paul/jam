const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

const TestResult = enum {
    pass,
    parse_error,
    ast_no_match,
};

const TestOutput = struct {
    fail_percent: std.json.Value,
    pass_percent: std.json.Value,
    unmatching_ast_count: usize,

    test_cases: std.json.Value,
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

    _ = try parser.parse();

    const source_explicit = try pass_explicit_dir.readFileAlloc(
        allocator,
        file_name,
        std.math.maxInt(u32),
    );
    defer allocator.free(source_explicit);

    var parser2 = try Parser.init(allocator, source_explicit, file_name);
    defer parser2.deinit();

    _ = try parser2.parse();

    if (parser.nodes.items.len != parser2.nodes.items.len) {
        return TestResult.ast_no_match;
    }

    for (parser.nodes.items, parser2.nodes.items) |n1, n2| {
        if (std.meta.activeTag(n1.data) != std.meta.activeTag(n2.data)) {
            return TestResult.ast_no_match;
        }
    }

    return TestResult.pass;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();

    defer arena.deinit();

    const tests_dir = try std.process.getEnvVarOwned(al, "JAM_TESTS_262_DIR");

    var d = try std.fs.openDirAbsolute(tests_dir, .{});
    defer d.close();

    var pass_dir = try d.openDir("pass", .{});
    defer pass_dir.close();

    var pass_explicit_dir = try d.openDir("pass-explicit", .{});
    defer pass_explicit_dir.close();

    var pass_dir_iter = pass_explicit_dir.iterate();

    var n_total: f64 = 0.0;
    var n_pass: f64 = 0.0;
    var n_fail: f64 = 0.0;
    var n_ast_no_match: usize = 0;

    var test_cases = std.json.ObjectMap.init(al);
    while (try pass_dir_iter.next()) |entry| {
        if (entry.kind != .file) continue;

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
            .ast_no_match => n_ast_no_match += 1,
        }

        const name = try al.dupe(u8, entry.name);
        try test_cases.put(name, .{ .string = @tagName(result) });
    }

    const fail_rate = ((n_fail + @as(f64, @floatFromInt(n_ast_no_match))) / n_total) * 100.0;
    const pass_rate = (n_pass / n_total) * 100.0;
    std.debug.assert(n_fail + @as(f64, @floatFromInt(n_ast_no_match)) + n_pass == n_total);

    const fail_rate_str = try std.fmt.allocPrint(al, "{d}", .{fail_rate});
    const pass_rate_str = try std.fmt.allocPrint(al, "{d}", .{pass_rate});

    const test_output = TestOutput{
        .test_cases = .{ .object = test_cases },
        .fail_percent = std.json.Value{ .number_string = fail_rate_str },
        .pass_percent = std.json.Value{ .number_string = pass_rate_str },
        .unmatching_ast_count = n_ast_no_match,
    };

    const s = try std.json.stringifyAlloc(
        al,
        test_output,
        .{ .whitespace = .indent_2 },
    );

    _ = try std.io.getStdOut().write(s);
}
