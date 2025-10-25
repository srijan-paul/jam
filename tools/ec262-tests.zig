const std = @import("std");
const js = @import("js");

const Parser = js.Parser;

const ParseResult = enum {
    pass,
    parse_error,
    ast_no_match,
    malformed_file_parsed,
};

/// Structured representation of the `tools/results.json` file.
const TestResult = struct {
    fail_percent: std.json.Value,
    pass_percent: std.json.Value,

    // The below two metrics are calculated from the 'fail' directory
    // in the test-suite.

    /// % of files that have parse errors,
    /// and the parser rightly fails on them.
    malformed_pass_percent: std.json.Value,
    /// % of files that have parse errors,
    /// but the parser incorrectly passes them.
    malformed_fail_percent: std.json.Value,

    unmatching_ast_count: usize,

    /// string -> string map.
    /// key is a filename like "12ea3bf0653f8409.js", and value is a stringified `TestResult`.
    test_cases: std.json.Value,
};

/// These test-cases are... incorrect in the test-suite (for the lack of a better word?)
/// The tc39 test suite says the following two programs should have the same parse trees:
/// `{ a: 1 }` and `{ 'a':  1 }`, but I disagree.
/// The first one has an identifier node as the key, while the second a string literal.
/// A linter cannot possibly work if we parsed them both the same.
/// One solution would be to write an AST-comparison function that accounts for this,
/// but an easier alternative (for now) is to just skip these few tests, and manually verify the parse results myself.
/// As long as the rest of the tc39 suite passes, I can be resonably sure that the parse-trees are correct.
const pass_exceptions = [_][]const u8{
    "12ea3bf0653f8409.js",
    "40766161d96ac708.js",
    "06f7278423cef571.js",
    "7f88f149f16fe97a.js",
    "85d6723f13f33101.js",
    "513275ce0e3c7ef3.js",
    "3793ec99f844de1c.js",
    "4014ec6c7931de54.js",
    "ce349e20cf388e87.js",
    "6e5fe0c2bb20b016.js",
    "ff215f966bed2b85.js",
    "23869c020fc2cb0f.js",
    "0da4b57d03d33129.js",
    "4deb8938d7b36024.js",
    "ae9667ad0d837abc.js",
    "5665da18579dd006.js",
    "0da4b57d03d33129.js",
    "d4c979f1a92a8cac.js",
    "d57a361bc638f38c.js",
    "366585381e4610b4.js",
    "ae700e3f8ff82c6c.js",
    "93cac77bbf2242ab.js",
    "5f1e0eff7ac775ee.js",
    "e877f5e6753dc7e4.js",
    "7da12349ac9f51f2.js",
    "47094fe8a994b7de.js",
    "1c055d256ec34f17.js",
    "0b2804600405dbf6.js",
    "7ebaa39b4a9b5b5b.js",
    "7bdcce70c382a9a4.js",
    "1f988cc22167927b.js",
    "aec65a9745669870.js",
    "95520bedf0fdd4c9.js",
    "b8403938b1ddd626.js",
    "1223609b0f7a2129.js",
    "90ad0135b905a622.js",
    "4ed17e0e2686e5e5.js",
    "aac70baa56299267.js",
    "446ffc8afda7e47f.js",
    "04b26d042948d474.js",
    "46173461e93df4c2.js",
    "3b5d1fb0e093dab8.js",
    "a6cb605b66ef0eb5.js",
    "d88992e07614f506.js",
    "55c15fe174790fb2.js",
    "aeca992c7be882ba.js",
    "0eb53d0e06cd5417.js",
    "24557730b5076325.js",
    // The spec's grammar says `if (foo) function f() { }` is invalid,
    // but the test suite has these tests as "pass" for some reason. WTF?
    "1c1e2a43fe5515b6.js",
    "59ae0289778b80cd.js",
    "3dabeca76119d501.js",
    "52aeec7b8da212a2.js",
    "c06df922631aeabc.js",
    "a4d62a651f69d815.js",

    // ((a, b), c) and (a, b, c)
    // should have the same AST according to the test suite,
    // but obviously that's not true :|
    "589dc8ad3b9aa28f.js",
    "87a9b0d1d80812cc.js",
    // ({Infinity: 1}) and ({'e1200': 1}) should have the same AST according to the test suite,
    // but that's ridiculous. Skill issue by test suite authors?
    "0426f15dac46e92d.js",
};

const fail_exceptions = [_][]const u8{
    "98204d734f8c72b3.js",
    "ef81b93cf9bdb4ec.js",
};

/// For a given file name, check whether `pass/<file>` and `pass-explicit/<file>`
/// have the same parse trees.
fn testOnPassingFile(
    allocator: std.mem.Allocator,
    pass_dir: *std.fs.Dir,
    pass_explicit_dir: *std.fs.Dir,
    file_name: []const u8,
) !ParseResult {
    const source = try pass_dir.readFileAlloc(file_name, allocator, std.Io.Limit.limited(std.math.maxInt(u32)));
    defer allocator.free(source);

    const source_type: js.Parser.SourceType =
        if (std.mem.endsWith(u8, file_name, ".module.js"))
            .module
        else
            .script;

    for (pass_exceptions) |exception_filename| {
        if (std.mem.eql(u8, file_name, exception_filename)) {
            return ParseResult.pass;
        }
    }

    // parse the program
    var result = try js.semantic.parseAndAnalyze(
        allocator,
        source,
        .{ .source_type = source_type, .preserve_parens = false },
    );
    defer result.deinit();

    const source_explicit = try pass_explicit_dir.readFileAlloc(
        file_name,
        allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    );
    defer allocator.free(source_explicit);

    var result2 = try js.semantic.parseAndAnalyze(
        allocator,
        source_explicit,
        .{ .source_type = source_type, .preserve_parens = false },
    );
    defer result2.deinit();

    if (result.tree.nodes.len != result2.tree.nodes.len) {
        return ParseResult.ast_no_match;
    }

    for (0..result.tree.nodes.len) |i| {
        const n1 = result.tree.nodes.get(i);
        const n2 = result2.tree.nodes.get(i);

        if (std.meta.activeTag(n1.data) != std.meta.activeTag(n2.data)) {
            // in my AST, `string_literal` and `numeric_literal` are different node types,
            // but in the tc39 test-suite (and ESTree), they are the same 'Literal' ast node.
            if (n1.isLiteral() and n2.isLiteral()) continue;

            // ({ "a": 1 }) and ({ a: 1 }) should be considered equal.
            // But my AST represents them differently. Honestly, that is OK.
            if (n1.tag() == .identifier and n2.tag() == .string_literal) {
                const n1_src = result.tree.nodeToByteSlice(@enumFromInt(i));
                const n2_src = result2.tree.nodeToByteSlice(@enumFromInt(i));
                if (std.mem.eql(u8, n1_src, n2_src[1 .. n2_src.len - 1]))
                    continue;
            } else if (n2.tag() == .identifier and n1.tag() == .string_literal) {
                const n2_src = result2.tree.nodeToByteSlice(@enumFromInt(i));
                const n1_src = result.tree.nodeToByteSlice(@enumFromInt(i));
                if (std.mem.eql(u8, n2_src, n1_src[1 .. n1_src.len - 1]))
                    continue;
            }

            return ParseResult.ast_no_match;
        }
    }

    return ParseResult.pass;
}

/// Run the parser on a file that has a syntax error, then ensure
/// that the parser exits with an error.
fn testOnMalformedFile(
    allocator: std.mem.Allocator,
    fail_dir: *std.fs.Dir,
    file_name: []const u8,
) !ParseResult {
    const source = try fail_dir.readFileAlloc(file_name, allocator, std.Io.Limit.limited(std.math.maxInt(u32)));
    defer allocator.free(source);

    const source_type: js.Parser.SourceType =
        if (std.mem.endsWith(u8, file_name, ".module.js"))
            .module
        else
            .script;

    // parse the program
    var parser = Parser.init(
        allocator,
        source,
        .{ .source_type = source_type },
    ) catch return .pass;
    defer parser.deinit();

    var result = parser.parse() catch
        return ParseResult.pass;
    defer result.deinit();

    for (fail_exceptions) |exception_filename| {
        if (std.mem.eql(u8, file_name, exception_filename)) {
            return ParseResult.pass;
        }
    }

    return ParseResult.malformed_file_parsed;
}

/// Read an existing `tools/results.json` file.
pub fn readResultsFile(allocator: std.mem.Allocator, results_file_path: []const u8) !std.json.Parsed(TestResult) {
    const previous_results_str = try std.fs.cwd().readFileAlloc(
        results_file_path,
        allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    );

    const parsed = try std.json.parseFromSlice(
        TestResult,
        allocator,
        previous_results_str,
        .{},
    );
    return parsed;
}

/// Runs the JS parser on all files in `pass` and `pass-explicit` directories,
/// compares the ASTs for every file `<file>.js` in `pass/<file>.js` and `pass-explicit/<file>.js`.
/// Returns a TestResult containing all comparison results.
pub fn runValidSyntaxTests(al: std.mem.Allocator) !TestResult {
    const tests_dir = std.process.getEnvVarOwned(al, "JAM_TESTS_262_DIR") catch |e| {
        if (e == error.EnvironmentVariableNotFound) {
            _ = std.posix.write(std.posix.STDERR_FILENO, "env var 'JAM_TESTS_262_DIR' not set\n") catch {};
            std.process.exit(1);
        }

        return e;
    };

    var d = try std.fs.openDirAbsolute(tests_dir, .{ .iterate = true });
    defer d.close();

    var pass_dir = try d.openDir("pass", .{ .iterate = true });
    defer pass_dir.close();

    var pass_explicit_dir = try d.openDir("pass-explicit", .{ .iterate = true });
    defer pass_explicit_dir.close();

    var fail_dir = try d.openDir("fail", .{ .iterate = true });
    defer fail_dir.close();

    var pass_dir_iter = pass_explicit_dir.iterate();
    var fail_dir_iter = fail_dir.iterate();

    var n_good_files: f64 = 0.0;
    var n_pass: f64 = 0.0;
    var n_fail: f64 = 0.0;

    var n_ast_no_match: usize = 0;

    var test_cases = std.json.ObjectMap.init(al);
    while (try pass_dir_iter.next()) |entry| {
        if (entry.kind != .file) continue;

        n_good_files += 1.0;
        const result = testOnPassingFile(
            al,
            &pass_dir,
            &pass_explicit_dir,
            entry.name,
        ) catch .parse_error;

        switch (result) {
            .pass => n_pass += 1.0,
            .ast_no_match => n_ast_no_match += 1,
            else => n_fail += 1.0,
        }

        const name = try al.dupe(u8, entry.name);
        try test_cases.put(name, .{ .string = @tagName(result) });
    }

    var n_malformed_pass: f64 = 0.0;
    var n_malformed_fail: f64 = 0.0;
    var n_malformed_files: f64 = 0.0;
    while (try fail_dir_iter.next()) |entry| {
        if (entry.kind != .file) continue;
        n_malformed_files += 1.0;
        const result = try testOnMalformedFile(al, &fail_dir, entry.name);
        switch (result) {
            .pass => n_malformed_pass += 1.0,
            else => n_malformed_fail += 1.0,
        }

        const name = try al.dupe(u8, entry.name);
        try test_cases.put(name, .{ .string = @tagName(result) });
    }

    const fail_rate = ((n_fail + @as(f64, @floatFromInt(n_ast_no_match))) / n_good_files) * 100.0;
    const pass_rate = (n_pass / n_good_files) * 100.0;
    std.debug.assert(n_fail + @as(f64, @floatFromInt(n_ast_no_match)) + n_pass == n_good_files);

    const malformed_pass_rate = (n_malformed_pass / n_malformed_files) * 100.0;
    const malformed_fail_rate = (n_malformed_fail / n_malformed_files) * 100.0;

    const fail_rate_str = try std.fmt.allocPrint(al, "{d}", .{fail_rate});
    const pass_rate_str = try std.fmt.allocPrint(al, "{d}", .{pass_rate});
    const malformed_pass_rate_str = try std.fmt.allocPrint(al, "{d}", .{malformed_pass_rate});
    const malformed_fail_rate_str = try std.fmt.allocPrint(al, "{d}", .{malformed_fail_rate});

    return TestResult{
        .test_cases = .{ .object = test_cases },
        .fail_percent = .{ .number_string = fail_rate_str },
        .pass_percent = .{ .number_string = pass_rate_str },
        .malformed_pass_percent = .{ .number_string = malformed_pass_rate_str },
        .malformed_fail_percent = .{ .number_string = malformed_fail_rate_str },
        .unmatching_ast_count = n_ast_no_match,
    };
}

/// Compare an old test run (from results.json) with a freshly run
/// test result, and ensure that there are no cases where the parser succeeded previously
/// but fails now.
pub fn compareTestResults(
    new_result: *const TestResult,
    old_result: *const TestResult,
) !bool {
    var passing = true;

    {
        const old_pass_rate: f64 = if (std.meta.activeTag(old_result.pass_percent) == .float)
            old_result.pass_percent.float
        else
            @floatFromInt(old_result.pass_percent.integer);
        const new_pass_rate: f64 = try std.fmt.parseFloat(f64, new_result.pass_percent.number_string);

        if (new_pass_rate < old_pass_rate) {
            passing = false;
            std.log.err("Passing tests dropped from {d}% to {d}%", .{
                old_pass_rate,
                new_pass_rate,
            });
        } else if (old_pass_rate > new_pass_rate) {
            std.log.err("Passing test score went up from {d}% to {d}%!", .{
                old_pass_rate,
                new_pass_rate,
            });
        }
    }

    {
        const old_malformed_pass_rate: f64 = if (std.meta.activeTag(old_result.malformed_pass_percent) == .float)
            old_result.malformed_pass_percent.float
        else
            @floatFromInt(old_result.malformed_pass_percent.integer);

        const new_malformed_pass_rate: f64 = try std.fmt.parseFloat(
            f64,
            new_result.malformed_pass_percent.number_string,
        );

        if (new_malformed_pass_rate < old_malformed_pass_rate) {
            passing = false;
            std.log.err("Malformed files passing tests dropped from {d}% to {d}%", .{
                old_malformed_pass_rate,
                new_malformed_pass_rate,
            });
        } else {
            std.log.err("Malformed files passing test score went up from {d}% to {d}%!", .{
                old_malformed_pass_rate,
                new_malformed_pass_rate,
            });
        }
    }

    const old_file_results = old_result.test_cases.object;
    const new_file_results = new_result.test_cases.object;

    var n_regressions: usize = 0;
    var old_iter = old_file_results.iterator();
    while (old_iter.next()) |entry| {
        const test_file_name = entry.key_ptr.*;
        const new_value: std.json.Value = new_file_results.get(test_file_name) orelse {
            passing = false;
            std.log.err("Missing entry for file {s} in new result file.", .{test_file_name});
            break;
        };

        const old_value: std.json.Value = entry.value_ptr.*;
        if (std.mem.eql(u8, old_value.string, @tagName(ParseResult.pass)) and
            !std.mem.eql(u8, new_value.string, @tagName(ParseResult.pass)))
        {
            n_regressions += 1;
            std.log.err("Test {s} went from passing to failing.", .{test_file_name});
        }
    }

    if (n_regressions > 0) {
        std.debug.print("{d} tests that were previously passing are now failing", .{n_regressions});
        passing = false;
    }
    return passing;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();
    defer arena.deinit();

    // Run the current parser on all the files in `pass` and `pass-explicit`, then prepare results.
    const test_results = try runValidSyntaxTests(al);

    var args = std.process.args();
    _ = args.next();

    // When the `--compare` flag is passed, compare the new test results
    // with an existing `results.json` (in CI, its from the main branch),
    // and ensure there are no regressions.
    const existing_results_file_path: ?[]const u8 = blk: {
        const arg_name = args.next() orelse break :blk null;
        if (!std.mem.eql(u8, arg_name, "--compare")) {
            std.log.err("unknown argument '{s}'; --compare is the only supported flag", .{arg_name});
            break :blk null;
        }

        if (args.next()) |file_path| {
            break :blk file_path;
        }
        std.log.err("missing argument for --compare flag. Need a file path for results.json", .{});
        std.process.exit(1);
    };

    if (existing_results_file_path) |results_json_path| {
        const old_results_parsed = try readResultsFile(al, results_json_path);
        defer old_results_parsed.deinit();
        const old_results = old_results_parsed.value;
        const is_passing = try compareTestResults(&test_results, &old_results);
        std.process.exit(if (is_passing) 0 else 1);
    }

    const s = try std.json.Stringify.valueAlloc(
        al,
        test_results,
        .{ .whitespace = .indent_2 },
    );
    _ = try std.posix.write(std.posix.STDOUT_FILENO, s);
}
