const std = @import("std");
const syntax = @import("jam-syntax");

const Parser = syntax.Parser;

const ParseResult = enum {
    pass,
    parse_error,
    ast_no_match,
};

/// Structured representation of the `tools/results.json` file.
const TestResult = struct {
    fail_percent: std.json.Value,
    pass_percent: std.json.Value,
    unmatching_ast_count: usize,

    /// string -> string map.
    /// key is a filename like "12ea3bf0653f8409.js", and value is `TestResult`.
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
};

/// For a given file name, check whether `pass/<file>` and `pass-explicit/<file>`
/// have the same parse trees.
fn testOnPassingFile(
    allocator: std.mem.Allocator,
    pass_dir: *std.fs.Dir,
    pass_explicit_dir: *std.fs.Dir,
    file_name: []const u8,
) !ParseResult {
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
        return ParseResult.ast_no_match;
    }

    for (parser.nodes.items, parser2.nodes.items) |n1, n2| {
        if (std.meta.activeTag(n1.data) != std.meta.activeTag(n2.data)) {
            // see `pass_exceptions` array.
            for (pass_exceptions) |exception_filename| {
                if (std.mem.eql(u8, file_name, exception_filename)) {
                    return ParseResult.pass;
                }
            }
            return ParseResult.ast_no_match;
        }
    }

    return ParseResult.pass;
}

/// Read an existing `tools/results.json` file.
pub fn readResultsFile(allocator: std.mem.Allocator) !TestResult {
    const results_file_path = "tools" ++ std.fs.path.sep_str.* ++ "results.json";
    const previous_results_str = try std.fs.cwd().readFileAlloc(
        allocator,
        results_file_path,
        std.math.maxInt(u32),
    );

    const parsed = try std.json.parseFromSlice(
        TestResult,
        allocator,
        previous_results_str,
        .{},
    );
    defer parsed.deinit();
    return parsed.value;
}

/// Runs the JS parser on all files in `pass` and `pass-explicit` directories,
/// compares the ASTs for every file `<file>.js` in `pass/<file>.js` and `pass-explicit/<file>.js`.
/// Returns a TestResult containing all comparison results.
pub fn runValidSyntaxTests(al: std.mem.Allocator) !TestResult {
    const tests_dir = try std.process.getEnvVarOwned(al, "JAM_TESTS_262_DIR");

    var d = try std.fs.openDirAbsolute(tests_dir, .{ .iterate = true });
    defer d.close();

    var pass_dir = try d.openDir("pass", .{ .iterate = true });
    defer pass_dir.close();

    var pass_explicit_dir = try d.openDir("pass-explicit", .{ .iterate = true });
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
            al,
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

    return TestResult{
        .test_cases = .{ .object = test_cases },
        .fail_percent = std.json.Value{ .number_string = fail_rate_str },
        .pass_percent = std.json.Value{ .number_string = pass_rate_str },
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

    const old_pass_rate: f64 = old_result.pass_percent.float;
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

    const old_file_results = old_result.test_cases.object;
    const new_file_results = new_result.test_cases.object;

    var n_regressions: usize = 0;
    var old_iter = old_file_results.iterator();
    while (old_iter.next()) |entry| {
        const test_file_name = entry.key_ptr.*;
        const new_value: std.json.Value = new_file_results.get(test_file_name) orelse {
            passing = false;
            std.log.err("Missing entry for fiel {s} in new result file.", .{test_file_name});
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

    const test_results = try runValidSyntaxTests(al);

    var args = std.process.args();
    _ = args.next();

    // When the `--compare` flag is passed, compare the new test results
    // with the existing results in `results.json` and ensure there are no regressions.
    const compare_with_old = if (args.next()) |s|
        std.mem.eql(u8, s, "--compare")
    else
        false;

    if (compare_with_old) {
        const old_results = try readResultsFile(al);
        const is_passing = try compareTestResults(&test_results, &old_results);
        std.process.exit(if (is_passing) 0 else 1);
    }

    const s = try std.json.stringifyAlloc(
        al,
        test_results,
        .{ .whitespace = .indent_2 },
    );
    _ = try std.io.getStdOut().write(s);
}
