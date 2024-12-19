// Test runner for Babel's test suite.
// Parses all files matching `babel/**/*.js`,
// then compares the resulting ASTs with @babel/parser's output.

const std = @import("std");
const js = @import("js");

const json = std.json;

const Parser = js.Parser;
const Allocator = std.mem.Allocator;

/// Result of comparing output from `@babel/parser` to that of jam
const ParseResult = enum {
    /// The parse was successful, and the AST matched
    pass,
    /// An unexpected error was raised during parsing.
    fail_with_error,
    /// The parse was successful, but the AST did not match
    fail_with_ast_mismatch,
    /// An error was expected, but the parse was successful
    fail_did_not_error,
};

const tests_to_run = [_][]const u8{
    "core",
    "es2015",
    "es2016",
    "es2017",
    "es2018",
    "es2019",
    "es2020",
    "es2021",
    "es2022",
    "es2024",
    "estree",
    "esprima",
    "comments",
};

fn runTest(al: Allocator, d: std.fs.Dir, key: []const u8, filename: []const u8, out: *json.ObjectMap) !ParseResult {
    // Read the expected json output
    const expected_json = try d.readFileAlloc(al, "output.json", std.math.maxInt(u32));
    const expected = try json.parseFromSlice(json.Value, al, expected_json, .{});
    defer expected.deinit();

    const should_error =
        std.meta.activeTag(expected.value) == .object and
        expected.value.object.contains("error");

    // Parse the file
    const source = try d.readFileAlloc(al, filename, std.math.maxInt(u32));

    var parser = Parser.init(al, source, .{ .source_type = .script }) catch
        return if (should_error) .pass else .fail_with_error;
    defer parser.deinit();

    var result = parser.parse() catch
        return if (should_error) .pass else .fail_with_error;
    defer result.deinit();

    // An error was expected, but the parser just succeeded
    if (should_error) return .fail_did_not_error;

    const estree_json: []const u8 = try js.estree.toJsonString(
        al,
        result.tree,
        js.estree.BabelEstreeOptions,
    );

    try d.writeFile(.{
        .sub_path = "output.jam.json",
        .data = estree_json,
        .flags = .{},
    });

    const equal = std.mem.eql(u8, expected_json, estree_json);
    const test_result: ParseResult = if (equal)
        .pass
    else
        .fail_with_ast_mismatch;

    try out.put(key, json.Value{ .string = @tagName(test_result) });
    return test_result;
}

fn runTests(al: Allocator, babel_tests_dir: []const u8) !json.ObjectMap {
    var out = json.ObjectMap.init(al);
    var tests_dir = try std.fs.cwd().openDir(babel_tests_dir, .{});
    defer tests_dir.close();

    for (tests_to_run) |subdir_path| {
        var d = try tests_dir.openDir(subdir_path, .{ .iterate = true });
        defer d.close();

        var it = try d.walk(al);
        while (try it.next()) |entry| {
            if (entry.kind != .file) continue;
            const ext = std.fs.path.extension(entry.path);
            if (!std.mem.eql(u8, ext, ".js")) continue;

            // For every .js file, parse and compare ASTs with babel
            const parent_dir_path = std.fs.path.dirname(entry.path) orelse continue;
            const key = try std.fs.path.join(al, &.{ subdir_path, parent_dir_path });
            _ = try runTest(al, entry.dir, key, entry.basename, &out);
        }
    }

    return out;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const gp_allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(gp_allocator);
    defer arena.deinit();
    const al = arena.allocator();

    const babel_tests_dir = try std.fs.path.join(al, &.{ "tools", "babel" });

    const results_json = try runTests(al, babel_tests_dir);
    const results_json_str = try json.stringifyAlloc(
        al,
        json.Value{ .object = results_json },
        .{ .whitespace = .indent_2 },
    );

    const results_file_path = try std.fs.path.join(al, &.{ "tools", "babel-results.json" });
    var results_file = try std.fs.cwd().createFile(results_file_path, .{});
    defer results_file.close();

    try results_file.writeAll(results_json_str);
}
