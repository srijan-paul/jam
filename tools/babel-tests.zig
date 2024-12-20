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

/// Deep equality check for dynamic JSON values.
fn isJsonEqual(a_json: std.json.Value, b_json: std.json.Value) bool {
    // Convert .number_string and .integer to .float
    // This will mean that some comparisons run the risk of being imprecise (as floats always are),
    // but it's the simplest way to handle cases where fields in babel's output are parsed as flats,
    // while those in jam are parsed as integers.
    var a = a_json;
    if (std.meta.activeTag(a) == .number_string) {
        a = json.Value{ .float = std.fmt.parseFloat(f64, a.number_string) catch return false };
    } else if (std.meta.activeTag(a) == .integer) {
        a = json.Value{ .float = @floatFromInt(a.integer) };
    }

    var b = b_json;
    if (std.meta.activeTag(b) == .number_string) {
        b = json.Value{ .float = std.fmt.parseFloat(f64, b.number_string) catch return false };
    } else if (std.meta.activeTag(b) == .integer) {
        b = json.Value{ .float = @floatFromInt(b.integer) };
    }

    if (std.meta.activeTag(a) != std.meta.activeTag(b))
        return false;

    switch (a) {
        .null => return true,
        .bool => return a.bool == b.bool,
        .string => return std.mem.eql(u8, a.string, b.string),
        .number_string => return std.mem.eql(u8, a.number_string, b.number_string),
        .integer => return a.integer == b.integer,
        .float => return a.float == b.float,

        .object => |a_o| {
            const b_o = b.object;

            if (a_o.count() != b_o.count()) return false;

            var a_iter = a_o.iterator();
            while (a_iter.next()) |a_entry| {
                const b_val = b_o.get(a_entry.key_ptr.*) orelse return false;
                const a_val = a_entry.value_ptr.*;
                if (!isJsonEqual(a_val, b_val)) return false;
            }

            return true;
        },

        .array => |a_arr| {
            const b_arr = b.array;
            if (a_arr.items.len != b_arr.items.len) return false;
            for (0.., a_arr.items) |i, a_val|
                if (!isJsonEqual(a_val, b_arr.items[i])) return false;

            return true;
        },
    }
}

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

    const babel_config: json.Value = blk: {
        if (d.readFileAlloc(al, "options.json", std.math.maxInt(u32))) |json_src| {
            break :blk (try json.parseFromSlice(json.Value, al, json_src, .{})).value;
        } else |_| {
            break :blk json.Value{ .object = json.ObjectMap.init(al) };
        }
    };

    var parser_config: Parser.Config = .{};
    switch (babel_config) {
        .object => |o| {
            const soruce_type = o.get("sourceType") orelse json.Value{ .string = "script" };
            switch (soruce_type) {
                .string => |s| {
                    if (std.mem.eql(u8, s, "module"))
                        parser_config.source_type = .module
                    else if (std.mem.eql(u8, s, "script"))
                        parser_config.source_type = .script;
                },
                else => {},
            }
        },
        else => {},
    }

    const should_error =
        std.meta.activeTag(expected.value) == .object and
        expected.value.object.contains("error");

    // Parse the file
    const source = try d.readFileAlloc(al, filename, std.math.maxInt(u32));

    var parser = Parser.init(al, source, parser_config) catch
        return if (should_error) .pass else .fail_with_error;
    defer parser.deinit();

    var result = parser.parse() catch
        return if (should_error) .pass else .fail_with_error;
    defer result.deinit();

    // An error was expected, but the parser just succeeded
    if (should_error) return .fail_did_not_error;

    const estree_json = try js.estree.toJsonObject(
        al,
        result.tree,
        js.estree.BabelEstreeOptions,
    );
    defer estree_json.deinit();

    // write the stringified output to a `output.jam.json`
    {
        const estree_json_str = try std.json.stringifyAlloc(al, estree_json.tree, .{
            .whitespace = .indent_2,
            .emit_null_optional_fields = false,
        });
        defer al.free(estree_json_str);

        try d.writeFile(.{
            .sub_path = "output.jam.json",
            .data = estree_json_str,
            .flags = .{},
        });
    }

    const equal = isJsonEqual(expected.value, estree_json.tree);
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
