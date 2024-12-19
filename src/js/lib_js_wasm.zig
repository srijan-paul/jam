const root = @import("./root.zig");
const std = @import("std");
const util = @import("util");

const wasm_allocator = std.heap.wasm_allocator;

/// Wasm export to allocate a slice of [size] bytes.
pub export fn alloc(size: usize) ?[*]u8 {
    const buf = wasm_allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

/// Wasm export to free a slice of [size] bytes, starting at [ptr]
pub export fn free(ptr: [*]u8, size: usize) void {
    wasm_allocator.free(ptr[0..size]);
}

/// Wasm export to free a ParseResult.
pub export fn freeResult(result: ResultWasm) void {
    if (!result.has_result) return;

    const parse_result = result.result;
    if (parse_result.ok) {
        const len = std.mem.len(parse_result.json_ast);
        wasm_allocator.free(parse_result.json_ast[0..len]);
    } else {
        const len = std.mem.len(parse_result.err.error_message);
        wasm_allocator.free(parse_result.err.error_message[0..len]);
    }
}

/// Result of running the parser on a JavaScript source file.
/// This is a "dumb" struct instead of a tagged union, because using it
/// in WASM code is easier this way.
pub const ParseResult = extern struct {
    /// The AST in JSON format (if parsing succeeded, and `ok` is true).
    /// Accessing this pointer is undefined behavior otherise.
    json_ast: [*:0]const u8,
    /// The error message (if parsing failed, and `ok` is false).
    /// Accessing this pointer is undefined behavior otherise.
    err: extern struct {
        error_message: [*:0]const u8,
        line: u32,
        column: u32,
    },
    /// Whether parsing succeeded.
    /// If false, `diagnostics` will contain the error message.
    ok: bool,
};

pub export const ParseResultSize: u32 = @sizeOf(ParseResult);

/// Null-terminate a slice of bytes
/// This modifies the slice in-place
fn nullTerminate(source: []u8) std.mem.Allocator.Error![:0]u8 {
    const source_len = source.len;
    const source_with_null = try wasm_allocator.realloc(source, source_len + 1);
    source_with_null[source_len] = 0;
    return @ptrCast(source_with_null[0 .. source_len + 1]);
}

/// Parse a JavaScript source file and return a `ParseResult`
fn parseJsImpl(source: []const u8, source_type: root.Parser.SourceType) !ParseResult {
    var parser = try root.Parser.init(wasm_allocator, source, .{ .source_type = source_type });
    defer parser.deinit();

    var result = parser.parse() catch |err| {
        if (parser.diagnostics.items.len > 0) {
            const diagnostic = parser.diagnostics.items[0];
            const error_message = try wasm_allocator.dupeZ(u8, diagnostic.message);

            return ParseResult{
                .ok = false,
                .err = .{
                    .error_message = error_message.ptr,
                    .line = diagnostic.coord.line,
                    .column = diagnostic.coord.column,
                },
                .json_ast = undefined,
            };
        }

        const error_message = try std.mem.concat(wasm_allocator, u8, &.{
            "tokenizer error: ",
            @errorName(err),
        });
        const error_message_z = try nullTerminate(error_message);

        const coord = util.offsets.byteIndexToCoordinate(source, parser.tokenizer.index);
        return ParseResult{
            .ok = false,
            .err = .{
                .error_message = error_message_z.ptr,
                .line = coord.line,
                .column = coord.column,
            },
            .json_ast = undefined,
        };
    };

    defer result.deinit();

    const json_str = try root.estree.toJsonString(wasm_allocator, result.tree, .{});
    const json_str_z = try nullTerminate(json_str);
    return ParseResult{
        .ok = true,
        .err = undefined,
        .json_ast = json_str_z.ptr,
    };
}

pub const ResultWasm = extern struct {
    result: ParseResult,
    has_result: bool,
};

pub export const ResultWasmSize: u32 = @sizeOf(ResultWasm);

pub export fn parseModule(source_bytes: [*]const u8, len: u32) ResultWasm {
    const source: []const u8 = source_bytes[0..len];
    const result = parseJsImpl(source, .module) catch
        return .{ .has_result = false, .result = undefined };
    return .{ .has_result = true, .result = result };
}

pub export fn parseScript(source_bytes: [*]const u8, len: u32) ResultWasm {
    const source: []const u8 = source_bytes[0..len];
    const result = parseJsImpl(source, .script) catch
        return .{ .has_result = false, .result = undefined };
    return .{ .has_result = true, .result = result };
}

pub export fn freeJsonAstWasm(json_ast: [*]const u8, len: u32) void {
    const s: []const u8 = json_ast[0..len];
    wasm_allocator.free(s);
}
