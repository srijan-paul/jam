const root = @import("./root.zig");
const std = @import("std");

// var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const wasm_allocator = std.heap.wasm_allocator;

pub export fn alloc(size: usize) ?[*]u8 {
    const buf = wasm_allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

pub export fn free(ptr: [*]u8, size: usize) void {
    wasm_allocator.free(ptr[0..size]);
}

fn parseModuleImpl(source: []const u8) ![*]const u8 {
    var parser = try root.Parser.init(wasm_allocator, source, .{ .source_type = .module });
    defer parser.deinit();
    const json_str = blk: {
        const root_id = parser.parse() catch |err| {
            if (parser.diagnostics.items.len > 0) {
                const err_str = try std.json.stringifyAlloc(
                    wasm_allocator,
                    parser.diagnostics.items[0].message,
                    .{},
                );
                break :blk err_str;
            }

            return err;
        };

        const json_str = try root.estree.toJsonString(wasm_allocator, &parser, root_id);
        break :blk json_str;
    };

    var json_str_z = try wasm_allocator.realloc(json_str, json_str.len + 1);
    json_str_z.ptr[json_str.len] = 0;
    return json_str_z.ptr;
}

pub export fn parseModule(source_bytes: [*]const u8, len: u32) ?[*]const u8 {
    const source: []const u8 = source_bytes[0..len];
    return parseModuleImpl(source) catch null;
}

pub export fn freeJsonAstWasm(json_ast: [*]const u8, len: u32) void {
    const s: []const u8 = json_ast[0..len];
    wasm_allocator.free(s);
}
