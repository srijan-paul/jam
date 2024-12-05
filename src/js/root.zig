pub const Parser = @import("parser.zig");
pub const Token = @import("token.zig");
pub const ast = @import("ast.zig");
pub const Tokenizer = @import("tokenize.zig");
pub const estree = @import("estree.zig");

const std = @import("std");

/// Parse a JavaScript source as either a module or script.
/// Caller owns the returned tree, and must call `deinit`.
pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    source_type: Parser.SourceType,
) Parser.Error!Parser.Result {
    var parser = try Parser.init(allocator, source, .{ .source_type = source_type });
    defer parser.deinit();
    return try parser.parse();
}

/// Parse a JavaScript source as an ES Module.
/// Caller owns the returned tree, and must call `deinit`.
pub fn parseModule(
    allocator: std.mem.Allocator,
    source: []const u8,
) Parser.Error!Parser.Result {
    return parse(allocator, source, .module);
}

/// Parse a JavaScript source as a script.
/// Caller owns the returned tree, and must call `deinit`.
pub fn parseScript(
    allocator: std.mem.Allocator,
    source: []const u8,
) Parser.Error!Parser.Result {
    return parse(allocator, source, .script);
}

test {
    const util = @import("util");

    std.testing.refAllDeclsRecursive(@This());
    std.testing.refAllDeclsRecursive(util);
}
