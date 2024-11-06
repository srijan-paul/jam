const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn parseBlock(p: *Parser) Parser.Error!ast.Node.Index {
    const lbrac = try p.nextToken();
    const start_pos = lbrac.start;

    const body = try parseBlockBody(p);

    const rbrac = try p.expect(.@"}");
    const end_pos = rbrac.start + rbrac.len;

    return p.addNode(.{ .block = body }, start_pos, end_pos);
}

pub fn parseBlockBody(p: *Parser) Parser.Error!ast.SubRange {
    while (p.current_token.tag() != .@"}") {
        _ = try p.nextToken();
    }
    return ast.SubRange{ .start = 0, .end = 0 };
}

test parseBlock {
    const t = std.testing;
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();

    const source = "{ }";
    var p = try Parser.init(&arena, source);

    const block_id = try parseBlock(&p);
    const block = p.getNode(block_id);

    try t.expectEqual(std.meta.activeTag(block.data), .block);
    try t.expectEqual(block.start, 0);
    try t.expectEqual(block.end, source.len);
}
