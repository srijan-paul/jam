const std = @import("std");
const block = @import("./block.zig");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn parseRule(p: *Parser) Parser.Error!ast.Node.Index {
    switch (p.current_token.tag()) {
        else => return qualifiedRule(p),
    }
}

/// https://drafts.csswg.org/css-syntax-3/#consume-qualified-rule
pub fn qualifiedRule(p: *Parser) Parser.Error!ast.Node.Index {
    // TODO: handle empty preludes.
    const start_pos = p.current_token.start;
    var selector_list: std.ArrayList(ast.Node.Index) = .{};
    try selector_list.ensureTotalCapacity(p.allocator, 4);

    const body = blk: {
        while (p.current_token.tag() != .eof) {
            switch (p.current_token.tag()) {
                .@"}" => unreachable, // TODO: parse error here
                .@"{" => break :blk try block.parseBlock(p),
                else => {
                    try selector_list.append(p.allocator, try parseComponent(p));
                },
            }
        }

        try p.diagnostics.emit(
            p.current_token.startCoord(p.source),
            "Unexpected end of file while looking for rule body",
            .{},
        );

        return Parser.Error.UnexpectedEof;
    };

    const end_pos = p.current_token.start;

    const prelude = switch (selector_list.items.len) {
        0 => unreachable, // TODO: error
        1 => selector_list.items[0],
        else => try p.addNode(
            .{ .selector_list = try p.newSubRange(selector_list.items) },
            start_pos,
            end_pos,
        ),
    };

    const rule = ast.QualifiedRule{ .prelude = prelude, .body = body };
    return p.addNode(.{ .rule = rule }, start_pos, end_pos);
}

/// Parse a single component of a selector.
/// E.g: parse 'body' from `body + p + .foo`
pub fn parseComponent(p: *Parser) Parser.Error!ast.Node.Index {
    switch (p.current_token.tag()) {
        .identifier => {
            const start = p.current_token.start;
            const end = p.current_token.start + p.current_token.len;

            const token_id = try p.saveToken(try p.nextToken());
            return p.addNode(
                ast.NodeData{ .type_selector = token_id },
                start,
                end,
            );
        },

        else => std.debug.panic("not implemented\n", .{}),
    }
}

test parseRule {
    const t = std.testing;
    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();

    const source = "body { }";
    var p = try Parser.init(&arena, source);

    const sheet_index = try p.parse();
    try t.expectEqual(.stylesheet, std.meta.activeTag(p.getNode(sheet_index).data));

    const sheet = p.getNode(sheet_index).data.stylesheet;
    const start_index = sheet.start;
    const end_index = sheet.end;

    try t.expectEqual(1, end_index - start_index);

    const rule0 = p.getNode(p.node_refs.items[start_index]).data.rule;

    try t.expectEqual(.block, std.meta.activeTag(p.getNode(rule0.body).data));
    try t.expectEqual(.type_selector, std.meta.activeTag(p.getNode(rule0.prelude).data));
}
