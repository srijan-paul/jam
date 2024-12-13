const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("./ast.zig");

const NodeKind = std.meta.Tag(ast.NodeData);

/// Create a struct that traverses an AST, and calls `TControl.onEnter`
/// every time it enters a node, and `TControl.onExit` every time it exits a node.
/// This structure that determines what actions to perform is called a "Control".
/// (can I come up with a better name?).
pub fn Traverser(TControl: type) type {
    const ctrl_type: std.builtin.Type = @typeInfo(TControl);
    if (std.meta.activeTag(ctrl_type) != .@"struct") {
        @compileError("type of `control` passed to `Traverser` must have a struct type");
    }

    return struct {
        const Self = @This();

        t: *const ast.Tree,
        ctrl: *TControl,

        // TODO: get rid of this `anyerror`.
        fn subRange(self: *Self, maybe_statements: ?ast.SubRange) anyerror!void {
            const stat_range = maybe_statements orelse return;
            const children = self.t.getSubRange(stat_range.from, stat_range.to);
            for (children) |child| try self.visit(child);
        }

        fn binaryPayload(self: *Self, binary: *const ast.BinaryPayload) anyerror!void {
            try self.visit(binary.lhs);
            try self.visit(binary.rhs);
        }

        fn visit(self: *Self, node_id: ast.Node.Index) anyerror!void {
            const pl = self.t.nodeData(node_id).*;
            if (@hasDecl(TControl, "onEnter"))
                try self.ctrl.onEnter(node_id, pl);

            switch (pl) {
                .program => |maybe_statements| if (maybe_statements) |stmts| {
                    try self.subRange(stmts);
                },
                .expression_statement => |e| try self.visit(e),
                .assignment_expr,
                .binary_expr,
                .assignment_pattern,
                => |binary| try self.binaryPayload(&binary),
                .variable_declaration => |decl| try self.subRange(decl.declarators),

                // leaf nodes cannot be explored further
                .literal => {},
                .identifier => {},
                else => std.debug.panic("not implemented: {s}!\n", .{@tagName(std.meta.activeTag(pl))}),
            }

            if (@hasDecl(TControl, "onExit"))
                try self.ctrl.onExit(node_id, pl);
        }

        pub fn traverse(self: *Self) !void {
            try self.visit(self.t.root);
        }
    };
}

const t = std.testing;

fn parse(program: []const u8) !Parser.Result {
    var parser = try Parser.init(t.allocator, program, .{});
    defer parser.deinit();
    return try parser.parse();
}

fn enterNode(_: ast.Node.Index, pl: ast.NodeData) !void {
    std.debug.print("visiting: {s}\n", .{@tagName(std.meta.activeTag(pl))});
}

const TestCtrl = struct {
    allocator: std.mem.Allocator,
    visited: std.ArrayList(NodeKind),

    pub fn init() TestCtrl {
        return .{
            .allocator = t.allocator,
            .visited = std.ArrayList(NodeKind).init(t.allocator),
        };
    }

    pub fn deinit(self: *TestCtrl) void {
        self.visited.deinit();
    }

    pub fn onEnter(self: *TestCtrl, _: ast.Node.Index, pl: ast.NodeData) !void {
        try self.visited.append(std.meta.activeTag(pl));
    }
};

test Traverser {
    var p = try parse("a = 1 + 2;");
    defer p.deinit();

    var ctrl = TestCtrl.init();
    defer ctrl.deinit();

    var traverser = Traverser(TestCtrl){ .t = p.tree, .ctrl = &ctrl };
    try traverser.traverse();

    try t.expectEqualSlices(NodeKind, &.{
        .program,
        .expression_statement,
        .assignment_expr,
        .identifier,
        .binary_expr,
        .literal,
        .literal,
    }, ctrl.visited.items);
}
