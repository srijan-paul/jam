const std = @import("std");
const Token = @import("./tokenizer.zig").Token;
const util = @import("util");
const types = util.types;

/// Used to represent a list of nodes in the AST.
pub const SubRange = struct {
    pub const Index = u32;
    start: u32,
    end: u32,
};

/// A CSS Rule like:
/// ```css
/// p.foo + h1 {
///    color: red;
/// }
/// ```
pub const QualifiedRule = struct {
    prelude: Node.Index,
    body: Node.Index,
};

/// Payload for each kind of node in the AST.
pub const NodeData = union(enum) {
    rule: QualifiedRule,
    block: SubRange,
    type_selector: Token.Index,
    selector_list: SubRange,
    operator: Token.Index,
    /// The Root AST node.
    /// This is a list of rules.
    stylesheet: SubRange,
};

pub const Node = struct {
    pub const Index = enum(u32) { _ };
    data: NodeData,
    /// Byte index into the source code where the start ends.
    start: u32,
    /// Byte index into the source code where the node ends
    end: u32,
};

/// `Node`s are converted into `PrettyNode`s for pretty printing.
pub const PrettyNode = union(enum) {
    rule: struct {
        prelude: *PrettyNode,
        body: *PrettyNode,
    },
    operator: []const u8,
    stylesheet: []PrettyNode,
    type_selector: []const u8,
    selector_list: []PrettyNode,
    block: []PrettyNode,
};

const Parser = @import("./parser.zig");

const copy = util.copy;

/// Convert a SubRange into a slice of PrettyNode
fn prettySubRange(
    p: *const Parser,
    al: std.mem.Allocator,
    range: SubRange,
) error{OutOfMemory}![]PrettyNode {
    const node_ids = p.subRangeAsSlice(range);
    var pretty_nodes = try al.alloc(PrettyNode, node_ids.len);

    for (0.., node_ids) |i, node_id| {
        const pretty_node = try toPretty(p, al, node_id);
        pretty_nodes[i] = pretty_node;
    }

    return pretty_nodes;
}

/// Convert an ast.Node to an ast.PrettyNode, which
/// can then be pretty printed.
fn toPretty(
    p: *const Parser,
    al: std.mem.Allocator,
    node_id: Node.Index,
) error{OutOfMemory}!PrettyNode {
    const node = p.getNode(node_id);
    switch (node.data) {
        .operator => |op| return .{ .operator = p.getToken(op).toByteSlice(p.source) },
        .type_selector => |sel| return .{ .type_selector = p.getToken(sel).toByteSlice(p.source) },
        .selector_list => |children| {
            return PrettyNode{
                .selector_list = try prettySubRange(p, al, children),
            };
        },
        .rule => |rule| {
            return .{
                .rule = .{
                    .prelude = try copy(al, try toPretty(p, al, rule.prelude)),
                    .body = try copy(al, try toPretty(p, al, rule.body)),
                },
            };
        },

        .block => |block| {
            return .{ .block = try prettySubRange(p, al, block) };
        },

        .stylesheet => |sheet| {
            return .{ .stylesheet = try prettySubRange(p, al, sheet) };
        },
    }
}

/// Convert a CSS AST into a JSON string representation.
/// The slice returned is owned by the caller.
pub fn toJsonString(
    allocator: std.mem.Allocator,
    parser: *const Parser,
    node: Node.Index,
) error{OutOfMemory}![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const al = arena.allocator();

    const pretty_node = try toPretty(parser, al, node);
    return try std.json.Stringify.valueAlloc(allocator, pretty_node, .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });
}
