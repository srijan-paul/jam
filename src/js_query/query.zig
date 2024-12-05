const js = @import("js");
const std = @import("std");
const ParsedQuery = @import("./parse.zig").ParsedQuery;

/// The payload of a query that has two sub-queries
const BinaryOpPayload = struct {
    left: Query.Index,
    right: Query.Index,
};

const NodeTag = std.meta.Tag(js.ast.NodeData);
const NodeId = js.ast.Node.Index;

pub const SubRange = struct {
    /// Index of first item in the sub-query list
    start_index: Query.Index = @enumFromInt(0),
    /// 1 + Index of the last item in the sub-query list
    end_index: Query.Index = @enumFromInt(0),
};

/// A parsed query on a JS/TS/JSX syntax tree.
pub const Query = union(enum) {
    /// Used to uniquely identify a query in list of queries
    pub const Index = enum(u32) { _ };
    /// Matches when `right` is a child of `left`.
    child: BinaryOpPayload,
    /// Matches `right when it is` is at the same
    /// depth as `left`.
    sibling: BinaryOpPayload,
    /// Matches the node right next to `left`.
    adjacent: BinaryOpPayload,
    /// Matches when `right` is a descendent of `left`.
    /// `right` doesn't have to be an immediate child.
    descendent: BinaryOpPayload,
    /// Matches a node with a specific tag.
    tag: std.meta.Tag(js.ast.NodeData),
    /// Matches when a node matching a specific query is also
    /// the first child of its parent node.
    first_child: Query.Index,
    /// Matches when a node matching a specific query is also
    /// the last child of its parent node.
    last_child: Query.Index,
    /// Matches when a node matching a specific query is also
    /// the nth child of its parent node.
    nth_child: struct { query: Query.Index, n: u32 },
    // A list of sub-queries that are all `attributes` which we
    // expect to be present on a node that matches.
    attribute_list: struct {
        /// Start index of the sub-attributes in a list of queries
        attrs_start: u32,
        /// End index of the sub-attributes in a list of queries
        attrs_end: u32,
        /// Returns a slice of all attributes in the query
        pub fn getAttributes(self: *const @This(), p: *const ParsedQuery) []const Query {
            return p.queries[self.attrs_start..self.attrs_end];
        }
    },
    /// Matches (possibly nested) field names on any node kind.
    /// `name` can be a single property like `"id"`, or
    /// nested, like `"id.name"`.
    attribute: struct {
        /// The name of the attribute to match
        name: Query.Index,
        /// Expected value
        value: Query.Index,
    },
    /// Part of an `attribute` query. (the `name` field)
    /// E.g: `foo.bar`
    dot_attr: BinaryOpPayload,
    /// Part of an `attribute` query. (the `name` field)
    attr_name: []const u8,
    /// Matches a literal node with the given value
    numeric_literal: f64,

    pub fn matches(self: *const Query, tree: *const js.ast.Tree, node_id: NodeId) bool {
        return switch (self.*) {
            .tag => |tag| tag == tree.tag(node_id),
            else => unreachable,
        };
    }
};

// tests beyond this point

const t = std.testing;
const NodePredicate = fn (node_id: NodeId, tree: *const js.ast.Tree) bool;

/// Search and return the first node that satisfies the predicate.
/// This does not necessarily traverse the AST nodes in a specified order.
fn searchForNode(tree: *const js.ast.Tree, predicate: NodePredicate) ?NodeId {
    for (0..tree.nodes.len) |i| {
        const id: NodeId = @enumFromInt(i);
        if (predicate(id, tree)) {
            return id;
        }
    }

    return null;
}

/// Return `true` if the node is an identifier.
fn isIdentifier(node_id: NodeId, tree: *const js.ast.Tree) bool {
    return tree.tag(node_id) == .identifier;
}

test Query {
    var result = try js.parseModule(t.allocator, "let x = 1;");
    defer result.deinit();

    const query = Query{ .tag = .identifier };
    const x = searchForNode(result.tree, isIdentifier);
    try t.expect(query.matches(result.tree, x.?));
}
