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

/// A parsed query on a JS/TS/JSX syntax tree.
pub const Query = union(enum) {
    /// Used to uniquely identify a query in list of queries
    pub const Index = enum(u32) { _ };
    /// Matches if a node matches both sub-queries
    compound: BinaryOpPayload,
    /// Matches a node with a specific tag.
    tag: std.meta.Tag(js.ast.NodeData),
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
