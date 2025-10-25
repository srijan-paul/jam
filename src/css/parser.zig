const std = @import("std");
const ast = @import("./ast.zig");
const util = @import("util");
const block = @import("./block.zig");
const parseRule = @import("./rule.zig").parseRule;

const Tokenizer = @import("./tokenizer.zig");

const DiagnosticsBuilder = util.DiagnosticsBuilder;
const Diagnostic = DiagnosticsBuilder.Diagnostic;

pub const Error = Tokenizer.Error || error{UnexpectedEof};
pub const Token = Tokenizer.Token;

const Self = @This();

arena: *std.heap.ArenaAllocator,
allocator: std.mem.Allocator,
/// Immutable reference to the source code.
source: []const u8,
/// Most recently consumed token.
/// Tokens are consumed on-demand.
current_token: Token = undefined, // initialized by `init`
/// Produces tokens from source string.
/// See: src/css/tokenizer.zig
tokenizer: Tokenizer,
/// Stores errors reported during parsing.
diagnostics: DiagnosticsBuilder,
/// AST nodes are stored in this flat list and reference each other by index.
nodes: std.ArrayList(ast.Node),
node_refs: std.ArrayList(ast.Node.Index),
/// A flat list of tokens that are referenced in the AST.
tokens: std.ArrayList(Token),

pub fn init(arena: *std.heap.ArenaAllocator, source: []const u8) Error!Self {
    const al = arena.allocator();

    var tokenizer = try Tokenizer.init(al, source);
    var first_token = try tokenizer.next();
    while (first_token.tag() == .comment) : (first_token = try tokenizer.next()) {
        // TODO: store leading comments as trivia.
    }

    var nodes: std.ArrayList(ast.Node) = .{};
    try nodes.ensureTotalCapacity(al, 128);
    var node_refs: std.ArrayList(ast.Node.Index) = .{};
    try node_refs.ensureTotalCapacity(al, 8);
    var tokens: std.ArrayList(Token) = .{};
    try tokens.ensureTotalCapacity(al, 128);

    return Self{
        .arena = arena,
        .allocator = al,
        .source = source,
        .tokenizer = tokenizer,
        .diagnostics = DiagnosticsBuilder.init(al),
        .nodes = nodes,
        .node_refs = node_refs,
        .tokens = tokens,
        .current_token = first_token,
    };
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit(self.allocator);
    self.diagnostics.deinit();

    self.arena.deinit();
}

pub fn parse(self: *Self) Error!ast.Node.Index {
    var rules: std.ArrayList(ast.Node.Index) = .{};
    try rules.ensureTotalCapacity(self.allocator, 8);
    while (self.current_token.tag() != .eof) {
        const rule_index = try parseRule(self);
        try rules.append(self.allocator, rule_index);
    }

    const rule_nodes = try self.newSubRange(rules.items);
    return self.addNode(
        ast.NodeData{ .stylesheet = rule_nodes },
        0,
        @intCast(self.source.len),
    );
}

pub fn getNode(self: *const Self, index: ast.Node.Index) ast.Node {
    return self.nodes.items[@intFromEnum(index)];
}

/// Add a new node to the list of nodes and return its index.
pub fn addNode(
    self: *Self,
    node: ast.NodeData,
    start: u32,
    end: u32,
) std.mem.Allocator.Error!ast.Node.Index {
    try self.nodes.append(self.allocator, .{
        .data = node,
        .start = start,
        .end = end,
    });

    const len: u32 = @intCast(self.nodes.items.len - 1);
    return @enumFromInt(len);
}

/// Advance in the token stream, save the next token to the list of tokens and return its index.
pub fn saveToken(self: *Self, token: Token) Error!Token.Index {
    const index = self.tokens.items.len;
    try self.tokens.append(self.allocator, token);
    return @enumFromInt(index);
}

/// Consume the next token from the lexer, skipping all comments.
pub fn nextToken(self: *Self) Error!Token {
    var next_token = try self.tokenizer.next();
    while (next_token.tag() == .comment) : (next_token = try self.tokenizer.next()) {
        // TODO: store comments as trivia.
    }

    const cur = self.current_token;
    self.current_token = next_token;

    if (cur.tag() == .eof) {
        try self.diagnostics.emit(
            cur.startCoord(self.source),
            "Unexpected end of file.",
            .{},
        );
        return Error.UnexpectedEof;
    }

    return cur;
}

/// Get a slice of nodes referenced by the given subrange.
/// a "SubRage" is a [from, to] index pair used to represent a contiguous range of nodes in the AST.
pub fn subRangeAsSlice(self: *const Self, range: ast.SubRange) []ast.Node.Index {
    return self.node_refs.items[range.start..range.end];
}

/// Get the token at the given index.
pub fn getToken(self: *const Self, t: Token.Index) Token {
    return self.tokens.items[@intFromEnum(t)];
}

// TODO: improve this doc comment.
/// Create a new subrange from the given nodes and append them to the list of node ranges.
pub fn newSubRange(
    self: *Self,
    nodes: []ast.Node.Index,
) std.mem.Allocator.Error!ast.SubRange {
    const from: u32 = @intCast(self.node_refs.items.len);
    try self.node_refs.appendSlice(self.allocator, nodes);
    const to: u32 = @intCast(self.node_refs.items.len);

    return ast.SubRange{ .start = from, .end = to };
}

/// Consume the next token from the lexer.
/// Emit a diagnostic and return an error if the next token doesn't match the expected tag
pub fn expect(self: *Self, expected: std.meta.Tag(Token.Data)) Error!Token {
    if (self.current_token.tag() != expected) {
        try self.diagnostics.emit(
            self.current_token.startCoord(self.source),
            "Expected {s}, got {s} instead.",
            .{ @tagName(expected), self.current_token.toByteSlice(self.source) },
        );
    }

    return self.nextToken();
}
