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

pub const Block = struct {
    rules: []Node.Index,
};

pub const Selector = struct {
    name_token: Token.Index,
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
    block: Block,
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
