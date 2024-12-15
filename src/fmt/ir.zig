// A language agnostic formatter IR.
// This is inspired by Philip Wadler's Haskell printer:
// https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf,
// and Christian Lindig's strict implementation of the wadler printer in OCaml:
// https://lindig.github.io/papers/strictly-pretty-2000.pdf
// Of course, changes had to made for this to work for JavaScript formatter.
// The code should still be relatively straightforward to follow.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// The "Intermediate Representation" for the formatter.
/// All source text must be converted to a slice of documents
/// before it can rendered.
pub const Doc = union(enum) {
    /// A unique ID for a document in a list of docs.
    pub const Id = enum(u32) { _ };

    /// Represents an empty document, never gets printed.
    nil,
    /// A '\n' character.
    nl,
    /// Indent the inner document by the specified number of tabs.
    indent: struct { u16, Doc.Id },
    /// A list of one or more documents
    /// The first item in the tuple is the head of the list,
    /// and the second item is the "tail".
    /// The end of the list is denoted by a any document type
    /// that isn't another `list`.
    ///
    /// E.g, `"foo" -> "bar" -> nil` is represented as:
    /// ```zig
    /// // (cons "foo" (cons "bar" nil))
    /// Doc{
    ///   .cons = .{
    ///     Doc{ .text = "foo" },
    ///     Doc{
    ///       .cons = .{
    ///         Doc{ .text = "bar" },
    ///         Doc{ .nil  = {}    },
    ///       },
    ///    },
    ///   },
    /// }
    /// ```
    ///
    list: struct { Doc.Id, Doc.Id },
    /// raw source text without any newline characters
    text: []const u8,
    /// Chooses between two layouts, whichever is best
    /// The LHS must not have any `nl`s
    choice: struct { Doc.Id, Doc.Id },
    /// Print this sub-layout without any newlines.
    /// Any `nl`s that are children of this flat node will become spaces
    flat: Doc.Id,
};

pub fn indent(width: u16, doc: Doc.Id) Doc {
    return Doc{ .indent = .{ width, doc } };
}

pub fn cons(left: Doc.Id, right: Doc.Id) Doc {
    return Doc{ .list = .{ left, right } };
}

pub fn flat(doc: Doc.Id) Doc {
    return Doc{ .flat = doc };
}

/// A simpler representation of `Doc`, without any `choice`s.
/// A layout algorithm should convert a list of `Doc`s
/// to a list of `SimpleDoc`s, which can then be rendered.
const SimpleDoc = union(enum) {
    /// Empty document. Not printed in rendered output.
    nil,
    /// raw string
    text: []const u8,
    /// Stores the number of tabs to place after the '\n' character
    line: u32,
};

pub const LayoutBuilder = struct {
    const Self = @This();

    /// Maximum number of characters allowed on a single line
    max_width: u32,
    /// List of documents to render
    docs: std.ArrayList(Doc),

    const State = struct {
        indent_level: u32,
        mode: enum { brk, flatten },
    };

    fn fits(_: *Self, doc: *const Doc, _: State) bool {
        switch (doc.*) {
            .nil => true,
        }
        return false;
    }
};
