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
    pub const Id = enum(u32) { _ };

    /// Represents an empty document, never gets printed.
    nil,
    /// A '\n' character.
    nl,
    /// A single whitespace character
    space,
    /// Indent the inner document by the specified number of tabs.
    indent: struct { u16, Doc.Id },
    /// A group of two or more documents
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
    cons: struct { Doc.Id, Doc.Id },
    /// raw source text without any newline characters
    text: []const u8,
    /// Represents two possible layouts.
    /// If the first layout in the pair doesn't fit, the second is chosen.
    /// NOTE:
    /// 1. The LHS must not have any `nl`s
    /// 2. None of the first lines in the left doc should be longer
    ///    than any of the first lines in the right doc.
    either: struct { Doc.Id, Doc.Id },
    /// Print this sub-layout without any newlines.
    /// Any `nl`s that are children of this flat node will become spaces
    flat: Doc.Id,
};

pub const nl = Doc{ .nl = {} };
pub const nil = Doc{ .nil = {} };
pub const spc = Doc{ .space = {} };

pub fn indent(width: u16, doc: Doc.Id) Doc {
    return Doc{ .indent = .{ width, doc } };
}

pub fn cons(left: Doc.Id, right: Doc.Id) Doc {
    return Doc{ .cons = .{ left, right } };
}

pub fn either(left: Doc.Id, right: Doc.Id) Doc {
    return Doc{ .either = .{ left, right } };
}

fn flatten(doc: Doc.Id) Doc {
    switch (doc.*) {
        .nil => nil,
        .nl, .space => spc,
        .either => |pair| flatten(pair[0]),
        .indent => |d| flatten(d),
        .cons => |pair| cons(flatten(pair[0]), flatten(pair[1])),
    }
}

/// A simpler representation of `Doc`, without any `choice`s.
/// A layout algorithm should convert a list of `Doc`s
/// to a list of `SimpleDoc`s, which can then be rendered.
const SimpleDoc = union(enum) {
    /// Empty document. Not printed in rendered output.
    nil,
    /// raw string
    text: []const u8,
    /// A single whitespace character
    space,
    /// Stores the number of tabs to place after the '\n' character
    line: u32,
    /// Convert this simple document to a text format, then append it to the
    /// given array-list.
    pub fn write(self: SimpleDoc, allocator: Allocator, out: *std.ArrayList(u8)) Allocator.Error!void {
        switch (self) {
            .nil => {},
            .space => try out.append(allocator, ' '),
            .line => |n| {
                try out.ensureUnusedCapacity(allocator, n + 1);
                try out.appendAssumeCapacity('\n');
                for (0..n) |_| try out.appendAssumeCapacity(' ');
            },
            .text => |txt| try out.appendSlice(allocator, txt),
        }
    }
};

/// When printed as a string, can `doc` fit within `max_width`
/// characters?
pub fn fits(max_width: u32, doc: Doc) bool {
    return fitsImpl(max_width, doc) >= 0;
}

fn fitsImpl(_: u32, _: Doc) u32 {
    return 1;
}

pub const LayoutBuilder = struct {
    const Self = @This();
    /// Maximum number of characters allowed on a single line
    max_width: u32,
    /// List of documents to render
    docs: std.ArrayList(Doc),

    /// A document with a specific level of indentation
    const IndentedDoc = struct {
        /// Indentation level
        indent_level: u32,
        /// Whether to separate sub-documents with a new-line
        /// or a whitespace
        mode: enum { brk, flatten },
        /// The actual document for which to check the fitting
        doc: Doc,
        /// Return a new doc that is indented by [n] tabs
        pub fn indent(self: *const IndentedDoc, n: u32) IndentedDoc {
            return .{
                .indent_level = self.indent_level + n,
                .doc = self.doc,
                .mode = self.mode,
            };
        }
    };
};
