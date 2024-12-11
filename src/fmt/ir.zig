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
    /// An empty document. Corresponds to an empty
    /// string in the printed file.
    nil,
    /// Either a space, or a newline character.
    /// By default, this is printed as a space.
    /// If printing as a space exceeds the column limit,
    /// this is printed as a newline instead.
    space_or_nl: u8,
    /// A '\n' character
    newline: u8,
    // TODO: make two groups: hard broken and soft broken
    group: []Doc,
    /// raw source text
    text: []const u8,
};

/// A lower level representation of `Doc` without any nesting.
/// All whitespaces and newlines are made explicit, and no "dual" states
/// like "space_or_nl" are present.
const SimpleDoc = union(enum) {
    nil,
    text: []const u8,
    /// stores the number of line breaks to place after the newline
    line_break: u32,

    /// Write the characters represented by this document to the given array-list.
    pub fn render(self: *const SimpleDoc, out: *std.ArrayList(u8)) Allocator.Error!void {
        switch (self.*) {
            .nil => return,
            .text => |s| try out.appendSlice(s),
            .line_break => |n| {
                try out.append('\n');
                for (0..n) |_| {
                    try out.append(' ');
                }
            },
        }
    }
};

fn renderSimpleDocuments(
    allocator: Allocator,
    sdocs: []const SimpleDoc,
) Allocator.Error![]const u8 {
    var out = try std.ArrayList(u8).initCapacity(allocator, sdocs.len);
    defer out.deinit();

    for (sdocs) |doc| try doc.render(&out);

    return out.toOwnedSlice();
}

const t = std.testing;

fn testRender(docs: []const SimpleDoc, expected: []const u8) !void {
    const out = try renderSimpleDocuments(t.allocator, docs);
    defer t.allocator.free(out);
    try t.expectEqualStrings(expected, out);
}

test renderSimpleDocuments {
    try testRender(&[_]SimpleDoc{.{ .nil = {} }}, "");
    try testRender(&[_]SimpleDoc{.{ .text = "hello, world!" }}, "hello, world!");
    try testRender(
        &[_]SimpleDoc{
            .{ .text = "a" },
            .{ .line_break = 3 },
            .{ .text = "b" },
        },
        "a\n   b",
    );
}
