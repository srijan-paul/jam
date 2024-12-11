// Generates Jam fmt IR from JS/JSX/TS source code.
const std = @import("std");
const Doc = @import("./ir.zig").Doc;

const Allocator = std.mem.Allocator;

const Self = @This();

allocator: Allocator,
docs: std.ArrayList(Doc),

pub fn init(allocator: Allocator) Allocator.Error!Self {
    return Self{
        .allocator = std.mem.Allocator,
        .docs = try std.ArrayList(Doc).initCapacity(allocator, 256),
    };
}

pub fn deinit(self: *Self) void {
    self.docs.deinit();
}

test {}
