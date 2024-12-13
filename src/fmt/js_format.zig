// Generates Jam fmt IR from JS/JSX/TS source code.
const std = @import("std");
const ir = @import("./ir.zig");

const Doc = ir.Doc;

const Allocator = std.mem.Allocator;

const Self = @This();

allocator: Allocator,
docs: std.ArrayList(Doc),

pub fn init(allocator: Allocator) Allocator.Error!Self {
    return Self{
        .allocator = allocator,
        .docs = try std.ArrayList(Doc).initCapacity(allocator, 256),
    };
}

pub fn format() []const u8 {
    return "";
}

pub fn deinit(self: *Self) void {
    self.docs.deinit();
}

test {}
