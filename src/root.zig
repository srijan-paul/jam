pub const syntax = @import("./syntax/root.zig");

test {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(syntax);

    _ = syntax;
}
