const std = @import("std");

pub fn build(b: *std.Build) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "jsickle",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    const util_module = b.addModule("util", .{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const jam_syntax_module = b.addModule("jam-syntax", .{
        .root_source_file = b.path("src/syntax/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    jam_syntax_module.addImport("util", util_module);

    const exe = b.addExecutable(.{
        .name = "jsickle",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);
    exe.root_module.addImport("util", util_module);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib_unit_tests.root_module.addImport("util", util_module);
    lib_unit_tests.root_module.addImport("jam-syntax", jam_syntax_module);

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // dependencies
    var deps = std.ArrayList(struct {
        name: []const u8,
        module: *std.Build.Module,
    }).init(allocator);
    defer deps.deinit();

    const unicode_id = b.dependency("unicode_id", .{});
    try deps.appendSlice(&.{
        .{
            .name = "unicode-id",
            .module = unicode_id.module("unicode-id"),
        },
    });

    exe.root_module.addImport("jam-syntax", jam_syntax_module);

    for (deps.items) |dep| {
        lib_unit_tests.root_module.addImport(dep.name, dep.module);
        lib.root_module.addImport(dep.name, dep.module);
        exe.root_module.addImport(dep.name, dep.module);
        jam_syntax_module.addImport(dep.name, dep.module);
    }
}
