const std = @import("std");

pub fn build(b: *std.Build) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // dependencies
    const Dep = struct {
        name: []const u8,
        module: *std.Build.Module,
    };
    var deps = std.ArrayList(Dep).init(allocator);
    defer deps.deinit();

    const unicode_id = b.dependency("unicode_id", .{});
    const unicode_id_dep = Dep{
        .name = "unicode-id",
        .module = unicode_id.module("unicode-id"),
    };
    try deps.appendSlice(&.{unicode_id_dep});

    const util_module = b.addModule("util", .{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const jam_syntax_module = b.addModule("jam_syntax", .{
        .root_source_file = b.path("src/syntax/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    jam_syntax_module.addImport("util", util_module);

    const jam_js_module = b.addModule("js", .{
        .root_source_file = b.path("src/js/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    jam_js_module.addImport("util", util_module);
    jam_js_module.addImport("syntax", jam_syntax_module);

    {
        // wasm
        const wasm_target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
            .cpu_features_add = std.Target.wasm.featureSet(&.{
                .atomics,
                .bulk_memory,
                // .extended_const, not supported by Safari
                .multivalue,
                .mutable_globals,
                .nontrapping_fptoint,
                .reference_types,
                //.relaxed_simd, not supported by Firefox or Safari
                .sign_ext,
                // observed to cause Error occured during wast conversion :
                // Unknown operator: 0xfd058 in Firefox 117
                .simd128,
                // .tail_call, not supported by Safari
            }),
        });

        const js_wasm = b.addExecutable(.{
            .name = "jam_js",
            .root_source_file = b.path("src/js/lib_js_wasm.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        });

        js_wasm.entry = .disabled;
        js_wasm.rdynamic = true;
        js_wasm.root_module.addImport("util", util_module);
        js_wasm.root_module.addImport("syntax", jam_syntax_module);
        js_wasm.root_module.addImport(unicode_id_dep.name, unicode_id_dep.module);

        b.installArtifact(js_wasm);
        const js_wasm_file = b.addInstallFile(js_wasm.getEmittedBin(), js_wasm.out_filename);
        b.getInstallStep().dependOn(&js_wasm_file.step);
    }

    const jam_css_module = b.addModule("jam-css", .{
        .root_source_file = b.path("src/css/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    jam_css_module.addImport("util", util_module);
    jam_css_module.addImport("syntax", jam_syntax_module);

    const jam_fmt_module = b.addModule("jam-fmt", .{
        .root_source_file = b.path("src/fmt/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    jam_fmt_module.addImport("util", util_module);
    jam_fmt_module.addImport("js", jam_js_module);

    const lib = b.addStaticLibrary(.{
        .name = "jam",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "jam",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_check = b.addExecutable(.{
        .name = "jam_check",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_check.root_module.addImport("js", jam_js_module);
    exe_check.root_module.addImport("css", jam_css_module);
    exe_check.root_module.addImport("syntax", jam_syntax_module);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    b.installArtifact(exe);

    // Command to run the tc39/test-262-parser-tests test suite
    {
        const test262 = b.addExecutable(.{
            .name = "test262",
            .root_source_file = b.path("tools/ec262-tests.zig"),
            .target = target,
            .optimize = optimize,
        });

        test262.root_module.addImport("js", jam_js_module);
        const tests_262_cmd = b.addRunArtifact(test262);

        const test_runner_step = b.step("test-262", "Run the @babel/parser test suite");
        test_runner_step.dependOn(&tests_262_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            tests_262_cmd.addArgs(args);
        }
    }

    // Command to run the babel test suite
    {
        const test_babel = b.addExecutable(.{
            .name = "test-babel",
            .root_source_file = b.path("tools/babel-tests.zig"),
            .target = target,
            .optimize = optimize,
        });

        test_babel.root_module.addImport("js", jam_js_module);
        const test_babel_cmd = b.addRunArtifact(test_babel);

        const test_runner_step = b.step("test-babel", "Run the EC262 parser tests");
        test_runner_step.dependOn(&test_babel_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            test_babel_cmd.addArgs(args);
        }
    }

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
    lib_unit_tests.root_module.addImport("syntax", jam_syntax_module);
    lib_unit_tests.root_module.addImport("js", jam_js_module);

    const js_unit_tests = b.addTest(.{
        .root_source_file = b.path("./src/js/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    js_unit_tests.root_module.addImport("util", util_module);
    js_unit_tests.root_module.addImport("syntax", jam_syntax_module);

    const scope_unit_tests = b.addTest(.{
        .root_source_file = b.path("./src/js/scope.zig"),
        .target = target,
        .optimize = optimize,
    });

    scope_unit_tests.root_module.addImport("util", util_module);
    scope_unit_tests.root_module.addImport("syntax", jam_syntax_module);

    const util_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const run_js_unit_tests = b.addRunArtifact(js_unit_tests);
    const run_util_unit_tests = b.addRunArtifact(util_unit_tests);
    const run_scope_unit_tests = b.addRunArtifact(scope_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_js_unit_tests.step);
    test_step.dependOn(&run_util_unit_tests.step);
    test_step.dependOn(&run_scope_unit_tests.step);

    exe.root_module.addImport("fmt", jam_fmt_module);
    exe.root_module.addImport("js", jam_js_module);
    exe.root_module.addImport("css", jam_css_module);
    exe.root_module.addImport("syntax", jam_syntax_module);

    for (deps.items) |dep| {
        jam_js_module.addImport(dep.name, dep.module);
        jam_css_module.addImport(dep.name, dep.module);

        lib.root_module.addImport(dep.name, dep.module);
        lib_unit_tests.root_module.addImport(dep.name, dep.module);
        js_unit_tests.root_module.addImport(dep.name, dep.module);

        exe.root_module.addImport(dep.name, dep.module);
        exe_check.root_module.addImport(dep.name, dep.module);
    }
}
