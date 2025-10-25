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
    var deps: std.ArrayList(Dep) = .{};
    defer deps.deinit(allocator);

    const unicode_id = b.dependency("unicode_id", .{});
    const unicode_id_dep = Dep{
        .name = "unicode-id",
        .module = unicode_id.module("unicode-id"),
    };
    try deps.appendSlice(allocator, &.{unicode_id_dep});

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

        const js_wasm_module = b.createModule(.{
            .root_source_file = b.path("src/js/lib_js_wasm.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        });
        js_wasm_module.addImport("util", util_module);
        js_wasm_module.addImport("syntax", jam_syntax_module);
        js_wasm_module.addImport(unicode_id_dep.name, unicode_id_dep.module);

        const js_wasm = b.addExecutable(.{
            .name = "jam_js",
            .root_module = js_wasm_module,
        });

        js_wasm.entry = .disabled;
        js_wasm.rdynamic = true;

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

    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = "jam",
        .root_module = lib_module,
        .linkage = .static,
    });

    b.installArtifact(lib);

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "jam",
        .root_module = exe_module,
    });

    const exe_check_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_check_module.addImport("js", jam_js_module);
    exe_check_module.addImport("css", jam_css_module);
    exe_check_module.addImport("syntax", jam_syntax_module);

    const exe_check = b.addExecutable(.{
        .name = "jam_check",
        .root_module = exe_check_module,
    });

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    b.installArtifact(exe);

    // Command to run the tc39/test-262-parser-tests test suite
    {
        const test262_module = b.createModule(.{
            .root_source_file = b.path("tools/ec262-tests.zig"),
            .target = target,
            .optimize = optimize,
        });
        test262_module.addImport("js", jam_js_module);

        const test262 = b.addExecutable(.{
            .name = "test262",
            .root_module = test262_module,
        });

        const tests_262_cmd = b.addRunArtifact(test262);

        const test_runner_step = b.step("test-262", "Run the tc39/test-262-parser-tests test suite");
        test_runner_step.dependOn(&tests_262_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            tests_262_cmd.addArgs(args);
        }
    }

    // Command to run the @babel/parser test suite
    {
        const test_babel_module = b.createModule(.{
            .root_source_file = b.path("tools/babel-tests.zig"),
            .target = target,
            .optimize = optimize,
        });
        test_babel_module.addImport("js", jam_js_module);

        const test_babel = b.addExecutable(.{
            .name = "test-babel",
            .root_module = test_babel_module,
        });

        const test_babel_cmd = b.addRunArtifact(test_babel);

        const test_runner_step = b.step("test-babel", "Run the @babel/parser test suite");
        test_runner_step.dependOn(&test_babel_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            test_babel_cmd.addArgs(args);
        }
    }

    // Command to generate traversal code from the js AST definition
    {
        const ast_gen_module = b.createModule(.{
            .root_source_file = b.path("tools/gen/main.zig"),
            .target = target,
            .optimize = optimize,
        });

        const ast_gen = b.addExecutable(.{
            .name = "astgen",
            .root_module = ast_gen_module,
        });

        const ast_gen_cmd = b.addRunArtifact(ast_gen);
        const ast_gen_step = b.step("astgen", "Generate traversal code from the js AST definition");
        ast_gen_step.dependOn(&ast_gen_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            ast_gen_cmd.addArgs(args);
        }
    }

    // Command to benchmark the parser
    {
        const bench_parser_module = b.createModule(.{
            .root_source_file = b.path("benchmark/bench-parser.zig"),
            .target = target,
            .optimize = std.builtin.OptimizeMode.ReleaseFast,
        });
        bench_parser_module.addImport("js", jam_js_module);

        const bench_parser = b.addExecutable(.{
            .name = "bench-parser",
            .root_module = bench_parser_module,
        });

        const bench_cmd = b.addRunArtifact(bench_parser);
        const bench_step = b.step("bench-parser", "Run parser benchmarks");
        bench_step.dependOn(&bench_cmd.step);

        // forward all CLI arguments from build.zig to the test runner.
        if (b.args) |args| {
            bench_cmd.addArgs(args);
        }
    }

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests_module = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests_module.addImport("util", util_module);
    lib_unit_tests_module.addImport("syntax", jam_syntax_module);
    lib_unit_tests_module.addImport("js", jam_js_module);

    const lib_unit_tests = b.addTest(.{
        .root_module = lib_unit_tests_module,
    });

    const js_unit_tests_module = b.createModule(.{
        .root_source_file = b.path("./src/js/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    js_unit_tests_module.addImport("util", util_module);
    js_unit_tests_module.addImport("syntax", jam_syntax_module);

    const js_unit_tests = b.addTest(.{
        .root_module = js_unit_tests_module,
    });

    const util_unit_tests_module = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const util_unit_tests = b.addTest(.{
        .root_module = util_unit_tests_module,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const run_js_unit_tests = b.addRunArtifact(js_unit_tests);
    const run_util_unit_tests = b.addRunArtifact(util_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_js_unit_tests.step);
    test_step.dependOn(&run_util_unit_tests.step);

    exe_module.addImport("fmt", jam_fmt_module);
    exe_module.addImport("js", jam_js_module);
    exe_module.addImport("css", jam_css_module);
    exe_module.addImport("syntax", jam_syntax_module);

    for (deps.items) |dep| {
        jam_js_module.addImport(dep.name, dep.module);
        jam_css_module.addImport(dep.name, dep.module);

        lib_module.addImport(dep.name, dep.module);
        lib_unit_tests_module.addImport(dep.name, dep.module);
        js_unit_tests_module.addImport(dep.name, dep.module);

        exe_module.addImport(dep.name, dep.module);
        exe_check_module.addImport(dep.name, dep.module);
    }
}
