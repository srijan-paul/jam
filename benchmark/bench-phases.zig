// Phase-split benchmark: tokenizer-only vs full parse on typescript.js.
// Helps localize where parse time is spent.

const std = @import("std");
const js = @import("js");

const Tokenizer = js.Tokenizer;
const Token = js.Token.Token;

const parse_runs: usize = 20;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();
    const al = arena.allocator();

    const cwd = std.Io.Dir.cwd();
    const source = try cwd.readFileAlloc(
        io,
        try std.Io.Dir.path.join(al, &.{ "benchmark", "typescript.js" }),
        al,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    );

    // Phase 1: tokenizer only.
    // NOTE: standalone tokenization desyncs where the parser would normally
    // drive rescans (regex vs division, template `}` handling). On error we
    // just skip a byte and keep going, so coverage stays ~100%.
    var total_tokens: usize = 0;
    var skipped_bytes: usize = 0;
    const tok_start = std.Io.Clock.awake.now(io);
    for (0..parse_runs) |_| {
        var tokenizer = try Tokenizer.init(source, .{});
        var n: usize = 0;
        var max_len: u32 = 0;
        var max_tag: []const u8 = "";
        while (true) {
            const token = tokenizer.next() catch {
                if (tokenizer.index >= source.len) break;
                skipped_bytes += 1;
                tokenizer.index += 1;
                continue;
            };
            if (token.tag == .eof) break;
            if (token.len > max_len) {
                max_len = token.len;
                max_tag = @tagName(token.tag);
            }
            n += 1;
        }
        total_tokens += n;
        if (max_len > 4096)
            std.debug.print("run: giant token: {s} of {d} bytes at {d}\n", .{ max_tag, max_len, @as(u32, 0) });
    }
    const tok_end = std.Io.Clock.awake.now(io);

    // Phase 2: full parse.
    // NOTE: arena is retained across runs (reset with retain_capacity) to
    // exclude page-fault costs and measure pure parse work.
    var parse_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer parse_arena.deinit();
    var stats = struct {
        nodes: usize = 0,
        tokens: usize = 0,
        extras: usize = 0,
        node_indices: usize = 0,
    }{};
    const parse_start = std.Io.Clock.awake.now(io);
    for (0..parse_runs) |_| {
        _ = parse_arena.reset(.retain_capacity);
        var parser = try js.Parser.init(parse_arena.allocator(), source, .{});
        defer parser.deinit();
        var result = try parser.parse();
        defer result.deinit();
        if (stats.nodes == 0) {
            stats = .{
                .nodes = result.tree.nodes.len,
                .tokens = result.tree.tokens.items.len,
                .extras = result.tree.extras.items.len,
                .node_indices = result.tree.node_indices.items.len,
            };
        }
    }
    const parse_end = std.Io.Clock.awake.now(io);

    const phase1_ms = nsToMs(tok_end.toNanoseconds() - tok_start.toNanoseconds());
    const parse_ms = nsToMs(parse_end.toNanoseconds() - parse_start.toNanoseconds());
    const out = try al.print(
        \\tokens/run:      {d} (skipped {d} bytes)
        \\tokenizer only:  {d:.2} ms
        \\full parse:      {d:.2} ms
        \\parser overhead: {d:.2} ms ({d:.0}% of parse)
        \\
    , .{ total_tokens / parse_runs, skipped_bytes / parse_runs, phase1_ms, parse_ms, parse_ms - phase1_ms, (parse_ms - phase1_ms) / parse_ms * 100 });
    _ = try std.Io.File.stdout().writeStreamingAll(io, out);
}

fn nsToMs(ns: i96) f64 {
    return @as(f64, @floatFromInt(ns)) / std.time.ns_per_ms / @as(f64, @floatFromInt(parse_runs));
}
