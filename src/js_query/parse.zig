const Query = @import("query.zig").Query;

const std = @import("std");

/// State of the parser.
/// This does not hold an allocator to allow
/// for compile-time parsing of queries.
const State = struct {
    /// The queries that have been parsed so far
    queries: []Query,
    /// The number of queries that have been parsed so far
    n_queries: usize = 0,
    /// The query string that is being parsed
    source: []const u8,
    /// The current byte position in the query string
    index: usize = 0,

    /// Returns the next character in the query string without consuming it.
    /// Returns `null` if the end of the query string has been reached.
    pub fn peek(self: *State) ?u8 {
        if (self.index >= self.source.len) return null;
        return self.source[self.index];
    }

    /// Returns the next character in the query string without consuming it.
    /// Does not do any bounds checking, the caller must ensure
    /// that the entire input hasn't been consumed yet.
    pub fn peekUnchecked(self: *State) u8 {
        return self.source[self.index];
    }

    /// Consumes the next character in the query string.
    pub fn bump(self: *State) void {
        self.index += 1;
    }

    /// Returns whether the end of the query string has been reached.
    pub fn eof(self: *State) bool {
        return self.index >= self.source.len;
    }

    /// Returns the next character in the query string and consumes it.
    /// Does not do any bounds checking, the caller must ensure
    /// that the entire input hasn't been consumed yet.
    pub fn nextUnchecked(self: *State) u8 {
        const ch = self.source[self.index];
        self.index += 1;
        return ch;
    }

    /// Returns the next character in the query string and consumes it.
    /// Returns an error if the end of the query string has been reached.
    pub fn next(self: *State) Error!u8 {
        if (self.index >= self.source.len)
            return Error.UnexpectedEof;
        return self.nextUnchecked();
    }

    /// Consumes the next character in the query string,
    /// and checks if it matches the expected character.
    pub fn expectChar(self: *State, expected: u8) Error!void {
        const ch = self.peek();
        if (ch != expected)
            return Error.UnexpectedChar;
        self.index += 1;
    }

    /// Insert a new sub-query into the list of sub-queries.
    pub fn addQuery(self: *State, query: Query) error{OutOfMemory}!Query.Index {
        if (self.n_queries >= self.queries.len)
            return Error.OutOfMemory;
        self.queries[self.n_queries] = query;
        self.n_queries += 1;
        return @enumFromInt(self.n_queries - 1);
    }

    /// Reserve space for `n` queries, and return the reserved slice.
    pub fn reserveQueries(self: *State, n: usize) error{OutOfMemory}![]Query {
        if (self.n_queries + n >= self.queries.len)
            return Error.OutOfMemory;
        const slice = self.queries[self.n_queries .. self.n_queries + n];
        self.n_queries += n;
        return slice;
    }
};

pub const Error = error{
    UnexpectedChar,
    UnexpectedEof,
    OutOfMemory,
    InvalidCharacter,
    InvalidTrailingComma,
};

pub const ParsedQuery = struct {
    allocator: std.mem.Allocator,
    queries: []Query,
    root_node_id: Query.Index,

    pub fn deinit(self: *ParsedQuery) void {
        self.allocator.free(self.queries);
    }

    pub inline fn get(self: *ParsedQuery, id: Query.Index) *Query {
        return &self.queries[@intFromEnum(id)];
    }
};

pub fn parse(al: std.mem.Allocator, comptime query_str: []const u8) Error!ParsedQuery {
    // TODO: come up with a routine to more reliably predict
    // how many nodes there will be in a parsed query.
    // That way, we don't eat up too much memory even at compile time.
    // (Though this should honestly be fine considering there's no runtime allocation here)
    const root_id, const queries = comptime blk: {
        var queries = [_]Query{undefined} ** query_str.len;
        var state = State{ .source = query_str, .index = 0, .queries = &queries };
        const root_id = parseImpl(&state);
        break :blk .{ root_id, queries[0..state.n_queries] };
    };

    // This .* is necessary, and the only way to
    // copy a comptime array into a stack allocated runtime array
    // TODO: can the comptime slice be used directly somehow?
    const ret_queries = queries.*;
    return ParsedQuery{
        .allocator = al,
        .queries = try al.dupe(Query, &ret_queries),
        .root_node_id = try root_id,
    };
}

fn parseImpl(s: *State) Error!Query.Index {
    const ch = s.peek() orelse
        return Error.UnexpectedEof;

    return switch (ch) {
        '{' => parseAttributeList(s),
        else => return Error.UnexpectedChar,
    };
}

fn parseAttributeList(s: *State) Error!Query.Index {
    std.debug.assert(s.peek() == '{');
    s.bump(); // eat '{'
    skipSpaces(s);

    // count ","s to find out how many attributes there are
    // in this attribute list
    const attrs_count = countAttribtues(s.source[s.index..]) orelse
        return Error.UnexpectedChar; // TODO: better error

    if (s.peek() == '}') {
        std.debug.assert(attrs_count == 0);
        s.bump();
        // empty attribute list
        return s.addQuery(Query{ .attribute_list = 0 });
    }

    const id = try s.addQuery(Query{ .attribute_list = attrs_count });

    // Reserve space for as many attributes as we counted,
    // then parse them.
    var attrs = try s.reserveQueries(attrs_count);
    var parsed_attrs_count: usize = 0;
    while (s.peek()) |ch| {
        if (ch == '}') {
            s.bump();
            break;
        }

        std.debug.assert(parsed_attrs_count < attrs_count);
        attrs[parsed_attrs_count] = try parseAttribute(s);

        parsed_attrs_count += 1;
        skipSpaces(s);

        if (s.peek() == ',') {
            // a ',' isn't allowed after the last attribute
            if (parsed_attrs_count == attrs_count)
                return Error.InvalidTrailingComma;

            s.bump(); // eat ','
            skipSpaces(s);
        }
    }

    return id;
}

fn countAttribtues(input: []const u8) ?usize {
    if (input[0] == '}') return 0;

    var n_attributes: usize = 1;

    var i: usize = 0;
    while (i < input.len and input[i] != '}') : (i += 1) {
        const ch = input[i];

        if (ch == ',') {
            n_attributes += 1;
            continue;
        }

        // skip commas inside nested { { } }, [ [ ] ] and " " pairs
        const maybe_closing_ch = switch (ch) {
            '{' => '}',
            '[' => ']',
            '"', '\'' => ch,
            else => null,
        };

        if (maybe_closing_ch) |close_ch| {
            // TODO: return an "Unmatched '%c'" error if the closing
            // character is not found
            i += std.mem.indexOfScalar(u8, input[i..], close_ch) orelse
                return null;
        }
    }

    return n_attributes;
}

fn parseAttribute(s: *State) Error!Query {
    const attr_name = try parseAttributeName(s);
    skipSpaces(s);

    if (s.peek() != ':') return attr_name;

    s.bump(); // eat ':'
    skipSpaces(s);
    const literal = try parseLiteral(s);
    skipSpaces(s);

    return Query{
        .attribute = .{
            .name = try s.addQuery(attr_name),
            .value = literal,
        },
    };
}

fn parseAttributeName(s: *State) Error!Query {
    const start = s.index;
    const is_valid_start = std.ascii.isAlphabetic(s.peekUnchecked());
    if (!is_valid_start) return Error.UnexpectedChar;

    while (s.peek()) |ch| : (s.bump()) {
        if (!(std.ascii.isAlphanumeric(ch) or ch == '_' or std.ascii.isDigit(ch)))
            break;
    }

    const name = s.source[start..s.index];
    return Query{ .attr_name = name };
}

fn skipSpaces(s: *State) void {
    while (s.peek()) |ch| : (s.bump()) {
        if (!std.ascii.isWhitespace(ch)) break;
    }
}

fn parseLiteral(s: *State) Error!Query.Index {
    const ch = s.peek() orelse return Error.UnexpectedEof;
    return switch (ch) {
        '0'...'9' => parseNumericLiteral(s),
        else => Error.InvalidCharacter,
    };
}

fn parseNumericLiteral(s: *State) Error!Query.Index {
    std.debug.assert(!s.eof() and std.ascii.isDigit(s.peekUnchecked()));

    const start = s.index;
    while (s.peek()) |ch| : (s.bump()) {
        if (!std.ascii.isDigit(ch)) break;
    }

    // eat the part after '.'
    if (s.peek() == '.') {
        s.bump();
        while (s.peek()) |ch| : (s.bump()) {
            if (!std.ascii.isDigit(ch)) break;
        }
    }

    const value = try std.fmt.parseFloat(f64, s.source[start..s.index]);
    return s.addQuery(Query{ .numeric_literal = value });
}

const t = std.testing;
test parse {
    var parsed = parse(t.allocator, "{ value: 123 }") catch |e| {
        std.debug.print("err>> {any}", .{e});
        return e;
    };
    defer parsed.deinit();

    const q = parsed.get(parsed.root_node_id);
    try t.expectEqual(.attribute_list, std.meta.activeTag(q.*));

    const attr = parsed.get(@enumFromInt(1)).attribute;
    try t.expectEqual(123.0, parsed.get(attr.value).numeric_literal);
    try t.expectEqualStrings("value", parsed.get(attr.name).attr_name);
}
