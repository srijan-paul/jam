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
    /// If parsing fails, an error message might be stored here.
    error_message: ?[]const u8 = null,

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

    pub fn emitError(self: *State, message: []const u8, err: Error) Error {
        self.error_message = message;
        return err;
    }
};

pub const Error = error{
    UnexpectedChar,
    UnexpectedEof,
    OutOfMemory,
    InvalidCharacter,
    InvalidTrailingComma,
    UnknownTagName,
};

pub const ParsedQuery = struct {
    allocator: std.mem.Allocator,
    queries: []Query,
    root_node_id: Query.Index,

    pub fn deinit(self: *ParsedQuery) void {
        self.allocator.free(self.queries);
    }

    pub inline fn get(self: *const ParsedQuery, id: Query.Index) *const Query {
        return &self.queries[@intFromEnum(id)];
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    error_message: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator) Parser {
        return Parser{ .allocator = allocator };
    }

    /// Parse a comptime query string into a runtime `ParsedQuery` structure.
    pub fn parse(self: *Parser, comptime query_str: []const u8) Error!ParsedQuery {
        // TODO: come up with a routine to more reliably predict
        // how many nodes there will be in a parsed query.
        // That way, we don't eat up too much memory even at compile time.
        // (Though this should honestly be fine considering there's no runtime allocation here)
        const maybe_root_id, const queries, const maybe_err_message = comptime blk: {
            var queries = [_]Query{undefined} ** query_str.len;
            var state = State{ .source = query_str, .index = 0, .queries = &queries };
            const root_id = parseQuery(&state);
            break :blk .{ root_id, queries[0..state.n_queries], state.error_message };
        };

        self.error_message = maybe_err_message;
        const root_id = try maybe_root_id;

        // This .* is necessary, and the only way to
        // copy a comptime array into a stack allocated runtime array
        // TODO: can the comptime slice be used directly somehow?
        const ret_queries = queries.*;
        return ParsedQuery{
            .allocator = self.allocator,
            .queries = try self.allocator.dupe(Query, &ret_queries),
            .root_node_id = root_id,
        };
    }
};

/// checks if a character is the valid start char for a query
fn isValidQueryStart(ch: u8) bool {
    return switch (ch) {
        '{',
        '0'...'9',
        'a'...'z',
        'A'...'Z',
        => true,
        else => false,
    };
}

fn parseQuery(s: *State) Error!Query.Index {
    skipSpaces(s);
    var lhs = try parseAtom(s);
    while (s.peek()) |ch| {
        if (!std.ascii.isWhitespace(ch)) break;
        skipSpaces(s);

        if (s.peek()) |next_ch| {
            if (!isValidQueryStart(next_ch))
                break;
        } else {
            break; // eof
        }

        const rhs = try parseAtom(s);
        lhs = try s.addQuery(Query{
            .compound = .{ .left = lhs, .right = rhs },
        });
    }

    return lhs;
}

fn parseAtom(s: *State) Error!Query.Index {
    const ch = s.peek() orelse
        return Error.UnexpectedEof;

    return switch (ch) {
        '{' => parseAttributeList(s),
        '0'...'9' => parseNumericLiteral(s),
        'a'...'z', 'A'...'Z' => try parseTag(s),
        else => {
            const message = if (@inComptime())
                "Unexpected character ('" ++ &[_]u8{ch} ++ "'), expecting the start of a query"
            else
                // At runtime, we do not have access to an allocator (we could, though. TODO?)
                "Unexpected character, expecting the start of a query";

            return s.emitError(message, Error.InvalidCharacter);
        },
    };
}

const ast = @import("js").ast;
const valid_tags = std.meta.tags(std.meta.Tag(ast.NodeData));
fn parseTag(s: *State) Error!Query.Index {
    std.debug.assert(std.ascii.isAlphabetic(s.peekUnchecked()));

    const start = s.index;
    while (s.peek()) |ch| : (s.bump()) {
        if (!(std.ascii.isAlphabetic(ch) or ch == '_')) break;
    }

    const tag_name = s.source[start..s.index];
    for (valid_tags) |node_tag| {
        if (std.mem.eql(u8, tag_name, @tagName(node_tag))) {
            return s.addQuery(Query{ .tag = node_tag });
        }
    }

    if (@inComptime())
        return s.emitError("Unknown tag name: " ++ tag_name, Error.UnknownTagName);

    return s.emitError("Unknown tag name", Error.UnknownTagName);
}

/// When on a '{', parse a list of comma separate attributes,
/// including the closing '}'.
fn parseAttributeList(s: *State) Error!Query.Index {
    std.debug.assert(s.peek() == '{');
    s.bump(); // eat '{'
    skipSpaces(s);

    // count ","s to find out how many attributes there are
    // in this attribute list
    const attrs_count = countAttribtues(s.source[s.index..]) orelse
        return Error.UnexpectedChar; // TODO: better error

    if (s.peek() == '}') {
        // empty attribute list
        std.debug.assert(attrs_count == 0);
        s.bump();
        return s.addQuery(Query{ .attribute_list = .{ .attrs_start = 0, .attrs_end = 0 } });
    }

    const attr_start_index: usize = s.n_queries + 1;
    const attr_end_index = attr_start_index + attrs_count;
    const attr_list_id = try s.addQuery(Query{
        .attribute_list = .{
            .attrs_start = attr_start_index,
            .attrs_end = attr_end_index,
        },
    });

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
            if (parsed_attrs_count == attrs_count) {
                return s.emitError(
                    "A trailing ',' is not allowed in an attribute list",
                    Error.InvalidTrailingComma,
                );
            }

            s.bump(); // eat ','
            skipSpaces(s);
        }
    } else {
        return s.emitError("Missing closing '}' in attribute list", Error.UnexpectedEof);
    }

    return attr_list_id;
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
        const maybe_closing_ch: ?u8 = switch (ch) {
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
    const value = try parseQuery(s);
    skipSpaces(s);

    return Query{
        .attribute = .{
            .name = try s.addQuery(attr_name),
            .value = value,
        },
    };
}

fn parseAttributeName(s: *State) Error!Query {
    const start = s.index;
    const is_valid_start = std.ascii.isAlphabetic(s.peekUnchecked());
    if (!is_valid_start) {
        const error_message = "Expected attribute name or ':'";
        return s.emitError(if (@inComptime())
            error_message ++ " but found a " ++ s.source[s.index .. s.index + 1]
        else
            error_message, Error.UnexpectedChar);
    }

    while (s.peek()) |ch| : (s.bump()) {
        if (!(std.ascii.isAlphanumeric(ch) or ch == '_'))
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

fn parse(comptime query_string: []const u8) Error!ParsedQuery {
    var p = Parser.init(t.allocator);
    return p.parse(query_string) catch |err| {
        if (p.error_message) |message| {
            std.debug.print("parse error: {s}\n", .{message});
        }

        std.debug.print("parse error: {any}\n", .{err});
        return err;
    };
}

test Parser {
    {
        var p = try parse("{value:123}");
        defer p.deinit();

        const attr_list = p.get(p.root_node_id);
        try t.expectEqual(.attribute_list, std.meta.activeTag(attr_list.*));

        const attrs = attr_list.attribute_list.getAttributes(&p);
        try t.expectEqual(123.0, p.get(attrs[0].attribute.value).numeric_literal);
        try t.expectEqualStrings("value", p.get(attrs[0].attribute.name).attr_name);
    }

    {
        var p = try parse("{}");
        defer p.deinit();

        const attr_list = p.get(p.root_node_id);
        try t.expectEqual(.attribute_list, std.meta.activeTag(attr_list.*));

        const attrs = attr_list.attribute_list.getAttributes(&p);
        try t.expectEqual(0, attrs.len);
    }

    {
        var p = try parse("{a:1, b:2}");
        defer p.deinit();

        const attr_list = p.get(p.root_node_id);
        try t.expectEqual(.attribute_list, std.meta.activeTag(attr_list.*));

        const attrs = attr_list.attribute_list.getAttributes(&p);
        try t.expectEqual(2, attrs.len);

        try t.expectEqual(1.0, p.get(attrs[0].attribute.value).numeric_literal);
        try t.expectEqualStrings("a", p.get(attrs[0].attribute.name).attr_name);

        try t.expectEqual(2.0, p.get(attrs[1].attribute.value).numeric_literal);
        try t.expectEqualStrings("b", p.get(attrs[1].attribute.name).attr_name);
    }

    {
        var p = try parse("{a:{nested_a: 1, nested_b: 3}, b:2}");
        defer p.deinit();

        const attr_list = p.get(p.root_node_id);
        try t.expectEqual(.attribute_list, std.meta.activeTag(attr_list.*));

        const attrs = attr_list.attribute_list.getAttributes(&p);
        try t.expectEqual(2, attrs.len);

        const nested_attr_list = p.get(attrs[0].attribute.value).*;
        try t.expectEqual(.attribute_list, std.meta.activeTag(nested_attr_list));

        const nested_attrs = nested_attr_list.attribute_list.getAttributes(&p);
        try t.expectEqual(2, nested_attrs.len);

        try t.expectEqual(1.0, p.get(nested_attrs[0].attribute.value).numeric_literal);
        try t.expectEqualStrings("nested_a", p.get(nested_attrs[0].attribute.name).attr_name);

        try t.expectEqual(3.0, p.get(nested_attrs[1].attribute.value).numeric_literal);
        try t.expectEqualStrings("nested_b", p.get(nested_attrs[1].attribute.name).attr_name);

        try t.expectEqual(2.0, p.get(attrs[1].attribute.value).numeric_literal);
        try t.expectEqualStrings("b", p.get(attrs[1].attribute.name).attr_name);
    }

    {
        var p = try parse("binary_expr { left: 1, right: 2 }");
        defer p.deinit();

        const compound_query = p.get(p.root_node_id).*;

        try t.expectEqual(.compound, std.meta.activeTag(compound_query));

        // binary_expr
        const left = p.get(compound_query.compound.left).*;
        try t.expectEqual(.binary_expr, left.tag);

        // { left: 1, right: 2}
        const right = p.get(compound_query.compound.right).*;
        try t.expectEqual(.attribute_list, std.meta.activeTag(right));

        const attrs = right.attribute_list.getAttributes(&p);
        try t.expectEqual(2, attrs.len);
        try t.expectEqual(1.0, p.get(attrs[0].attribute.value).numeric_literal);
        try t.expectEqualStrings("left", p.get(attrs[0].attribute.name).attr_name);
        try t.expectEqual(2.0, p.get(attrs[1].attribute.value).numeric_literal);
        try t.expectEqualStrings("right", p.get(attrs[1].attribute.name).attr_name);
    }
}
