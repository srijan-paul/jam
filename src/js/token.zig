const offsets = @import("util").offsets;
const types = @import("util").types;

pub const Coordinate = types.Coordinate;

pub const Token = struct {
    /// Index into the token array.
    /// Open ended enum for type safety.
    pub const Index = enum(u32) { _ };

    pub const Tag = enum(u32) {
        comment,

        identifier,
        private_identifier,
        numeric_literal,
        string_literal,

        @"\"",
        @"'",
        @"{",
        @"}",
        @"(",
        @")",
        @"[",
        @"]",
        @".",
        @"...",
        @";",
        @",",

        relational_start,
        @"<",
        @">",
        @"<=",
        @">=",
        kw_in,
        kw_instanceof,
        relational_end,

        eq_op_start,
        @"==",
        @"!=",
        @"===",
        @"!==",
        eq_op_end,

        additive_start,
        @"+",
        @"-",
        additive_end,

        multiplicative_start,
        @"*",
        @"/",
        @"%",
        multiplicative_end,

        @"**",
        @"++",
        @"--",

        shift_op_start,
        @"<<",
        @">>",
        @">>>",
        shift_op_end,

        @"&",
        @"|",
        @"^",
        @"!",
        @"~",
        @"&&",
        @"||",
        @"??",
        @"?.",
        @"?",
        @":",
        @"<<=",
        @">>=",

        // assignment operators
        assignment_op_start,
        @"=",
        @"%=",
        @"+=",
        @"*=",
        @"-=",
        @">>>=",
        @"&=",
        @"|=",
        @"^=",
        @"&&=",
        @"||=",
        @"/=",
        @"??=",
        @"**=",
        assignment_op_end,

        @"=>",

        keywords_start,

        kw_true,
        kw_false,
        kw_null,
        kw_var,
        kw_let,
        kw_const,
        kw_break,
        kw_case,
        kw_catch,
        kw_class,
        kw_continue,
        kw_debugger,
        kw_default,
        kw_do,
        kw_else,
        kw_export,
        kw_extends,
        kw_finally,
        kw_for,
        kw_function,
        kw_if,
        kw_import,
        kw_new,
        kw_return,
        kw_super,
        kw_switch,
        kw_this,
        kw_throw,
        kw_try,
        kw_while,
        kw_with,
        kw_typeof,
        kw_void,
        kw_delete,

        // strict mode keywords
        strict_mode_kw_start,

        kw_implements,
        kw_interface,
        kw_package,
        kw_private,
        kw_protected,
        kw_public,
        kw_static,
        kw_yield,

        strict_mode_kw_end,

        // contextual keywords.
        contextual_keywords_start,
        kw_as,
        kw_await,
        kw_async,
        kw_constructor,
        kw_get,
        kw_set,
        kw_from,
        kw_of,
        kw_enum,
        contextual_keywords_end,

        keywords_end,

        eof,

        pub fn isContextualKeyword(self: Tag) bool {
            const tag_u32: u32 = @intFromEnum(self);
            return tag_u32 > @intFromEnum(Tag.contextual_keywords_start) and
                tag_u32 < @intFromEnum(Tag.contextual_keywords_end);
        }

        pub fn isStrictModeKeyword(self: Tag) bool {
            const tag_u32: u32 = @intFromEnum(self);
            return tag_u32 > @intFromEnum(Tag.strict_mode_kw_start) and
                tag_u32 < @intFromEnum(Tag.strict_mode_kw_end);
        }

        pub fn isKeyword(self: Tag) bool {
            const tag_u32: u32 = @intFromEnum(self);
            return tag_u32 >= @intFromEnum(Tag.keywords_start) and
                tag_u32 <= @intFromEnum(Tag.keywords_end);
        }

        pub fn isValidPropertyName(self: Tag) bool {
            return self == .identifier or self.isKeyword();
        }
    };

    tag: Tag,
    /// Byte index into the source string
    start: u32,
    /// Size of the token's text in bytes
    len: u32,
    /// 0-indexed line location of the token.
    /// For multi-line tokens like template literals, this only stores the start line.
    line: u32,

    /// Return the token's text as a byte slice.
    pub fn toByteSlice(self: Token, source: []const u8) []const u8 {
        return source[self.start .. self.start + self.len];
    }

    /// (line, column) position for the start of this token.
    pub fn startCoord(self: Token, source: []const u8) Coordinate {
        return offsets.byteIndexToCoordinate(source, self.start);
    }

    /// (line, column) position for the end of this token.
    pub fn endCoord(self: Token, source: []const u8) Coordinate {
        return offsets.byteIndexToCoordinate(source, self.start + self.len);
    }

    /// (line, column) range for the end of this token.
    pub fn toRange(self: Token, source: []const u8) types.Range {
        // TODO: end-coord can be inferred from start-coord.
        // optimize this.
        return types.Range{
            .start = self.startCoord(source),
            .end = self.endCoord(source),
        };
    }

    pub fn isAssignmentOperator(self: *const Token) bool {
        const tag_u32: u32 = @intFromEnum(self.tag);
        return tag_u32 >= @intFromEnum(Tag.assignment_op_start) and
            tag_u32 <= @intFromEnum(Tag.assignment_op_end);
    }
};