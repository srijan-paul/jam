const syntax = @import("syntax");

pub const JsTokenTag = enum(u32) {
    // both single and multiline comments
    // TODO(@injuly): should there be a separate tag for multi-line comments?
    comment,

    // whitespaces and newlines
    // TODO(@injuly): should there be a separate tag for whitespaces that include newlines?
    whitespace,

    identifier,
    private_identifier,
    numeric_literal,
    string_literal,
    regex_literal,

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
    // TODO: enum and await are reserved, not contextual.
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

    pub fn isContextualKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 > @intFromEnum(JsTokenTag.contextual_keywords_start) and
            tag_u32 < @intFromEnum(JsTokenTag.contextual_keywords_end);
    }

    pub fn isStrictModeKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 > @intFromEnum(JsTokenTag.strict_mode_kw_start) and
            tag_u32 < @intFromEnum(JsTokenTag.strict_mode_kw_end);
    }

    pub fn isKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 >= @intFromEnum(JsTokenTag.keywords_start) and
            tag_u32 <= @intFromEnum(JsTokenTag.keywords_end);
    }

    pub fn isValidPropertyName(self: JsTokenTag) bool {
        return self == .identifier or self.isKeyword();
    }

    pub fn isAssignmentOperator(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 >= @intFromEnum(JsTokenTag.assignment_op_start) and
            tag_u32 <= @intFromEnum(JsTokenTag.assignment_op_end);
    }
};

pub const Token = syntax.Token(JsTokenTag);
