const syntax = @import("syntax");

/// Even though a token tag is 32 bits,
/// We will only need the lower 8 bits (value 0-255) for the token tag.
/// The higher bits then, are used for storing additional information about the token.
/// i.e: Is this token a keyword? A binary operator? A numeric literal?
/// Answering these questions is simply a matter of `tag & mask != 0`.
pub const Mask = struct {
    // Bits 18-20 are used to determine if the token is a keyword
    pub const IsKeyword: u32 = 1 << 12;
    pub const ContextualKeyword: u32 = IsKeyword | (1 << 13);
    pub const StrictModeKeyword: u32 = IsKeyword | (1 << 14);

    pub const IsAssignOp: u32 = 1 << 15;
    pub const IsBinaryOp: u32 = 1 << 16;
    pub const IsUnaryOp: u32 = 1 << 17;

    pub const IsNumericLiteral: u32 = 1 << 18;
    pub const IsIdentifier: u32 = 1 << 19;
    pub const IsLogicalOp: u32 = 1 << 20; // || and &&
    pub const IsJsxAttributeStart: u32 = 1 << 21;
    pub const Trivia: u32 = 1 << 22;

    /// For binary operators (token.tag & Mask.IsBinaryOp != 0),
    /// bits 8-11 store the precedence of the token.
    pub const PrecShift: u32 = 8;
    pub const PrecEnd: u32 = 15 << PrecShift;
};

pub const JsTokenTag = enum(u32) {

    // both single and multiline comments
    // TODO(@injuly): should there be a separate tag for multi-line comments?
    comment = 1 | Mask.Trivia,

    // whitespaces and newlines
    // TODO(@injuly): should there be a separate tag for whitespaces that include newlines?
    whitespace = 2 | Mask.Trivia,

    identifier = 3 | Mask.IsIdentifier | Mask.IsJsxAttributeStart,
    /// An identifier that is contains non-ASCII characters.
    /// This is a separate token kind because it allows us to have fewer allocations
    /// in some places in the parser.
    non_ascii_identifier = 4 | Mask.IsIdentifier | Mask.IsJsxAttributeStart,
    private_identifier = 5 | Mask.IsIdentifier,
    /// A private identifier that contains non-ASCII characters.
    /// See: non_ascii_identifier
    private_non_ascii_identifier = 6 | Mask.IsIdentifier,

    decimal_literal = 7 | Mask.IsNumericLiteral,
    /// 0(o|O)[0-7]+
    octal_literal = 8 | Mask.IsNumericLiteral,
    /// 0(x|X)[0-9|a-f|A-F]+
    hex_literal = 9 | Mask.IsNumericLiteral,
    /// 0(b|B)(0|1)+
    binary_literal = 10 | Mask.IsNumericLiteral,
    /// octal literal starting with '0', but not with '0o' or '0O',
    /// e.g: 012
    legacy_octal_literal = 11 | Mask.IsNumericLiteral,

    string_literal = 13,
    regex_literal = 14,
    template_literal_start = 15,
    template_literal_expression = 16,
    template_literal_part = 17,
    template_literal_end = 18,

    @"\"" = 19,
    @"'" = 20,
    @"{" = 21 | Mask.IsJsxAttributeStart,
    @"}" = 22,
    @"(" = 23,
    @")" = 24,
    @"[" = 25,
    @"]" = 26,
    @"." = 27,
    @"..." = 28,
    @";" = 29,
    @"," = 30,

    @"++" = 53,
    @"--" = 54,

    // Binary operators arranged in order of precedence (least binding to highest binding).
    @"**" = 52,
    @"??" = 67 | Mask.IsBinaryOp | (1 << Mask.PrecShift),
    @"||" = 66 | Mask.IsBinaryOp | Mask.IsLogicalOp | (2 << Mask.PrecShift),
    @"&&" = 65 | Mask.IsBinaryOp | Mask.IsLogicalOp | (3 << Mask.PrecShift),
    @"|" = 61 | Mask.IsBinaryOp | (4 << Mask.PrecShift),
    @"^" = 62 | Mask.IsBinaryOp | (5 << Mask.PrecShift),
    @"&" = 60 | Mask.IsBinaryOp | (6 << Mask.PrecShift),

    @"==" = 38 | Mask.IsBinaryOp | (7 << Mask.PrecShift),
    @"!=" = 39 | Mask.IsBinaryOp | (7 << Mask.PrecShift),
    @"===" = 40 | Mask.IsBinaryOp | (7 << Mask.PrecShift),
    @"!==" = 41 | Mask.IsBinaryOp | (7 << Mask.PrecShift),

    kw_in = 91 | Mask.IsKeyword | Mask.IsBinaryOp | (8 << Mask.PrecShift),
    kw_instanceof = 92 | Mask.IsKeyword | Mask.IsBinaryOp | (8 << Mask.PrecShift),
    @"<" = 32 | Mask.IsBinaryOp | (8 << Mask.PrecShift),
    @">" = 33 | Mask.IsBinaryOp | (8 << Mask.PrecShift),
    @"<=" = 34 | Mask.IsBinaryOp | (8 << Mask.PrecShift),
    @">=" = 35 | Mask.IsBinaryOp | (8 << Mask.PrecShift),

    @"<<" = 56 | Mask.IsBinaryOp | (9 << Mask.PrecShift),
    @">>" = 57 | Mask.IsBinaryOp | (9 << Mask.PrecShift),
    @">>>" = 58 | Mask.IsBinaryOp | (9 << Mask.PrecShift),

    @"+" = 44 | Mask.IsBinaryOp | (10 << Mask.PrecShift) | Mask.IsUnaryOp,
    @"-" = 45 | Mask.IsBinaryOp | (10 << Mask.PrecShift) | Mask.IsUnaryOp,

    @"*" = 48 | Mask.IsBinaryOp | (11 << Mask.PrecShift),
    @"/" = 49 | Mask.IsBinaryOp | (11 << Mask.PrecShift),
    @"%" = 50 | Mask.IsBinaryOp | (11 << Mask.PrecShift),

    @"!" = 63 | Mask.IsUnaryOp,
    @"~" = 64 | Mask.IsUnaryOp,

    @"?." = 68,
    @"?" = 69,
    @":" = 70,

    // assignment operators
    @"=" = 71 | Mask.IsAssignOp,
    @"%=" = 72 | Mask.IsAssignOp,
    @"+=" = 73 | Mask.IsAssignOp,
    @"*=" = 74 | Mask.IsAssignOp,
    @"-=" = 75 | Mask.IsAssignOp,
    @">>=" = 76 | Mask.IsAssignOp,
    @">>>=" = 77 | Mask.IsAssignOp,
    @"&=" = 78 | Mask.IsAssignOp,
    @"|=" = 79 | Mask.IsAssignOp,
    @"^=" = 80 | Mask.IsAssignOp,
    @"&&=" = 81 | Mask.IsAssignOp,
    @"||=" = 82 | Mask.IsAssignOp,
    @"/=" = 83 | Mask.IsAssignOp,
    @"??=" = 84 | Mask.IsAssignOp,
    @"**=" = 85 | Mask.IsAssignOp,
    @"<<=" = 86 | Mask.IsAssignOp,

    @"=>" = 88,

    // keywords.
    kw_true = 93 | Mask.IsKeyword,
    kw_false = 94 | Mask.IsKeyword,
    kw_null = 95 | Mask.IsKeyword,
    kw_var = 96 | Mask.IsKeyword,
    kw_let = 97 | Mask.IsKeyword,
    kw_const = 98 | Mask.IsKeyword,
    kw_break = 99 | Mask.IsKeyword,
    kw_case = 100 | Mask.IsKeyword,
    kw_catch = 101 | Mask.IsKeyword,
    kw_class = 102 | Mask.IsKeyword,
    kw_continue = 103 | Mask.IsKeyword,
    kw_debugger = 104 | Mask.IsKeyword,
    kw_default = 105 | Mask.IsKeyword,
    kw_do = 106 | Mask.IsKeyword,
    kw_else = 107 | Mask.IsKeyword,
    kw_export = 108 | Mask.IsKeyword,
    kw_extends = 109 | Mask.IsKeyword,
    kw_finally = 110 | Mask.IsKeyword,
    kw_for = 111 | Mask.IsKeyword,
    kw_function = 112 | Mask.IsKeyword,
    kw_if = 113 | Mask.IsKeyword,
    kw_import = 114 | Mask.IsKeyword,
    kw_new = 115 | Mask.IsKeyword,
    kw_return = 116 | Mask.IsKeyword,
    kw_super = 117 | Mask.IsKeyword,
    kw_switch = 118 | Mask.IsKeyword,
    kw_this = 119 | Mask.IsKeyword,
    kw_throw = 120 | Mask.IsKeyword,
    kw_try = 121 | Mask.IsKeyword,
    kw_while = 122 | Mask.IsKeyword,
    kw_with = 123 | Mask.IsKeyword,

    kw_typeof = 124 | Mask.IsKeyword | Mask.IsUnaryOp,
    kw_void = 125 | Mask.IsKeyword | Mask.IsUnaryOp,
    kw_delete = 126 | Mask.IsKeyword | Mask.IsUnaryOp,
    kw_enum = 127 | Mask.IsKeyword,

    // strict mode keywords
    kw_implements = 129 | Mask.StrictModeKeyword,
    kw_interface = 130 | Mask.StrictModeKeyword,
    kw_package = 131 | Mask.StrictModeKeyword,
    kw_private = 132 | Mask.StrictModeKeyword,
    kw_protected = 133 | Mask.StrictModeKeyword,
    kw_public = 134 | Mask.StrictModeKeyword,
    kw_static = 135 | Mask.StrictModeKeyword,
    kw_yield = 136 | Mask.StrictModeKeyword,

    // contextual keywords.
    // TODO: enum and await are reserved, not contextual.
    kw_as = 139 | Mask.ContextualKeyword,
    kw_await = 140 | Mask.ContextualKeyword,
    kw_async = 141 | Mask.ContextualKeyword,
    kw_constructor = 142 | Mask.ContextualKeyword,
    kw_get = 143 | Mask.ContextualKeyword,
    kw_set = 144 | Mask.ContextualKeyword,
    kw_from = 145 | Mask.ContextualKeyword,
    kw_of = 146 | Mask.ContextualKeyword,

    jsx_text = 147,
    jsx_identifier = 148 | Mask.IsJsxAttributeStart,

    eof = 149,

    pub inline fn is(self: JsTokenTag, mask: u32) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & mask != 0;
    }

    pub fn precedence(self: JsTokenTag) u32 {
        const tag_u32: u32 = @intFromEnum(self);
        return (tag_u32 & Mask.PrecEnd) >> Mask.PrecShift;
    }

    pub fn isContextualKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.ContextualKeyword == Mask.ContextualKeyword;
    }

    pub inline fn isNumericLiteral(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.IsNumericLiteral != 0;
    }

    pub fn isStrictModeKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.StrictModeKeyword == Mask.StrictModeKeyword;
    }

    pub fn isKeyword(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.IsKeyword != 0;
    }

    pub fn isValidPropertyName(self: JsTokenTag) bool {
        return self == .identifier or
            self == .non_ascii_identifier or
            self.isKeyword();
    }

    pub inline fn isIdentifier(self: JsTokenTag) bool {
        return @intFromEnum(self) & Mask.IsIdentifier != 0;
    }

    pub inline fn isAssignmentOperator(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.IsAssignOp != 0;
    }

    pub inline fn isLogicalOperator(self: JsTokenTag) bool {
        const tag_u32: u32 = @intFromEnum(self);
        return tag_u32 & Mask.IsLogicalOp != 0;
    }

    comptime {
        const std = @import("std");
        std.debug.assert(std.meta.fields(JsTokenTag).len < std.math.maxInt(u8));
    }
};

pub const Token = syntax.Token(JsTokenTag);
