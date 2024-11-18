// A recursive decent parser for JavaScript.

const std = @import("std");

const Self = @This();
const Tokenizer = @import("./tokenize.zig");
const Token = @import("./token.zig").Token;
const ast = @import("./ast.zig");
const StringHelper = @import("./strings.zig");

const util = @import("util");
const types = util.types;

const Node = ast.Node;
const NodeData = ast.NodeData;

pub const Error = error{
    UnexpectedToken,
    /// Invalid modifier like 'async'
    IllegalModifier,
    /// Return statement outside functions
    IllegalReturn,
    /// Break statement outside loops
    IllegalBreak,
    /// Continue statement outside loops
    IllegalContinue,
    /// Labeled statement in places like the body of a for loop.
    IllegalLabeledStatement,
    /// Await outside async scope.
    IllegalAwait,
    /// '=>' Not on the same line as arrow parameters
    IllegalFatArrow,
    /// Found a newline where there shouldn't be one
    IllegalNewline,
    /// Incorrect use of a meta property like 'new.target'
    InvalidMetaProperty,
    /// 5 = ...
    InvalidAssignmentTarget,
    ///  (1) => ...
    InvalidArrowParameters,
    InvalidArrowFunction,
    /// Setter must have exactly one parameter
    InvalidSetter,
    /// Getter must have no parameters
    InvalidGetter,
    /// {x=1, x: 5} is neither a valid object literal nor a valid assignment pattern.
    InvalidObject,
    /// for (1 of []) {}
    InvalidLoopLhs,
    /// function (1) {}; function(...xs, x) {}, etc.
    InvalidFunctionParameter,
    /// 'let' used as an identifier in strict mode.
    LetInStrictMode,
    /// let x = { x =  1} <- pattern where an object literal should've been
    UnexpectedPattern,
    /// let x = 5 let 6 = 5 <- no ';' between statements
    ExpectedSemicolon,
    /// let [a, ...as, b] = [1, 2, 3] <- 'as' must be the last
    RestElementNotLast,
    /// let { x, y } <- destructuring pattern must have an initializer
    MissingInitializer,
    /// Multiple default clauses in a switch statement.
    /// switch (x) { default: 1 default: 2 }
    MultipleDefaults,
    /// Class definition with multiple constructors
    MultipleConstructors,
    /// "with" statement used in strict mode.
    WithInStrictMode,
    /// std.mem.Allocator.Error
    OutOfMemory,
    // TODO: remove these two errors
    JsxNotImplemented,
    TypeScriptNotImplemented,
} || Tokenizer.Error;
const ParseFn = fn (self: *Self) Error!Node.Index;

/// An error or warning raised by the Parser.
pub const Diagnostic = struct {
    /// line/col position where error occurred.
    coord: types.Coordinate,
    /// Message reported by the parser.
    message: []const u8,
};

/// Represents the currently active set of grammatical parameters
/// in the parser state.
/// https://tc39.es/ecma262/#sec-grammatical-parameters
pub const ParseContext = packed struct(u8) {
    /// Is a ReturnStatement allowed in the current context?
    /// Unlike `await`, `return` is always parsed as a keyword, regardles of context.
    @"return": bool = false,
    /// Is a BreakStatement allowed in the current context?
    @"break": bool = false,
    /// Is a ContinueStatement allowed in the current context?
    @"continue": bool = false,
    /// Is an `await` parsed as an identifier or as a keyword?
    is_await_reserved: bool = false,
    /// Is `yield` parsed as an identifier or as a keyword?
    is_yield_reserved: bool = false,
    /// Are we parsing a module? (`false` = parsing a script).
    module: bool = false,
    /// Are we in strict mode?
    strict: bool = false,
    /// Are `in` statements allowed?
    /// Used to parse for-in loops.
    in: bool = true,
};

pub const SourceType = enum { script, module };
/// Configuration options for the parser.
pub const Config = struct {
    /// Whether we're parsing a script or a module.
    source_type: SourceType = .module,
    /// Enable strict mode by default
    strict_mode: bool = false,
    /// Enable JSX support
    jsx: bool = false,
    /// Enable typescript support
    typescript: bool = false,
};

// arranged in highest to lowest binding order

// TODO: Officially, exponentiation operator is defined as:
// ExponentiationExpression :
//  UnaryExpression
//  | UpdateExpression ** ExponentiationExpression
// So this isn't exactly correct.
// The parselet for this will have to be hand-written.
const exponentExpr = makeRightAssoc(.@"**", Self.unaryExpression);

const multiplicativeExpr = makeLeftAssoc(.multiplicative_start, .multiplicative_end, exponentExpr);
const additiveExpr = makeLeftAssoc(.additive_start, .additive_end, multiplicativeExpr);
const shiftExpr = makeLeftAssoc(.shift_op_start, .shift_op_end, additiveExpr);

// this one has to be hand-written to disallow 'in' inside for-loop iterators.
// see: forStatement
fn relationalExpr(self: *Self) Error!Node.Index {
    var node = try shiftExpr(self);

    const rel_op_start: u32 = @intFromEnum(Token.Tag.relational_start);
    const rel_op_end: u32 = @intFromEnum(Token.Tag.relational_end);

    var token = self.peek();
    while (true) : (token = self.peek()) {
        const itag: u32 = @intFromEnum(token.tag);
        if ((itag > rel_op_start and itag < rel_op_end) or
            token.tag == .kw_in or token.tag == .kw_instanceof)
        {

            // `in` expressions are not allowed when parsing the LHS
            // for `for-loop` iterators.
            if (token.tag == .kw_in and !self.context.in) {
                break;
            }

            self.current_destructure_kind.can_be_assigned_to = false;
            self.current_destructure_kind.can_destruct = false;

            const op_token = try self.next();
            const rhs = try shiftExpr(self);
            const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
            const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
            const binary_pl: ast.BinaryPayload = .{
                .lhs = node,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            };

            node = try self.addNode(.{ .binary_expr = binary_pl }, start_pos, end_pos);
        } else {
            break;
        }
    }

    return node;
}

const eqExpr = makeLeftAssoc(.eq_op_start, .eq_op_end, relationalExpr);

const bAndExpr = makeLeftAssoc(.@"&", .@"&", eqExpr);
const bXorExpr = makeLeftAssoc(.@"^", .@"^", bAndExpr);
const bOrExpr = makeLeftAssoc(.@"|", .@"|", bXorExpr);

const lAndExpr = makeLeftAssoc(.@"&&", .@"&&", bOrExpr);
const lOrExpr = makeLeftAssoc(.@"||", .@"||", lAndExpr);

allocator: std.mem.Allocator,

/// Immutable reference to the source code.
source: []const u8,
/// Tokens are consumed on-demand, instead of being pre-scanned into a list.
/// This is important because somtimes, tokens are context sensitive.
/// For instance: a '/' can either be the start of a division-related operator, or
/// the start of a regular expression literal.
tokenizer: Tokenizer,
/// All AST nodes are stored in this flat list and reference
/// each other using their indices.
/// This is an *append only* list.
nodes: std.MultiArrayList(Node),
/// Extra information about a node, if any. See: `ast.ExtraData`.
extra_data: std.ArrayList(ast.ExtraData),
/// List of tokens that are necessary to keep around
/// e.g - identifiers, literals, function names, etc.
tokens: std.ArrayList(Token),
/// Arguments for function calls, new-expressions, etc.
node_lists: std.ArrayList(Node.Index),
/// List of error messages and warnings generated during parsing.
diagnostics: std.ArrayList(Diagnostic),
/// Temporary array-list for intermediate allocations
/// throughout the parser, e.g: list of case-blocks, list of statements.
scratch: std.ArrayList(Node.Index),
/// The token that we're currently at.
/// Calling `next()` or `peek()` will return this token.
current_token: Token,
/// Line number of the token that was consumed previously.
/// When being accessed, this property can be thought of as the starting line
/// of the first token in the expression/statement that was last parsed.
/// Useful for Automatic Semicolon Insertion.
prev_token_line: u32 = 0,
/// Helper struct to manage, escape, and compare strings in source code.
strings: StringHelper,
/// The current grammatical context of the parser. See struct `ParseContext`.
context: ParseContext = .{},
/// The kind of destructuring that is allowed for the expression
/// that was just parsed by the parser.
current_destructure_kind: DestructureKind = @bitCast(@as(u8, 0)),
/// Whether we're parsing a module or a script.
source_type: SourceType,
/// Whether we're parsing JSX
jsx: bool = false,
/// Whether we're parsing TypeScript
typescript: bool = false,
/// Bit-flags to keep track of whether the
/// most recently parsed expression can be destructured.
const DestructureKind = packed struct(u8) {
    pub const MustDestruct = DestructureKind{
        .can_destruct = true,
        .can_be_assigned_to = true,
        .must_destruct = true,
    };

    pub const CannotDestruct = DestructureKind{
        .can_destruct = false,
        .must_destruct = false,
        .can_be_assigned_to = false,
    };

    /// `true` for any expression that is a valid destructuring target.
    /// `false` if the expression cannot be destructured and bound.
    /// Example:
    /// ```js
    /// ({ x: x.z })  // .can_destruct = false
    /// ({ x: z })    // .can_destruct = true
    /// ```
    can_destruct: bool = true,
    /// `true` for patterns like `{x=1}` that are valid
    /// assignment patterns, but not valid object literals.
    /// ```js
    /// let { x = 1 } = {}
    /// //  ^ .must_destruct       = true
    /// //    .can_destruct        = true
    /// //    .can_be_assigned_to  = true
    /// ```
    must_destruct: bool = false,
    /// `true` for patterns like `{ x = y.z }`
    /// that are valid LHS for assignments and for-loop iterators,
    /// but invalid destructuring targets.
    /// EXAMPLE:
    /// ```js
    /// ({x = y.z} = {}) // OK, assignment
    /// let { x = y.z } = {} // SYNTAX ERROR, destructured binding
    /// ```
    /// Used to differentiate between BindingPattern and AssignmentPattern
    /// grammar productions.
    can_be_assigned_to: bool = true,
    /// padding
    _: u5 = 0,

    /// Update the flags to indiciate that the current expression
    /// can neither be destructured nor assigned to.
    pub inline fn setNoAssignOrDestruct(self: *DestructureKind) void {
        self.can_destruct = false;
        self.can_be_assigned_to = false;
    }

    /// Sets it to a state where that implies an expression
    /// can neither be destructured, nor be assigned to.
    pub inline fn reset(self: *DestructureKind) void {
        self.* = .{
            .can_destruct = false,
            .can_be_assigned_to = false,
            .must_destruct = false,
        };
    }

    /// Merge the properties of two DestructureKinds.
    /// Used in combining the destructuring properties of property definition lists
    /// and array items.
    /// DestructureKind(object) = update(DestructureKind(prop1), DestructureKind(prop2)...)
    /// DestructureKind(array) = update(DestructureKind(item1), DestructureKind(item2)...)
    pub inline fn update(self: *DestructureKind, other: DestructureKind) void {
        // TODO(@injuly):
        // With some tweaking of this packed struct, this can be optimized to a bitwise and `&`.
        if (!other.can_be_assigned_to) self.can_be_assigned_to = false;
        if (!other.can_destruct) self.can_destruct = false;
        if (other.must_destruct) self.must_destruct = true;
    }

    pub fn isMalformed(self: *DestructureKind) bool {
        return self.must_destruct and !self.can_destruct and !self.can_be_assigned_to;
    }
};

pub fn init(
    allocator: std.mem.Allocator,
    source: []const u8,
    config: Config,
) Error!Self {
    var self = Self{
        .allocator = allocator,
        .source = source,
        .tokenizer = try Tokenizer.init(source, config),
        .current_token = undefined,

        .source_type = config.source_type,

        .diagnostics = try std.ArrayList(Diagnostic).initCapacity(allocator, 2),
        .nodes = .{},
        .scratch = try std.ArrayList(Node.Index).initCapacity(allocator, 32),
        .node_lists = try std.ArrayList(Node.Index).initCapacity(allocator, 32),
        .extra_data = try std.ArrayList(ast.ExtraData).initCapacity(allocator, 32),
        .tokens = try std.ArrayList(Token).initCapacity(allocator, 256),
        .strings = try StringHelper.init(allocator, source),
    };

    // "module"s are always parsed in strict mode.
    self.context.strict = config.strict_mode or config.source_type == SourceType.module;

    if (config.jsx) {
        self.jsx = true;
        return Error.JsxNotImplemented;
    }

    if (config.typescript) {
        self.typescript = true;
        return Error.TypeScriptNotImplemented;
    }

    errdefer self.deinit();

    // the `null` node always lives at index-0.
    // see: ast.NodeData.none
    const i = try self.addNode(.{ .none = {} }, 0, 0);
    std.debug.assert(i == Node.Index.empty);

    // this call will initialize `current_token`.
    _ = try self.next();
    return self;
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit(self.allocator);
    self.tokens.deinit();
    self.node_lists.deinit();
    self.extra_data.deinit();
    self.scratch.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit();
    self.strings.deinit();
}

pub fn parse(self: *Self) !Node.Index {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    if (self.source_type == .module) {
        while (self.current_token.tag != .eof) {
            const stmt = try self.moduleItem();
            try self.scratch.append(stmt);
        }
    } else {
        while (self.current_token.tag != .eof) {
            const stmt = try self.statementOrDeclaration();
            try self.scratch.append(stmt);
        }
    }

    const statements = self.scratch.items[prev_scratch_len..];
    const stmt_list = try self.addSubRange(statements);
    return try self.addNode(
        .{ .program = stmt_list },
        0,
        @intCast(self.source.len),
    );
}

pub fn moduleItem(self: *Self) Error!Node.Index {
    return switch (self.current_token.tag) {
        .kw_import => self.importDeclaration(),
        .kw_export => self.exportDeclaration(),
        else => self.statementOrDeclaration(),
    };
}

// ----------------------------------------------------------------------------
// Statements and declarators.
// https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations
// ----------------------------------------------------------------------------

pub fn importDeclaration(self: *Self) Error!Node.Index {
    const import_kw = try self.next();
    std.debug.assert(import_kw.tag == .kw_import);

    // import "foo";
    if (self.isAtToken(.string_literal)) {
        return self.completeImportDeclaration(import_kw.start, &.{});
    }

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    // If there is a trailing ',' after the default import,
    // we must parse a list of import specifiers inside '{}'
    var trailing_comma = false;

    // Whether we have at least one import (either default, namespace, or specifier list)
    // This is to avoid parsing `import from "foo";` as a valid import statement.
    var has_imports = false;

    const cur = self.current_token.tag;
    if (cur == .identifier or self.isKeywordIdentifier(cur)) {
        has_imports = true;
        const default_specifier = try self.defaultImportSpecifier();
        try self.scratch.append(default_specifier);

        trailing_comma = self.isAtToken(.@",");
        if (trailing_comma) {
            _ = try self.next();
        }
    }

    // import * as foo from "foo"
    if (self.isAtToken(.@"*")) {
        has_imports = true;
        const specifier = try self.starImportSpecifier();
        try self.scratch.append(specifier);

        _ = try self.expect(.kw_from);
        return self.completeImportDeclaration(
            import_kw.start,
            self.scratch.items[prev_scratch_len..],
        );
    }

    if (trailing_comma or self.isAtToken(.@"{")) {
        has_imports = true;
        _ = try self.expect(.@"{");

        while (self.current_token.tag != .@"}") {
            const specifier = try self.importSpecifier();
            try self.scratch.append(specifier);
            const comma_or_rb = try self.expect2(.@",", .@"}");
            if (comma_or_rb.tag == .@"}")
                break;
        } else {
            _ = try self.next(); // eat '}'
        }
    }

    if (!has_imports) {
        try self.emitDiagnosticOnToken(
            self.current_token,
            "Expected an import specifier, '*', or a default import",
            .{},
        );
        return Error.UnexpectedToken;
    }

    const specifiers = self.scratch.items[prev_scratch_len..];
    _ = try self.expect(.kw_from);
    return self.completeImportDeclaration(import_kw.start, specifiers);
}

/// Assuming 'current_token' is '*', parse a import namespace specifier
/// like `* as Identifier`
fn starImportSpecifier(self: *Self) Error!Node.Index {
    const star_token = try self.expect(.@"*");
    _ = try self.expect(.kw_as);

    const name_token = try self.expect(.identifier);
    const name = try self.identifier(name_token);

    const specifier = try self.addNode(
        .{
            .import_namespace_specifier = .{ .name = name },
        },
        star_token.start,
        name_token.start + name_token.len,
    );

    return specifier;
}

/// Assuming everything upto 'from' has been consumed,
/// parse the source of the import declaration, and return the import node.
fn completeImportDeclaration(
    self: *Self,
    start_pos: u32,
    specifiers: []const Node.Index,
) Error!Node.Index {
    const source = try self.stringLiteral();
    var end_pos = self.nodes.items(.end)[@intFromEnum(source)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(
        .{
            .import_declaration = .{
                .source = source,
                .specifiers = try self.addSubRange(specifiers),
            },
        },
        start_pos,
        end_pos,
    );
}

/// Parse the next identifier token as a default import specifier.
fn defaultImportSpecifier(self: *Self) Error!Node.Index {
    const name_token = try self.next();
    const name = try self.identifier(name_token);
    return self.addNode(
        .{
            .import_default_specifier = .{ .name = name },
        },
        name_token.start,
        name_token.start + name_token.len,
    );
}

fn importSpecifier(self: *Self) Error!Node.Index {
    var name_token: Token = undefined; // assigned in the 'blk' block
    const name = blk: {
        if (self.isAtToken(.string_literal)) {
            name_token = try self.next();
            break :blk try self.stringLiteralFromToken(name_token);
        }

        name_token = try self.next();
        break :blk try self.identifier(name_token);
    };

    if (self.isAtToken(.kw_as) or name_token.tag == .string_literal) {
        _ = try self.expect(.kw_as); // eat 'as'
        const alias_token = try self.next();
        const alias = try self.identifier(alias_token);
        return self.addNode(
            .{
                .import_specifier = .{
                    .local = alias,
                    .imported = null,
                },
            },
            name_token.start,
            alias_token.start + alias_token.len,
        );
    }

    return self.addNode(
        .{ .import_specifier = .{ .local = name, .imported = null } },
        name_token.start,
        name_token.start + name_token.len,
    );
}

fn moduleExportName(self: *Self) Error!Node.Index {
    if (self.isAtToken(.string_literal))
        return self.stringLiteral();

    const name_token = try self.next();
    if (name_token.tag != .identifier and
        name_token.tag != .kw_default and
        !self.isKeywordIdentifier(name_token.tag))
    {
        try self.emitBadTokenDiagnostic("an identifier", &name_token);
        return Error.UnexpectedToken;
    }

    return self.identifier(name_token);
}

/// Parse an 'export' declaration:
/// https://tc39.es/ecma262/#prod-ExportDeclaration
fn exportDeclaration(self: *Self) Error!Node.Index {
    const export_kw = try self.next();
    std.debug.assert(export_kw.tag == .kw_export);

    if (self.isAtToken(.kw_default)) {
        return try self.defaultExport(&export_kw);
    }

    const maybe_decl = blk: {
        switch (self.current_token.tag) {
            .kw_var, .kw_let, .kw_const => {
                const var_kw = try self.next();
                break :blk try self.variableStatement(var_kw);
            },
            else => break :blk try self.declarationStatement(),
        }
    };

    if (maybe_decl) |decl| {
        return self.addNode(
            .{ .export_declaration = .{ .declaration = decl } },
            export_kw.start,
            self.nodes.items(.end)[@intFromEnum(decl)],
        );
    }

    if (self.isAtToken(.@"{")) {
        _ = try self.expect(.@"{");
        const specifiers = try self.namedExportList();
        const rbrace = try self.expect(.@"}");
        var end_pos = rbrace.start + rbrace.len;

        // A "from" after an export specifier list implies
        // an export statement like:
        // ```js
        // export { x, y } from "foo";
        // ```
        if (self.isAtToken(.kw_from)) {
            _ = try self.expect(.kw_from);
            end_pos = self.current_token.start + self.current_token.len;
            const source = try self.stringLiteral();

            end_pos = try self.semiColon(end_pos);
            return self.addNode(
                .{
                    .export_from_declaration = .{
                        .specifiers = specifiers,
                        .source = source,
                    },
                },
                export_kw.start,
                end_pos,
            );
        }

        end_pos = try self.semiColon(end_pos);
        return self.addNode(.{
            .export_list_declaration = .{ .specifiers = specifiers },
        }, export_kw.start, end_pos);
    }

    if (self.isAtToken(.@"*")) {
        return self.starExportDeclaration(export_kw);
    }

    try self.emitDiagnosticOnToken(
        self.current_token,
        "Expected an export declaration",
        .{},
    );

    return Error.UnexpectedToken;
}

fn starExportDeclaration(self: *Self, export_kw: Token) Error!Node.Index {
    _ = try self.expect(.@"*");

    const name = blk: {
        if (self.isAtToken(.kw_as)) {
            _ = try self.next();
            const name_token = try self.next();
            if (name_token.tag != .identifier and
                !self.isKeywordIdentifier(name_token.tag))
            {
                try self.emitBadTokenDiagnostic("an identifier", &name_token);
                return Error.UnexpectedToken;
            }

            break :blk try self.identifier(name_token);
        }

        break :blk null;
    };

    _ = try self.expect(.kw_from);

    const source = try self.stringLiteral();
    var end_pos = self.nodes.items(.end)[@intFromEnum(source)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(
        .{
            .export_all_declaration = .{
                .name = name,
                .source = source,
            },
        },
        export_kw.start,
        end_pos,
    );
}

fn defaultExport(self: *Self, export_kw: *const Token) Error!Node.Index {
    const default_kw = try self.next();
    std.debug.assert(default_kw.tag == .kw_default);
    var end_pos = default_kw.start + default_kw.len;

    const exported = blk: {
        switch (self.current_token.tag) {
            .kw_class => {
                const class_kw = try self.next();
                const class = try self.classExpression(class_kw);
                end_pos = self.nodes.items(.end)[@intFromEnum(class)];
                break :blk class;
            },
            .kw_function => {
                const fn_token = try self.next();
                const func = try self.functionExpression(fn_token.start, .{});
                end_pos = self.nodes.items(.end)[@intFromEnum(func)];
                break :blk func;
            },
            .kw_async => {
                const async_line = self.current_token.line;
                const lookahead = try self.lookAhead();
                if (lookahead.tag == .kw_function and lookahead.line == async_line) {
                    const async_token = try self.next(); // eat 'async'
                    _ = try self.next(); // eat 'function'
                    const func = try self.functionExpression(
                        async_token.start,
                        .{ .is_async = true },
                    );
                    end_pos = self.nodes.items(.end)[@intFromEnum(func)];
                    break :blk func;
                }
                const expr = try self.assignExpressionNoPattern();
                end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
                end_pos = try self.semiColon(end_pos);
                break :blk expr;
            },
            else => {
                const expr = try self.assignExpressionNoPattern();
                end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
                end_pos = try self.semiColon(end_pos);
                break :blk expr;
            },
        }
    };

    return self.addNode(
        .{
            .export_declaration = .{
                .declaration = exported,
                .default = true,
            },
        },
        export_kw.start,
        end_pos,
    );
}

pub fn namedExportList(self: *Self) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (self.current_token.tag != .@"}") {
        const specifier = try self.exportSpecifier();
        try self.scratch.append(specifier);
        if (self.isAtToken(.@"}")) break;
        _ = try self.expect(.@",");
    }

    const specifiers = self.scratch.items[prev_scratch_len..];
    return try self.addSubRange(specifiers);
}

fn exportSpecifier(self: *Self) Error!Node.Index {
    const local = try self.moduleExportName();
    const start_pos = self.nodes.items(.start)[@intFromEnum(local)];
    var end_pos = self.nodes.items(.end)[@intFromEnum(local)];

    const alias = blk: {
        if (!self.isAtToken(.kw_as)) break :blk null;

        _ = try self.next(); // eat 'as'
        const alias_ = try self.moduleExportName();
        end_pos = self.nodes.items(.end)[@intFromEnum(alias_)];
        break :blk alias_;
    };

    return self.addNode(
        .{
            .export_specifier = .{
                .local = local,
                .exported = alias,
            },
        },
        start_pos,
        end_pos,
    );
}

/// https://tc39.es/ecma262/#prod-Statement
fn statementOrDeclaration(self: *Self) Error!Node.Index {
    if (try self.declarationStatement()) |decl| return decl;
    return self.statement();
}

fn statement(self: *Self) Error!Node.Index {
    return switch (self.current_token.tag) {
        .@"{" => self.blockStatement(),
        .@";" => self.emptyStatement(),
        .kw_if => self.ifStatement(),
        .kw_for => self.forStatement(),
        .kw_do => self.doWhileStatement(),
        .kw_while => self.whileStatement(),
        .kw_debugger => self.debuggerStatement(),
        .kw_return => self.returnStatement(),
        .kw_break => self.breakStatement(),
        .kw_continue => self.continueStatement(),
        .kw_let => self.letStatement(),
        .kw_var, .kw_const => self.variableStatement(try self.next()),
        .kw_try => self.tryStatement(),
        .kw_switch => self.switchStatement(),
        .kw_with => self.withStatement(),
        .kw_throw => self.throwStatement(),
        // TODO: right now, if expression parsing fails, the error message we get is:
        // "Expected an expression, found '<token>'."
        // This error message should be improved.
        else => self.labeledOrExpressionStatement(),
    };
}

fn labeledOrExpressionStatement(self: *Self) Error!Node.Index {
    const cur = self.current_token.tag;
    if ((cur == .identifier or self.isKeywordIdentifier(cur)) and
        (try self.lookAhead()).tag == .@":")
    {
        return self.labeledStatement();
    }

    return self.expressionStatement();
}

fn labeledStatement(self: *Self) Error!Node.Index {
    const label = try self.next();
    _ = try self.expect(.@":");

    const ctx = self.context;
    defer self.context = ctx;

    self.context.@"break" = true;

    const body = try self.labeledItem();
    const end_pos = self.nodes.items(.end)[@intFromEnum(body)];

    return self.addNode(
        .{
            .labeled_statement = .{
                .body = body,
                .label = try self.addToken(label),
            },
        },
        label.start,
        end_pos,
    );
}

fn labeledItem(self: *Self) Error!Node.Index {
    if (self.isAtToken(.kw_function)) {
        const fn_token = try self.next();
        return self.functionDeclaration(fn_token.start, .{});
    }
    return self.statement();
}

/// Declaration:
///     FunctionDeclaration
///     AsyncFunctionDeclaration
///     GeneratorDeclaration
///     AsyncGeneratorDeclaration
///     ClassDeclaration
///     LexicalDeclaration
fn declarationStatement(self: *Self) Error!?Node.Index {
    switch (self.current_token.tag) {
        .kw_class => return try self.classDeclaration(),
        .kw_function => {
            const fn_token = try self.next();
            return try self.functionDeclaration(fn_token.start, .{});
        },
        .kw_async => {
            if ((try self.lookAhead()).tag == .kw_function) {
                const async_token = try self.next(); // eat 'async'
                _ = try self.next(); // eat 'function'
                return try self.functionDeclaration(async_token.start, .{ .is_async = true });
            }
            return null;
        },
        .kw_let => {
            const let_kw = try self.startLetBinding() orelse return null;
            return try self.variableStatement(let_kw);
        },
        .kw_const => return try self.variableStatement(try self.next()),
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-class-definitions
fn classDeclaration(self: *Self) Error!Node.Index {
    const class_kw = try self.next();
    std.debug.assert(class_kw.tag == .kw_class);

    const name_token = try self.next();
    if (name_token.tag != .identifier and !self.isKeywordIdentifier(name_token.tag)) {
        try self.emitBadTokenDiagnostic("a class name", &name_token);
        return Error.UnexpectedToken;
    }

    const name = try self.identifier(name_token);

    const super_class = if (self.current_token.tag == .kw_extends)
        try self.classHeritage()
    else
        Node.Index.empty;

    const class_body = try self.classBody();
    const rb = try self.expect(.@"}");

    const info = try self.addExtraData(ast.ExtraData{
        .class = .{
            .name = name,
            .super_class = super_class,
        },
    });

    return try self.addNode(
        .{ .class_declaration = .{ .class_information = info, .body = class_body } },
        class_kw.start,
        rb.start + rb.len,
    );
}

fn classExpression(self: *Self, class_kw: Token) Error!Node.Index {
    const name = blk: {
        const cur = self.current_token.tag;
        if (cur == .identifier or self.isKeywordIdentifier(cur)) {
            const name_token = try self.next();
            break :blk try self.identifier(name_token);
        }

        break :blk null;
    };

    const super_class = if (self.isAtToken(.kw_extends))
        try self.classHeritage()
    else
        Node.Index.empty;

    const class_body = try self.classBody();
    const rb = try self.expect(.@"}");

    const info = try self.addExtraData(ast.ExtraData{
        .class = .{
            .name = name,
            .super_class = super_class,
        },
    });

    return try self.addNode(
        .{ .class_expression = .{ .class_information = info, .body = class_body } },
        class_kw.start,
        rb.start + rb.len,
    );
}

fn classHeritage(self: *Self) Error!Node.Index {
    _ = try self.expect(.kw_extends);
    const super_class = try self.lhsExpression();
    return super_class;
}

fn classBody(self: *Self) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    _ = try self.expect(.@"{");

    var n_constructors: usize = 0;
    while (self.current_token.tag != .@"}") {
        if (self.current_token.tag == .@";") {
            _ = try self.next(); // eat ';'
            continue;
        }

        var saw_constructor = false;
        const element = try self.classElement(&saw_constructor);
        if (saw_constructor) n_constructors += 1;

        if (n_constructors > 1) {
            try self.emitDiagnosticOnNode(
                element,
                "A class cannot have multiple constructor implementations",
            );
            return Error.MultipleConstructors;
        }
        try self.scratch.append(element);
    }

    const elements = self.scratch.items[prev_scratch_len..];
    return try self.addSubRange(elements);
}

// `saw_constructor` is an in-out parameter that is set to true if a constructor was seen.
fn classElement(self: *Self, saw_constructor_inout: *bool) Error!Node.Index {
    switch (self.current_token.tag) {
        .kw_constructor => {
            saw_constructor_inout.* = true;
            const ctor_key = try self.identifier(try self.next());
            return self.parseClassMethodBody(
                ctor_key,
                .{ .is_static = false, .kind = .constructor },
                .{},
            );
        },
        else => return self.classPropertyWithModifier(.{}, .{}),
    }
}

/// Parse a class property definition that has 0 or more modifiers like 'async', 'static', etc.
/// This function is called when the parser is at the start of a class element, i.e the first modifier token
/// (or name token, if the field has no modifiers).
fn classPropertyWithModifier(self: *Self, flags: ast.ClassFieldFlags, fn_flags: ast.FunctionFlags) Error!Node.Index {
    switch (self.current_token.tag) {
        .kw_async => {
            const async_token = try self.next();
            if ((canStartClassElementName(&self.current_token) or
                self.current_token.tag == .@"*") and
                self.current_token.line == async_token.line)
            {
                if (fn_flags.is_async) {
                    try self.emitBadTokenDiagnostic("Duplicate 'async' modifier", &async_token);
                    return Error.UnexpectedToken;
                }

                return self.classPropertyWithModifier(
                    flags,
                    .{
                        .is_async = true,
                        .is_generator = fn_flags.is_generator,
                        .is_arrow = false,
                    },
                );
            } else {
                const key = try self.identifier(async_token);
                return self.completeClassFieldDef(key, flags, fn_flags);
            }
        },

        .kw_static => {
            const static_token = try self.next();
            if ((canStartClassElementName(&self.current_token) or
                self.current_token.tag == .@"*") and
                self.current_token.line == static_token.line)
            {
                if (flags.is_static) {
                    try self.emitDiagnosticOnToken(
                        static_token,
                        "Duplicate 'static' modifier",
                        .{},
                    );
                    return Error.UnexpectedToken;
                }

                if (fn_flags.is_async) {
                    try self.emitDiagnosticOnToken(
                        static_token,
                        "'static' modifier must precede 'async' modifier",
                        .{},
                    );
                    return Error.UnexpectedToken;
                }

                return self.classPropertyWithModifier(
                    .{
                        .is_static = true,
                        .is_computed = flags.is_computed,
                        .kind = flags.kind,
                    },
                    fn_flags,
                );
            } else {
                const key = try self.identifier(static_token);
                return self.completeClassFieldDef(key, flags, fn_flags);
            }
        },

        .@"*" => {
            _ = try self.next();

            if (!canStartClassElementName(&self.current_token)) {
                try self.emitBadTokenDiagnostic(
                    "a method name after '*'",
                    &self.current_token,
                );
                return Error.UnexpectedToken;
            }

            const key = try self.classElementName();
            return self.parseClassMethodBody(key, flags, .{
                .is_generator = true,
                .is_async = fn_flags.is_async,
                .is_arrow = false,
            });
        },

        else => {
            // Check for 'get' or 'set'
            const kind: ast.ClassFieldKind = blk: {
                const token_str = self.current_token.toByteSlice(self.source);
                if (std.mem.eql(u8, token_str, "get"))
                    break :blk .get;
                if (std.mem.eql(u8, token_str, "set"))
                    break :blk .set;
                break :blk .init;
            };

            if (kind == .get or kind == .set) {
                const get_or_set = try self.next(); // eat 'get' or 'set'
                if (canStartClassElementName(&self.current_token)) {
                    // Getter or Setter.
                    const key = try self.classElementName();
                    var field_flags = flags;
                    field_flags.kind = kind;
                    std.debug.assert(!fn_flags.is_generator);
                    return self.parseClassMethodBody(key, field_flags, fn_flags);
                }

                // Probably a regular field named 'get' or 'set',
                // like `class A { get() { return 1 }; set = 2;  } `
                const key = try self.identifier(get_or_set);
                return self.completeClassFieldDef(key, flags, fn_flags);
            }

            const key = try self.classElementName();
            return self.completeClassFieldDef(key, flags, fn_flags);
        },
    }
}

/// Assuming that the key has been parsed, complete the property definition.
fn completeClassFieldDef(
    self: *Self,
    key: Node.Index,
    flags: ast.ClassFieldFlags,
    fn_flags: ast.FunctionFlags,
) Error!Node.Index {
    if (self.current_token.tag == .@"(") {
        self.current_destructure_kind.setNoAssignOrDestruct();
        return self.parseClassMethodBody(key, flags, fn_flags);
    }

    if (fn_flags.is_async) {
        // disallow fields like `async x = 5;`
        try self.emitDiagnosticOnNode(
            key,
            "'async' modifier cannot be used on fields that are not methods",
        );
        return Error.IllegalModifier;
    }

    if (self.current_token.tag == .@"=") {
        _ = try self.next();
        const value = try self.assignExpressionNoPattern();
        const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
        var end_pos = self.nodes.items(.end)[@intFromEnum(value)];
        end_pos = try self.semiColon(end_pos);
        const kv_node = ast.ClassFieldDefinition{
            .key = key,
            .value = value,
            .flags = flags,
        };
        return self.addNode(.{ .class_field = kv_node }, start_pos, end_pos);
    }

    var end_pos = self.nodes.items(.end)[@intFromEnum(key)];
    end_pos = try self.semiColon(end_pos);
    const kv_node = ast.ClassFieldDefinition{
        .key = key,
        .value = key,
        .flags = ast.ClassFieldFlags{
            .kind = .init,
            .is_computed = flags.is_computed,
            .is_static = flags.is_static,
        },
    };

    const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
    return self.addNode(.{ .class_field = kv_node }, start_pos, end_pos);
}

/// Parse the method body of a class, assuming we're at the '(' token.
/// Returns a `class_method` Node.
fn parseClassMethodBody(
    self: *Self,
    key: Node.Index,
    flags: ast.ClassFieldFlags,
    fn_flags: ast.FunctionFlags,
) Error!Node.Index {
    std.debug.assert(self.current_token.tag == .@"(");

    const start_pos = self.current_token.start;
    const func_expr = try self.parseFunctionBody(
        start_pos,
        null,
        fn_flags,
        false,
    );

    const end_pos = self.nodes.items(.end)[@intFromEnum(func_expr)];
    const kv_node = ast.ClassFieldDefinition{
        .key = key,
        .value = func_expr,
        .flags = flags,
    };

    if (flags.kind == .get or flags.kind == .set) {
        if (fn_flags.is_async) {
            try self.emitDiagnosticOnNode(
                key,
                "'async' modifier cannot be used on getters or setters",
            );

            return Error.IllegalModifier;
        }

        // verify the number of parameters for getters and setters.
        // (the if expression looks stupid but is necessary because those are different enum types with same member names)
        try self.checkGetterOrSetterParams(
            func_expr,
            if (flags.kind == .get) .get else .set,
        );
    }

    const key_start = self.nodes.items(.start)[@intFromEnum(key)];
    return self.addNode(
        .{ .class_method = kv_node },
        key_start,
        end_pos,
    );
}

/// Return any statement except a labeled statement.
/// Used in contexts where a labeled statement is not allowed, like the body of
/// a WhileStatement.
fn stmtNotLabeledFunction(self: *Self) Error!Node.Index {
    const lstmt = try self.statementOrDeclaration();
    const data_slice: []ast.NodeData = self.nodes.items(.data);
    const stmt_node = data_slice[@intFromEnum(lstmt)];

    switch (stmt_node) {
        .labeled_statement => |labeled_stmt| {
            // TODO: this should change to a helper function.
            const stmt = data_slice[@intFromEnum(labeled_stmt.body)];
            if (std.meta.activeTag(stmt) == .function_declaration) {
                try self.emitDiagnosticOnToken(
                    self.getToken(labeled_stmt.label),
                    "Function declarations cannot be labeled here",
                    .{},
                );
                return Error.IllegalLabeledStatement;
            }
        },
        else => {},
    }

    return lstmt;
}

fn ifStatement(self: *Self) Error!Node.Index {
    const if_kw = try self.next();
    std.debug.assert(if_kw.tag == .kw_if);

    _ = try self.expect(.@"(");
    const cond = try self.expression();
    _ = try self.expect(.@")");

    const consequent = try self.stmtNotLabeledFunction();
    var end_pos = self.nodeSpan(consequent).end;

    var alternate = Node.Index.empty;
    if (self.peek().tag == .kw_else) {
        _ = try self.next();
        alternate = try self.stmtNotLabeledFunction();
        end_pos = self.nodeSpan(alternate).end;
    }

    return self.addNode(
        .{
            .if_statement = .{
                .condition = cond,
                .consequent = consequent,
                .alternate = alternate,
            },
        },
        if_kw.start,
        end_pos,
    );
}

const ForLoopKind = enum {
    for_in,
    for_of,
    basic,
};

/// Parse a for loop. Returns an index of:
/// ForOfStatement / ForInStatement / ForStatement
fn forStatement(self: *Self) Error!Node.Index {
    const for_kw = try self.next();
    std.debug.assert(for_kw.tag == .kw_for);

    const loop_kind, const iterator = try self.forLoopIterator();

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.statement();
    const start_pos = for_kw.start;
    const end_pos = self.nodes.items(.end)[@intFromEnum(body)];

    const for_payload = ast.ForStatement{
        .body = body,
        .iterator = iterator,
    };

    const node_data: ast.NodeData = switch (loop_kind) {
        .basic => .{ .for_statement = for_payload },
        .for_of => .{ .for_of_statement = for_payload },
        .for_in => .{ .for_in_statement = for_payload },
    };

    return self.addNode(node_data, start_pos, end_pos);
}

/// Once a 'for' keyword has been consumed, this parses the part inside '()', including the parentheses.
/// Returns a tuple where the first item is the kind of for loop (for-in, for-of, or basic),
/// and the second item is the iterator (index of an `ast.ExtraData`).
fn forLoopIterator(self: *Self) Error!struct { ForLoopKind, ast.ExtraData.Index } {
    _ = try self.expect(.@"(");

    // Check for productions that start with a variable declarator.
    // for (var/let/const x = 0; x < 10; x++)
    // for (var/let/const x in y)
    // for (var/let/const x of y)
    const maybe_decl_kw = blk: {
        const curr = self.current_token.tag;
        if (curr == .kw_var or curr == .kw_const) {
            break :blk try self.next();
        }

        if (curr == .kw_let) {
            break :blk try self.startLetBinding();
        }

        break :blk null;
    };

    if (maybe_decl_kw) |decl_kw| {
        var loop_kind = ForLoopKind.basic;
        const iterator = try self.completeVarDeclLoopIterator(decl_kw, &loop_kind);
        return .{ loop_kind, iterator };
    }

    // 'for (;' indicates a basic 3-part for loop with empty initializer.
    if (self.isAtToken(.@";")) {
        const iterator = try self.completeBasicLoopIterator(Node.Index.empty);
        return .{ ForLoopKind.basic, iterator };
    }

    const context = self.context;
    defer self.context = context;

    // disallow `in` expressions.
    // The next `in` token is instead the iterator of the for loop.
    self.context.in = false;

    // for (<LeftHandSideExpression> <in/of> <Expression>)
    // for (<Expression> <in/of> <Expression>)
    // for (<Expression>;<Expression>;<Expression>);
    const expr = try self.forLoopStartExpression();

    // `in` expressions are allowed now.
    self.context.in = true;

    switch (self.current_token.tag) {
        .@";" => {
            // parse <Expression> ; <Expression> ; <Expression>
            if (self.current_destructure_kind.must_destruct) {
                const expr_start = self.nodes.items(.start)[@intFromEnum(expr)];
                try self.emitDiagnostic(
                    util.offsets.byteIndexToCoordinate(self.source, expr_start),
                    "Unexpected pattern",
                    .{},
                );

                return Error.UnexpectedPattern;
            }

            const iterator = try self.completeBasicLoopIterator(expr);
            return .{ ForLoopKind.basic, iterator };
        },

        .kw_of, .kw_in => {
            // parse <Expression> <in/of> <Expression>
            if (!self.current_destructure_kind.can_be_assigned_to) {
                try self.emitDiagnostic(
                    // TODO: use the expression's start coordinate instead, use util.offsets
                    self.current_token.startCoord(self.source),
                    "left hand side of for-loop must be a name or assignment pattern",
                    .{},
                );
                return Error.InvalidLoopLhs;
            }

            self.reinterpretAsPattern(expr);

            const loop_kind = if (self.isAtToken(.kw_of))
                ForLoopKind.for_of
            else
                ForLoopKind.for_in;

            _ = try self.next();
            const rhs = try self.expression();
            _ = try self.expect(.@")");

            const iterator = try self.addExtraData(.{
                .for_in_of_iterator = .{
                    .left = expr,
                    .right = rhs,
                },
            });
            return .{ loop_kind, iterator };
        },

        else => {
            try self.emitBadTokenDiagnostic("'of', 'in' or ';'", &self.current_token);
            return Error.UnexpectedToken;
        },
    }
}

/// Parse the first expression that can appear in a for-loop iterator after
/// the opening parenthesis.
/// This expression may be followed a ';' token (indicating a 3-step for loop),
/// or by 'in' or 'of' (indicating a for-in or for-of loop).
fn forLoopStartExpression(self: *Self) Error!Node.Index {
    const first_expr = try self.assignmentExpression();

    if (self.isAtToken(.@",") and !self.current_destructure_kind.must_destruct) {
        return self.completeSequenceExpr(first_expr);
    }

    return first_expr;
}

/// Once the opening '(' and a 'var'/'let'/'const' keyword has been consumed,
/// this function parses the rest of the loop iterator.
fn completeVarDeclLoopIterator(self: *Self, decl_kw: Token, loop_kind: *ForLoopKind) Error!ast.ExtraData.Index {
    std.debug.assert(decl_kw.tag == .kw_let or
        decl_kw.tag == .kw_var or
        decl_kw.tag == .kw_const);

    const lhs = try self.assignmentLhsExpr();
    const lhs_span = self.nodeSpan(lhs);
    switch (self.current_token.tag) {
        .kw_in, .kw_of => {
            loop_kind.* = if (self.current_token.tag == .kw_in)
                ForLoopKind.for_in
            else
                ForLoopKind.for_of;

            const declarator = try self.addNode(
                .{ .variable_declarator = .{ .lhs = lhs, .init = null } },
                lhs_span.start,
                lhs_span.end,
            );

            const decl_range = try self.addSubRange(&[_]Node.Index{declarator});
            const declaration = try self.addNode(
                .{ .variable_declaration = .{ .kind = varDeclKind(decl_kw.tag), .declarators = decl_range } },
                decl_kw.start,
                lhs_span.end,
            );

            _ = try self.next(); // eat 'in' or 'of'
            const rhs = try self.expression();
            _ = try self.expect(.@")");
            return self.addExtraData(.{
                .for_in_of_iterator = .{ .left = declaration, .right = rhs },
            });
        },
        else => {
            var end_pos = lhs_span.end;
            var rhs: ?Node.Index = null;
            if (self.isAtToken(.@"=")) {
                _ = try self.next();
                const init_expr = try self.assignmentExpression();

                if (self.current_destructure_kind.must_destruct) {
                    try self.emitBadDestructureDiagnostic(init_expr);
                    return Error.UnexpectedPattern;
                }

                end_pos = self.nodeSpan(init_expr).end;
                rhs = init_expr;
            }

            const first_decl = try self.addNode(
                .{ .variable_declarator = .{ .lhs = lhs, .init = rhs } },
                decl_kw.start,
                end_pos,
            );

            const for_init = try self.completeLoopInitializer(decl_kw, first_decl);
            return self.completeBasicLoopIterator(for_init);
        },
    }
}

/// A "Basic" loop iterator is a plain old (init; cond; update).
/// Given the `init` expression or statement, this function parses the rest of the iterator
/// upto the closing ')'.
fn completeBasicLoopIterator(self: *Self, for_init: Node.Index) Error!ast.ExtraData.Index {
    _ = try self.expect(.@";");

    const for_cond = switch (self.current_token.tag) {
        .@";" => Node.Index.empty,
        else => try self.expression(),
    };

    _ = try self.expect(.@";");

    const for_update = switch (self.current_token.tag) {
        .@")" => Node.Index.empty,
        else => try self.expression(),
    };

    _ = try self.expect(.@")");

    return self.addExtraData(ast.ExtraData{
        .for_iterator = ast.ForIterator{
            .init = for_init,
            .condition = for_cond,
            .update = for_update,
        },
    });
}

/// Parse the rest of the for loop initializer.
/// Example:
/// ```js
/// for (let i = 0, j = 0; i < 10; i++,j++);
/// //           ^
/// // already parsed upto here, this function parses the rest
/// ```
/// Given the 'var'/'let'/'const' keyword,
/// and the first declarator, parse the rest of the declarators (if any)
/// and return a var decl node that represents a loop initializer.
fn completeLoopInitializer(self: *Self, kw: Token, first_decl: Node.Index) Error!Node.Index {
    var decl_nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer decl_nodes.deinit();

    try decl_nodes.append(first_decl);

    while (self.current_token.tag == .@",") {
        _ = try self.next();
        const decl = try self.variableDeclarator();
        try decl_nodes.append(decl);
    }

    const decls = try self.addSubRange(decl_nodes.items);

    const last_decl = decl_nodes.items[decl_nodes.items.len - 1];
    const end_pos = self.nodes.items(.end)[@intFromEnum(last_decl)];

    return self.addNode(
        .{
            .variable_declaration = .{
                .kind = varDeclKind(kw.tag),
                .declarators = decls,
            },
        },
        kw.start,
        end_pos,
    );
}

fn whileStatement(self: *Self) Error!Node.Index {
    const while_kw = try self.next();
    std.debug.assert(while_kw.tag == .kw_while);

    _ = try self.expect(.@"(");
    const cond = try self.expression();
    _ = try self.expect(.@")");

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.stmtNotLabeledFunction();
    const end_pos = self.nodeSpan(body).end;

    return self.addNode(
        .{ .while_statement = .{ .condition = cond, .body = body } },
        while_kw.start,
        end_pos,
    );
}

fn doWhileStatement(self: *Self) Error!Node.Index {
    const do_kw = try self.next();
    std.debug.assert(do_kw.tag == .kw_do);

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    // TODO: Do not allow declarations or labeled functions here.
    const body = try self.statementOrDeclaration();
    _ = try self.expect(.kw_while);
    _ = try self.expect(.@"(");
    const cond = try self.expression();
    const rb = try self.expect(.@")");

    var end_pos = rb.start + rb.len;

    if (self.isAtToken(.@";")) {
        const semi = try self.next();
        end_pos = semi.start + semi.len;
    }

    return self.addNode(
        .{ .do_while_statement = .{ .condition = cond, .body = body } },
        do_kw.start,
        end_pos,
    );
}

/// DebuggerStatement: 'debugger' ';'
fn debuggerStatement(self: *Self) Error!Node.Index {
    const token = try self.next();
    const end_pos = try self.semiColon(token.start + token.len);
    return self.addNode(
        .{ .debugger_statement = {} },
        token.start,
        end_pos,
    );
}

/// Parse a VariableStatement, where `kw` is the keyword used to declare the variable (let, var, or const).
fn variableStatement(self: *Self, kw: Token) Error!Node.Index {
    std.debug.assert(kw.tag == .kw_let or kw.tag == .kw_var or kw.tag == .kw_const);

    const decls = try self.variableDeclaratorList();
    const last_decl = decls.asSlice(self)[0];

    var end_pos: u32 = self.nodes.items(.end)[@intFromEnum(last_decl)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(
        .{
            .variable_declaration = .{
                .kind = varDeclKind(kw.tag),
                .declarators = decls,
            },
        },
        kw.start,
        end_pos,
    );
}

/// Parse a list of variable declarators after the 'var', 'let', or 'const'
/// keyword has been eaten.
fn variableDeclaratorList(self: *Self) Error!ast.SubRange {
    var declarators = std.ArrayList(Node.Index).init(self.allocator);
    defer declarators.deinit();

    while (true) {
        const decl = try self.variableDeclarator();
        try declarators.append(decl);
        if (self.current_token.tag == .@",") {
            _ = try self.next();
        } else {
            break;
        }
    }

    const decls = try self.addSubRange(declarators.items);
    return decls;
}

/// Return the kind of variable declaration based on the keyword used to declare.
fn varDeclKind(tag: Token.Tag) ast.VarDeclKind {
    return switch (tag) {
        .kw_let => .let,
        .kw_var => .@"var",
        .kw_const => .@"const",
        else => unreachable,
    };
}

/// Parse a 'try'..'catch'..'finally' statement.
fn tryStatement(self: *Self) Error!Node.Index {
    const try_kw = try self.next();
    const start_pos = try_kw.start;
    std.debug.assert(try_kw.tag == .kw_try);

    const body = try self.blockStatement();

    var end_pos = self.nodes.items(.end)[@intFromEnum(body)];
    var catch_clause = Node.Index.empty;
    var finalizer = Node.Index.empty;

    switch (self.current_token.tag) {
        .kw_catch => {
            catch_clause = try self.catchClause();
            if (self.isAtToken(.kw_finally)) {
                finalizer = try self.finallyBlock();
                end_pos = self.nodes.items(.end)[@intFromEnum(finalizer)];
            } else {
                end_pos = self.nodes.items(.end)[@intFromEnum(catch_clause)];
            }
        },

        .kw_finally => {
            finalizer = try self.finallyBlock();
            end_pos = self.nodes.items(.end)[@intFromEnum(finalizer)];
        },

        else => {
            try self.emitBadTokenDiagnostic("'catch' or 'finally'", &self.current_token);
            return Error.UnexpectedToken;
        },
    }

    return self.addNode(.{
        .try_statement = ast.TryStatement{
            .finalizer = finalizer,
            .catch_clause = catch_clause,
            .body = body,
        },
    }, start_pos, end_pos);
}

fn catchClause(self: *Self) Error!Node.Index {
    const catch_kw = try self.expect(.kw_catch);
    const param = blk: {
        if (self.isAtToken(.@"(")) {
            _ = try self.next();
            const binding = try self.assignmentLhsExpr();
            _ = try self.expect(.@")");
            break :blk binding;
        }

        break :blk null;
    };

    const body = try self.blockStatement();
    return self.addNode(.{
        .catch_clause = ast.CatchClause{
            .param = param,
            .body = body,
        },
    }, catch_kw.start, self.nodeSpan(body).end);
}

fn finallyBlock(self: *Self) Error!Node.Index {
    const finally_kw = try self.next();
    std.debug.assert(finally_kw.tag == .kw_finally);
    const body = try self.blockStatement();
    return body;
}

fn switchStatement(self: *Self) Error!Node.Index {
    const switch_kw = try self.next();
    std.debug.assert(switch_kw.tag == .kw_switch);

    _ = try self.expect(.@"(");
    const discriminant = try self.expression();
    _ = try self.expect(.@")");

    _ = try self.expect(.@"{");

    // allow break statements inside switch-case.
    const ctx = self.context;
    defer self.context = ctx;
    self.context.@"break" = true;

    const cases = try self.caseBlock();
    const rbrace = try self.expect(.@"}");
    const end_pos = rbrace.start + rbrace.len;

    return self.addNode(
        .{ .switch_statement = .{ .discriminant = discriminant, .cases = cases } },
        switch_kw.start,
        end_pos,
    );
}

// Parse the all cases inside the body of a switch statement.
// Does not consume the opening '{' and closing '}',
// assumes the opening '{' has already been consumed.
fn caseBlock(self: *Self) Error!ast.SubRange {

    // restore the scratch space after parsing the case-block.
    const scratch_prev_len = self.scratch.items.len;
    defer self.scratch.items.len = scratch_prev_len;

    var saw_default = false;
    while (self.current_token.tag != .@"}") {
        const case_node = blk: {
            switch (self.current_token.tag) {
                .kw_case => break :blk try self.caseClause(),
                .kw_default => {
                    if (saw_default) {
                        try self.emitDiagnostic(
                            self.current_token.startCoord(self.source),
                            "Multiple 'default' clauses are not allowed in a switch statement",
                            .{},
                        );
                        return Error.MultipleDefaults;
                    }

                    saw_default = true;
                    break :blk try self.defaultCase();
                },
                else => {
                    try self.emitBadTokenDiagnostic("'case' or 'default'", &self.current_token);
                    return Error.UnexpectedToken;
                },
            }
        };

        try self.scratch.append(case_node);
    }

    const cases = try self.addSubRange(self.scratch.items[scratch_prev_len..]);

    return cases;
}

/// Parse a single case clause inside a switch statement.
/// A case clause begins with the 'case' (or 'default')  keyword:
/// 'case' Expression ':' StatementList
fn caseClause(self: *Self) Error!Node.Index {
    const case_kw = try self.next();
    std.debug.assert(case_kw.tag == .kw_case);

    const test_expr = try self.expression();

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    _ = try self.expect(.@":");
    while (self.current_token.tag != .@"}" and
        self.current_token.tag != .kw_case and
        self.current_token.tag != .kw_default)
    {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(stmt);
    }

    const consequent_stmts = self.scratch.items[prev_scratch_len..];

    const end_pos =
        if (consequent_stmts.len > 0)
        self.nodes.items(.end)[@intFromEnum(consequent_stmts[consequent_stmts.len - 1])]
    else
        case_kw.start + case_kw.len;

    const case_node = try self.addNode(
        .{
            .switch_case = ast.SwitchCase{
                .expression = test_expr,
                .consequent = try self.addSubRange(consequent_stmts),
            },
        },
        case_kw.start,
        end_pos,
    );

    return case_node;
}

/// Parse the 'default' case inside a switch statement.
fn defaultCase(self: *Self) Error!Node.Index {
    const default_kw = try self.next();
    std.debug.assert(default_kw.tag == .kw_default);

    _ = try self.expect(.@":");

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    var cur = self.current_token.tag;
    while (cur != .@"}" and cur != .kw_case and cur != .kw_default) : (cur = self.current_token.tag) {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(stmt);
    }

    const consequent_stmts = self.scratch.items[prev_scratch_len..];
    const end_pos =
        if (consequent_stmts.len > 0)
        self.nodes.items(.end)[@intFromEnum(consequent_stmts[consequent_stmts.len - 1])]
    else
        default_kw.start + default_kw.len;

    return self.addNode(
        .{
            .default_case = ast.SwitchDefaultCase{
                .consequent = try self.addSubRange(consequent_stmts),
            },
        },
        default_kw.start,
        end_pos,
    );
}

fn withStatement(self: *Self) Error!Node.Index {
    const with_kw = try self.next();
    std.debug.assert(with_kw.tag == .kw_with);

    _ = try self.expect(.@"(");
    const obj = try self.expression();
    _ = try self.expect(.@")");

    const body = try self.stmtNotLabeledFunction();
    const end_pos = self.nodeSpan(body).end;

    const stmt = try self.addNode(
        .{ .with_statement = .{ .object = obj, .body = body } },
        with_kw.start,
        end_pos,
    );

    if (self.context.strict) {
        // TODO: emit this error but continue parsing.
        try self.emitDiagnosticOnNode(stmt, "With statements are not allowed in strict mode");
        return Error.WithInStrictMode;
    }

    return stmt;
}

fn throwStatement(self: *Self) Error!Node.Index {
    const throw_kw = try self.next();
    std.debug.assert(throw_kw.tag == .kw_throw);

    if (self.current_token.line != throw_kw.line) {
        try self.emitDiagnostic(
            throw_kw.startCoord(self.source),
            "Illegal newline after 'throw'",
            .{},
        );
        return Error.IllegalNewline;
    }

    const expr = try self.expression();
    var end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(
        .{ .throw_statement = expr },
        throw_kw.start,
        end_pos,
    );
}

/// Parse a statement that starts with the `let` keyword.
/// This may not necessarily be a variable declaration, as `let`
/// is also a valid identifier.
fn letStatement(self: *Self) Error!Node.Index {
    std.debug.assert(self.current_token.tag == .kw_let);
    const let_kw = try self.startLetBinding() orelse {
        // If the `let` keyword doesn't start an identifier,
        // then its an identifier.
        if ((try self.lookAhead()).tag == .@":") {
            // This is a labeled statement: 'let: ...'
            return self.labeledStatement();
        }

        return self.expressionStatement();
    };

    if (self.isInStrictMode()) {
        // TODO: disallow 'let' to be bound lexically.
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "'let' cannot be used as a variable name in strict mode",
            .{},
        );
        return Error.LetInStrictMode;
    }

    return self.variableStatement(let_kw);
}

/// Checks if the parser is at the beginning of let binding,
/// consumes the `let` token if so, and then returns it.
/// Otherwise, it returns `null` and consumes nothing.
///
/// Must be called when `self.current_token` is a 'let' keyword.
fn startLetBinding(self: *Self) Error!?Token {
    std.debug.assert(self.current_token.tag == .kw_let);

    const lookahead = try self.lookAhead();
    if (self.isDeclaratorStart(lookahead.tag)) {
        // TODO: disallow `let let = ...`, `const [let] = ...` etc.
        return try self.next();
    }

    return null;
}

/// VariableDeclarator:
///  BindingPattern Initializer?
///  BindingElement Initializer?
fn variableDeclarator(self: *Self) Error!Node.Index {
    const lhs = try self.assignmentLhsExpr();
    if (!self.current_destructure_kind.can_destruct) {
        // TODO: improve error message
        try self.emitDiagnosticOnNode(lhs, "Invalid left-hand-side for variable declaration.");
        return Error.UnexpectedPattern;
    }

    const lhs_span = self.nodeSpan(lhs);
    const start_pos = lhs_span.start;
    var end_pos = lhs_span.end;

    const rhs: ?Node.Index = blk: {
        if (self.isAtToken(.@"=")) {
            _ = try self.next();
            const init_expr = try self.assignExpressionNoPattern();
            end_pos = self.nodeSpan(init_expr).end;
            break :blk init_expr;
        } else if (self.nodeTag(lhs) != .identifier) {
            // Assignment patterns must have an initializer.
            try self.emitDiagnosticOnNode(lhs, "A destructuring declaration must have an initializer");
            return Error.MissingInitializer;
        }

        break :blk null;
    };

    return self.addNode(
        .{ .variable_declarator = .{ .lhs = lhs, .init = rhs } },
        start_pos,
        end_pos,
    );
}

/// EmptyStatement: ';'
fn emptyStatement(self: *Self) Error!Node.Index {
    const semicolon = try self.next();
    std.debug.assert(semicolon.tag == .@";");

    return addNode(
        self,
        .{ .empty_statement = {} },
        semicolon.start,
        semicolon.start + semicolon.len,
    );
}

/// ExpressionStatement: Expression ';'
fn expressionStatement(self: *Self) Error!Node.Index {
    const expr = try self.expression();
    const expr_node = self.getNode(expr);

    const end_pos = try self.semiColon(expr_node.end);

    return addNode(
        self,
        .{ .expression_statement = expr },
        expr_node.start,
        end_pos,
    );
}

/// BlockStatement:
///   '{' StatementList? '}'
fn blockStatement(self: *Self) Error!Node.Index {
    const lbrac = try self.expect(.@"{");
    const start_pos = lbrac.start;

    var statements = std.ArrayList(Node.Index).init(self.allocator);
    defer statements.deinit();

    while (self.current_token.tag != .@"}") {
        const stmt = try self.statementOrDeclaration();
        try statements.append(stmt);
    }

    const rbrace = try self.next();
    std.debug.assert(rbrace.tag == .@"}");
    const end_pos = rbrace.start + rbrace.len;

    if (statements.items.len == 0) {
        return self.addNode(.{ .block_statement = null }, start_pos, end_pos);
    }

    const stmt_list_node = try self.addSubRange(statements.items);
    const block_node = ast.NodeData{ .block_statement = stmt_list_node };
    return self.addNode(block_node, start_pos, end_pos);
}

/// Assuming the parser is at the `function` keyword,
/// parse a function declaration statement.
fn functionDeclaration(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.next();
        fn_flags.is_generator = true;
    }

    const name_token = blk: {
        const token = try self.next();
        if (token.tag == .identifier or self.isKeywordIdentifier(token.tag)) {
            break :blk try self.addToken(token);
        }

        try self.emitBadTokenDiagnostic("function name", &token);
        return Error.UnexpectedToken;
    };

    return self.parseFunctionBody(start_pos, name_token, fn_flags, true);
}

/// ReturnStatement:
///    'return' ';'
///    'return' [no LineTerminator here] Expression? ';'
fn returnStatement(self: *Self) Error!Node.Index {
    const return_kw = try self.next();
    std.debug.assert(return_kw.tag == .kw_return);

    if (!self.context.@"return") {
        // todo: maybe we should just emit a diagnostic here and continue parsing?
        // that way we can catch more parse errors than just this.
        try self.emitDiagnostic(
            return_kw.startCoord(self.source),
            "Return statement is not allowed outside of a function",
            .{},
        );
        return Error.IllegalReturn;
    }

    if (self.current_token.line != return_kw.line or
        self.current_token.tag == .@";" or
        self.current_token.tag == .@"}")
    {
        const end_pos = try self.semiColon(return_kw.start + return_kw.len);
        return self.addNode(.{ .return_statement = null }, return_kw.start, end_pos);
    }

    const operand = try self.expression();
    const end_pos = try self.semiColon(self.nodeSpan(operand).end);
    return self.addNode(.{ .return_statement = operand }, return_kw.start, end_pos);
}

/// BreakStatement: 'break' ';'
fn breakStatement(self: *Self) Error!Node.Index {
    const break_kw = try self.next();
    std.debug.assert(break_kw.tag == .kw_break);

    if (!self.context.@"break") {
        try self.emitDiagnostic(
            break_kw.startCoord(self.source),
            "'break' is not allowed outside loops and switch statements",
            .{},
        );
        return Error.IllegalBreak;
    }

    const start_pos = break_kw.start;
    var end_pos = break_kw.start + break_kw.len;

    const label = blk: {
        const cur = self.current_token;
        if ((cur.tag == .identifier or self.isKeywordIdentifier(cur.tag)) and
            cur.line == break_kw.line)
        {
            // TODO: check if this label exists.
            const token = try self.next();
            const id = try self.identifier(token);
            end_pos = token.start + token.len;
            break :blk id;
        }

        break :blk null;
    };

    end_pos = try self.semiColon(end_pos);
    return self.addNode(
        .{ .break_statement = .{ .label = label } },
        start_pos,
        end_pos,
    );
}

/// ContinueStatement: 'continue' ';'
fn continueStatement(self: *Self) Error!Node.Index {
    const continue_kw = try self.next();
    std.debug.assert(continue_kw.tag == .kw_continue);

    if (!self.context.@"continue") {
        try self.emitDiagnostic(
            continue_kw.startCoord(self.source),
            "'continue' is not allowed outside loops",
            .{},
        );
        return Error.IllegalContinue;
    }

    const start_pos = continue_kw.start;
    var end_pos = continue_kw.start + continue_kw.len;

    const label = blk: {
        const cur = self.current_token;
        if ((cur.tag == .identifier or self.isKeywordIdentifier(cur.tag)) and
            cur.line == continue_kw.line)
        {
            // TODO: check if this label exists.
            const token = try self.next();
            const id = try self.identifier(token);
            end_pos = token.start + token.len;
            break :blk id;
        }

        break :blk null;
    };

    end_pos = try self.semiColon(end_pos);
    return self.addNode(
        .{ .continue_statement = .{ .label = label } },
        start_pos,
        end_pos,
    );
}

// ------------------------
// Common helper functions
// ------------------------

/// Helper function to finish a statement with a semi-colon.
/// `end_pos` is the end-position of the statement node,
/// if a semi-colon is found, returns the end position of the semi-colon,
/// otherwise returns `end_pos`.
/// If the ASI rules cannot be applied, returns a parse error.
fn semiColon(self: *Self, end_pos: u32) Error!u32 {
    return if (try self.eatSemiAsi()) |semi|
        semi.end
    else
        end_pos;
}

/// Eat a semicolon token and return its span.
/// If there if there is no semi-colon token, it will check if we're allowed to assume an implicit ';' exists
/// https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-automatic-semicolon-insertion
/// If no semi-colon can be inserted, a parse error is returned instead.
fn eatSemiAsi(self: *Self) Error!?types.Span {
    if (self.current_token.tag == .@";") {
        const semicolon = try self.next();
        return .{
            .start = semicolon.start,
            .end = semicolon.start + semicolon.len,
        };
    }

    if (self.current_token.line != self.prev_token_line or
        self.current_token.tag == .@"}" or
        self.current_token.tag == .eof)
        return null;

    try self.emitBadTokenDiagnostic("a ';' or a newline", &self.current_token);
    return Error.ExpectedSemicolon;
}

/// Returns whether a token with the tag `tag` can start a variable declarator
/// ({, or [, or an identifier).
fn isDeclaratorStart(self: *Self, tag: Token.Tag) bool {
    return tag == .@"[" or tag == .@"{" or
        tag == .identifier or self.isKeywordIdentifier(tag);
}

/// Returns whether the parser is currently parsing strict mode code.
fn isInStrictMode(self: *const Self) bool {
    return self.context.strict or self.context.module;
}

/// Returns `true` of `tag` is a keyword that can be allowed as an identifier
/// in the current context.
fn isKeywordIdentifier(self: *const Self, tag: Token.Tag) bool {
    switch (tag) {
        .kw_await => return !self.context.is_await_reserved,
        .kw_yield => return !self.context.is_yield_reserved,
        else => {
            if (tag.isContextualKeyword()) return true;
            if (self.isInStrictMode()) return false;
            // check if tag is a keyword, but that keyword can be used as an identifier in non-strict mode.
            return tag.isStrictModeKeyword() or tag == .kw_let;
        },
    }
}

/// Reserve a slot for node, then return the index of the reserved slot.
fn reserveSlot(self: *Self) Node.Index {
    try self.nodes.append(self.allocator, undefined);
    return @enumFromInt(self.nodes.len - 1);
}

fn addSubRange(self: *Self, nodes: []const Node.Index) error{OutOfMemory}!ast.SubRange {
    const from: ast.SubRange.Index = @enumFromInt(self.node_lists.items.len);
    try self.node_lists.appendSlice(nodes);
    const to: ast.SubRange.Index = @enumFromInt(self.node_lists.items.len);
    return ast.SubRange{ .from = from, .to = to };
}

fn addToken(self: *Self, token: Token) error{OutOfMemory}!Token.Index {
    try self.tokens.append(token);
    return @enumFromInt(self.tokens.items.len - 1);
}

/// Append a node to the flat node list.
fn addNode(self: *Self, node: NodeData, start: u32, end: u32) error{OutOfMemory}!Node.Index {
    try self.nodes.append(
        self.allocator,
        .{ .data = node, .start = start, .end = end },
    );
    return @enumFromInt(self.nodes.len - 1);
}

/// Append an ExtraData item to the list, and return its index.
fn addExtraData(self: *Self, data: ast.ExtraData) error{OutOfMemory}!ast.ExtraData.Index {
    try self.extra_data.append(data);
    return @enumFromInt(self.extra_data.items.len - 1);
}

/// Emit a diagnostic that says a rest parameter must be the last in a function's parameter list.
/// Always returns a parse error.
fn restParamNotLastError(self: *Self, token: *const Token) Error!void {
    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Rest parameter must be the last parameter in a function",
        .{},
    );

    return Error.InvalidFunctionParameter;
}

fn emitBadDestructureDiagnostic(self: *Self, expr: Node.Index) error{OutOfMemory}!void {
    const start_byte_index = self.nodes.items(.start)[@intFromEnum(expr)];
    const start_coord = util.offsets.byteIndexToCoordinate(self.source, start_byte_index);
    return self.emitDiagnostic(start_coord, "Unexpected destructuring pattern", .{});
}

fn emitDiagnosticOnNode(self: *Self, expr: Node.Index, comptime message: []const u8) error{OutOfMemory}!void {
    const start_byte_index = self.nodes.items(.start)[@intFromEnum(expr)];
    const start_coord = util.offsets.byteIndexToCoordinate(self.source, start_byte_index);
    return self.emitDiagnostic(start_coord, message, .{});
}

/// Emit a diagnostic about an un-expected token.
fn emitBadTokenDiagnostic(
    self: *Self,
    comptime expected: []const u8,
    got: *const Token,
) error{OutOfMemory}!void {
    try self.emitDiagnostic(
        got.startCoord(self.source),
        "Expected " ++ expected ++ ", got {s}",
        .{got.toByteSlice(self.source)},
    );
}

/// Push an error essage to the list of diagnostics.
fn emitDiagnostic(
    self: *Self,
    coord: types.Coordinate,
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostics.append(Diagnostic{
        .coord = coord,
        .message = message,
    });
}

/// Push an error essage to the list of diagnostics.
fn emitDiagnosticOnToken(
    self: *Self,
    token: Token,
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{OutOfMemory}!void {
    const message = try std.fmt.allocPrint(self.allocator, fmt, fmt_args);
    try self.diagnostics.append(Diagnostic{
        .coord = token.startCoord(self.source),
        .message = message,
    });
}

/// Emit a parse error if the current token does not match `tag`.
fn expect(self: *Self, tag: Token.Tag) Error!Token {
    if (self.current_token.tag == tag) {
        return try self.next();
    }

    try self.emitDiagnostic(
        self.current_token.startCoord(self.source),
        "Expected a '{s}', but found a '{s}'",
        .{ @tagName(tag), self.current_token.toByteSlice(self.source) },
    );
    return Error.UnexpectedToken;
}

/// Emit a parse error if the current token does not match `tag1` or `tag2`.
fn expect2(self: *Self, tag1: Token.Tag, tag2: Token.Tag) Error!Token {
    const token = try self.next();
    if (token.tag == tag1 or token.tag == tag2) {
        return token;
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Expected a '{s}' or a '{s}', but found a '{s}'",
        .{
            @tagName(tag1),
            @tagName(tag2),
            token.toByteSlice(self.source),
        },
    );
    return Error.UnexpectedToken;
}

/// Consume the next token from the lexer, skipping all comments.
fn next(self: *Self) Error!Token {
    var next_token = try self.tokenizer.next();
    while (next_token.tag == .comment or
        next_token.tag == .whitespace) : (next_token = try self.tokenizer.next())
    {
        // TODO: store comments as trivia.
    }

    const current = self.current_token;
    self.current_token = next_token;
    self.prev_token_line = current.line;
    return current;
}

/// Return the next non-whitespace and non-comment token,
/// without consuming it, or advancing in the source.
///
/// NOTE: The token returned by this function should only be used for
/// inspection, and never added to the AST.
fn lookAhead(self: *Self) Error!Token {
    const line = self.tokenizer.line;
    const index = self.tokenizer.index;

    var next_token = try self.tokenizer.next();
    while (next_token.tag == .comment or
        next_token.tag == .whitespace) : (next_token = try self.tokenizer.next())
    {}

    // restore the tokenizer state.
    self.tokenizer.rewind(index, line);
    return next_token;
}

inline fn peek(self: *Self) Token {
    return self.current_token;
}

fn isAtToken(self: *Self, tag: Token.Tag) bool {
    return self.current_token.tag == tag;
}

// --------------------------------------------------------------------------------------
// Expression parsing.
// https://262.ecma-international.org/15.0/index.html#sec-ecmascript-language-expressions
// --------------------------------------------------------------------------------------

// Expression : AssignmentExpression
//            | Expression, AssignmentExpression
fn expression(self: *Self) Error!Node.Index {
    const expr = try self.assignmentExpression();
    if (self.current_destructure_kind.must_destruct) {
        try self.emitBadDestructureDiagnostic(expr);
        return Error.UnexpectedPattern;
    }

    if (self.isAtToken(.@",")) {
        return self.completeSequenceExpr(expr);
    }

    return expr;
}

/// Parse a comma-separated sequence expression, where the first-expression is already parsed.
fn completeSequenceExpr(self: *Self, first_expr: Node.Index) Error!Node.Index {
    if (!self.isAtToken(.@",")) return first_expr;

    var nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer nodes.deinit();

    _ = try nodes.append(first_expr);

    const start_pos = self.nodes.items(.start)[@intFromEnum(first_expr)];
    var end_pos = self.nodes.items(.start)[@intFromEnum(first_expr)];

    while (self.isAtToken(.@",")) {
        _ = try self.next(); // eat ','
        const rhs = try self.assignExpressionNoPattern();
        end_pos = self.nodes.items(.start)[@intFromEnum(rhs)];
        try nodes.append(rhs);
    }

    const expr_list = try self.addSubRange(nodes.items);
    return try self.addNode(ast.NodeData{
        .sequence_expr = expr_list,
    }, start_pos, end_pos);
}

fn assignmentLhsExpr(self: *Self) Error!Node.Index {
    self.current_destructure_kind = .{
        .can_destruct = true,
        .can_be_assigned_to = true,
        .must_destruct = false,
    };

    const token: *const Token = &self.current_token;
    switch (token.tag) {
        .@"{" => return self.objectAssignmentPattern(),
        .@"[" => return self.arrayAssignmentPattern(),
        else => {
            if (token.tag == .identifier or self.isKeywordIdentifier(token.tag)) {
                return self.identifier(try self.next());
            }

            try self.emitBadTokenDiagnostic("assignment target", token);
            return Error.InvalidAssignmentTarget;
        },
    }
}

fn isSimpleAssignmentTarget(self: *const Self, node: Node.Index) bool {
    return switch (self.getNode(node).data) {
        .identifier, .member_expr, .computed_member_expr => true,
        else => false,
    };
}

/// Mutate existing nodes to convert a PrimaryExpression to an
/// AssignmentPattern (or Identifier) (e.g, .object_literal => .object_pattern)
fn reinterpretAsPattern(self: *Self, node_id: Node.Index) void {
    const node: *ast.NodeData = &self.nodes.items(.data)[@intFromEnum(node_id)];
    switch (node.*) {
        .identifier,
        .member_expr,
        .assignment_pattern,
        .empty_array_item,
        .rest_element,
        .computed_member_expr,
        .call_expr,
        => return,
        .object_literal => |object_pl| {
            node.* = .{ .object_pattern = object_pl };
            const elems_idx = object_pl orelse return;
            const elems = elems_idx.asSlice(self);
            for (elems) |elem_id| {
                self.reinterpretAsPattern(elem_id);
            }
        },
        .array_literal => |array_pl| {
            node.* = .{ .array_pattern = array_pl };
            const elems_idx = array_pl orelse return;
            const elems = elems_idx.asSlice(self);
            for (elems) |elem_id| {
                self.reinterpretAsPattern(elem_id);
            }
        },
        .object_property => |property_definiton| self.reinterpretAsPattern(property_definiton.value),
        .assignment_expr => |assign_pl| {
            node.* = .{ .assignment_pattern = assign_pl };
        },
        .spread_element => |spread_pl| {
            self.reinterpretAsPattern(spread_pl);
            node.* = .{ .rest_element = spread_pl };
        },
        else => unreachable,
    }
}

/// Parse an assignment expression that may be an L or an R-value.
/// i.e, it may return a pattern like `{x=1}` that *must* be used as a destructuring or assignment target,
/// or, it may return a pattern like `{x:a.b}` that can be assigned to, but not destructured
/// (`let { x: a.b } = ...` is invalid, `({x: a.b} = ...)` is fine).
/// It may also return a regular R-Value that is neither a valid destructuring target, nor a pattern.
/// To use any value returned by this function as a destructuring target, you must call `reinterpretAsPattern`:
/// ```zig
/// const expr_or_pattern = try assignmentExpression();
/// if (self.current_destructure_kind.must_destruct) {
///     // this will modify this node and its children in-place,
///     // and convert, for example, an `object_literal` to an `object_pattern`.
///     self.reinterpretAsPattern(expr_or_pattern);
/// }
/// ```
/// To parse an expression that *must* be a valid R-Value, use `assignExpressionNoPattern`.
fn assignmentExpression(self: *Self) Error!Node.Index {
    // Start with the assumption that we're parsing a valid destructurable expression.
    // Subsequent parsing functions will update this to refect the actual destructure-kind
    // of `lhs`.
    self.current_destructure_kind = .{
        .can_destruct = true,
        .can_be_assigned_to = true,
        .must_destruct = false,
    };

    const lhs = try yieldOrConditionalExpression(self);

    if (!self.current_token.tag.isAssignmentOperator()) {
        return lhs;
    }

    if (!self.current_destructure_kind.can_be_assigned_to) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "Invalid assignment target",
            .{},
        );
        return Error.InvalidAssignmentTarget;
    }

    self.reinterpretAsPattern(lhs);

    const op_token = try self.next(); // eat assignment operator

    const rhs = try self.assignmentExpression();
    const start = self.nodes.items(.start)[@intFromEnum(lhs)];
    const end = self.nodes.items(.end)[@intFromEnum(rhs)];

    // An assignment expression is a valid destructuring target.
    // `[x = 1] = [2]` is valid.
    // assignment patterns inside object literals are handled separately
    // in `Parser.identifierProperty`
    self.current_destructure_kind = .{
        .can_destruct = op_token.tag == .@"=",
        .can_be_assigned_to = true,
        .must_destruct = false,
    };

    return self.addNode(
        .{
            .assignment_expr = .{
                .lhs = lhs,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            },
        },
        start,
        end,
    );
}

/// Parse an assignment expression, and ensure that its not a pattern
/// that must be destructured (e.g: `{x=1}`).
fn assignExpressionNoPattern(self: *Self) Error!Node.Index {
    const expr = try self.assignmentExpression();
    if (self.current_destructure_kind.must_destruct) {
        const expr_start = self.nodes.items(.start)[@intFromEnum(expr)];
        const error_coord = util.offsets.byteIndexToCoordinate(self.source, expr_start);
        // TODO: the location for this error can be improved.
        try self.emitDiagnostic(
            error_coord,
            "Found destructuring pattern instead of an expression",
            .{},
        );

        return Error.UnexpectedPattern;
    }

    return expr;
}

fn coalesceExpression(self: *Self, start_expr: Node.Index) Error!Node.Index {
    const start_expr_span = self.nodeSpan(start_expr);
    const start_pos = start_expr_span.start;
    var end_pos = start_expr_span.end;

    var expr: Node.Index = start_expr;
    while (self.isAtToken(.@"??")) {
        const op = try self.next(); // eat '??'
        const rhs = try bOrExpr(self);
        end_pos = self.nodeSpan(rhs).end;

        self.current_destructure_kind.setNoAssignOrDestruct();

        expr = try self.addNode(
            .{
                .binary_expr = .{
                    .lhs = expr,
                    .rhs = rhs,
                    .operator = try self.addToken(op),
                },
            },
            start_pos,
            end_pos,
        );
    }

    return expr;
}

/// ShortCircuitExpression:
///    LogicalOrExpression
///    CoalesceExpression
fn shortCircuitExpresion(self: *Self) Error!Node.Index {
    const expr = try lOrExpr(self);
    switch (self.nodes.items(.data)[@intFromEnum(expr)]) {
        .binary_expr => |pl| {
            const op_tag = self.tokens.items[@intFromEnum(pl.operator)].tag;
            if (op_tag == .@"||" or op_tag == .@"&&") {
                return expr;
            }
            return self.coalesceExpression(expr);
        },
        else => {
            return self.coalesceExpression(expr);
        },
    }
}

fn yieldOrConditionalExpression(self: *Self) Error!Node.Index {
    if (!(self.current_token.tag == .kw_yield and self.context.is_yield_reserved)) {
        return self.conditionalExpression();
    }

    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.yieldExpression();
}

fn yieldExpression(self: *Self) Error!Node.Index {
    const yield_kw = try self.next();
    std.debug.assert(yield_kw.tag == .kw_yield and self.context.is_yield_reserved);

    var has_operand = false;
    var is_delegated = false;
    if (self.current_token.line == yield_kw.line) {
        if (self.current_token.tag == .@"*") {
            _ = try self.next();
            is_delegated = true;
            has_operand = true;
        } else {
            // `{ a = yield } = 5;` is valid JS syntax,
            // and we shouldn't try to parse an expression
            // even though the '}' and 'yield' are on the same line.
            has_operand = switch (self.current_token.tag) {
                .@"]", .@"}", .@",", .@")", .@";" => false,
                else => true,
            };
        }
    }

    var end_pos = yield_kw.start + yield_kw.len;
    const operand: ?Node.Index = blk: {
        if (has_operand) {
            const operand_expr = try self.assignExpressionNoPattern();
            end_pos = self.nodes.items(.end)[@intFromEnum(operand_expr)];
            break :blk operand_expr;
        }

        break :blk null;
    };

    return self.addNode(.{
        .yield_expr = .{
            .value = operand,
            .is_delegated = is_delegated,
        },
    }, yield_kw.start, end_pos);
}

/// ConditionalExpression:
///     ShortCircuitExpression
///     ShortCircuitExpression '?' AssignmentExpression ':' AssignmentExpression
fn conditionalExpression(self: *Self) Error!Node.Index {
    const cond_expr = try self.shortCircuitExpresion();
    if (!self.isAtToken(.@"?")) return cond_expr;

    self.current_destructure_kind.setNoAssignOrDestruct();

    _ = try self.next(); // eat '?'
    const true_expr = try self.assignExpressionNoPattern();
    _ = try self.expect(.@":");

    // [+In] grammar parameter
    const context = self.context;
    self.context.in = true;
    const false_expr = try self.assignExpressionNoPattern();
    self.context = context;

    const start_pos = self.nodes.items(.start)[@intFromEnum(cond_expr)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(false_expr)];

    return try self.addNode(
        .{
            .conditional_expr = .{
                .condition = cond_expr,
                .consequent = true_expr,
                .alternate = false_expr,
            },
        },
        start_pos,
        end_pos,
    );
}

// TODO: check static semantics according to the spec.
fn assignmentPattern(self: *Self) Error!Node.Index {
    const lhs = try self.lhsExpression();
    if (!self.isAtToken(.@"=")) return lhs;

    const op_token = try self.next(); // eat '='

    const context = self.context;
    self.context.in = true;
    const rhs = try self.assignmentExpression();
    self.context = context;

    const lhs_start_pos = self.nodes.items(.start)[@intFromEnum(lhs)];
    const rhs_end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];

    self.current_destructure_kind.must_destruct = true;
    self.current_destructure_kind.can_destruct = true;
    self.current_destructure_kind.can_be_assigned_to = true;
    return self.addNode(
        .{
            .assignment_pattern = .{
                .lhs = lhs,
                .rhs = rhs,
                .operator = try self.addToken(op_token),
            },
        },
        lhs_start_pos,
        rhs_end_pos,
    );
}

/// https://tc39.es/ecma262/#prod-ArrayAssignmentPattern
fn arrayAssignmentPattern(self: *Self) Error!Node.Index {
    const lbrac = try self.next(); // eat '['
    std.debug.assert(lbrac.tag == .@"[");

    var items = std.ArrayList(Node.Index).init(self.allocator);
    defer items.deinit();

    var token = self.peek();
    while (true) : (token = self.peek()) {
        switch (token.tag) {
            .@"," => {
                const comma = try self.next(); // eat ','
                if (self.isAtToken(.@"]")) break;
                try items.append(try self.addNode(
                    .{ .empty_array_item = {} },
                    comma.start,
                    comma.start + 1,
                ));
            },

            .@"..." => {
                try items.append(try self.restElement());
                if (self.isAtToken(.@",")) {
                    const comma_tok = try self.next();
                    try self.emitDiagnostic(
                        comma_tok.startCoord(self.source),
                        "Comma not permitted after spread element in array pattern",
                        .{},
                    );
                    return Error.InvalidAssignmentTarget;
                }

                break;
            },

            .@"]" => break,

            else => {
                try items.append(try self.assignmentPattern());
            },
        }

        if (self.current_token.tag == .@",") {
            _ = try self.next();
        } else if (self.current_token.tag == .@"]") {
            break;
        }
    }

    const rbrac = try self.expect(.@"]"); // eat ']'
    const array_items = try self.addSubRange(items.items);
    return try self.addNode(
        .{ .array_pattern = array_items },
        lbrac.start,
        rbrac.start + rbrac.len,
    );
}

fn completePropertyPatternDef(self: *Self, key: Node.Index) Error!Node.Index {
    _ = try self.expect(.@":");

    const value = try self.assignmentPattern();
    const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(value)];

    return self.addNode(
        .{ .object_property = .{ .key = key, .value = value } },
        start_pos,
        end_pos,
    );
}

fn destructuredPropertyDefinition(self: *Self) Error!Node.Index {
    switch (self.current_token.tag) {
        .string_literal,
        .numeric_literal,
        .legacy_octal_literal,
        => {
            const key_token = try self.next();
            const key = try self.parseLiteral(&key_token);
            return self.completePropertyPatternDef(key);
        },
        .@"[" => {
            _ = try self.next(); // eat '['
            const key = try self.assignmentExpression();
            _ = try self.expect(.@"]");
            return self.completePropertyPatternDef(key);
        },
        else => {
            if (self.current_token.tag == .identifier or
                self.current_token.tag.isKeyword())
            {
                return self.destructuredIdentifierProperty();
            }

            return try self.assignmentPattern();
        },
    }
}

/// Parse a destructured object property starting with an identifier.
/// Assumes that self.current_token is the .identifier.
fn destructuredIdentifierProperty(self: *Self) Error!Node.Index {
    const key_token = try self.next();
    const key = try self.identifier(key_token);

    const cur_token = self.peek();
    if (cur_token.tag == .@"=") {
        const eq_token = try self.next(); // eat '='
        const rhs = try self.assignmentExpression();
        const end_pos = self.getNode(rhs).end;

        self.current_destructure_kind.must_destruct = true;
        self.current_destructure_kind.can_destruct = true;
        self.current_destructure_kind.can_be_assigned_to = true;

        return self.addNode(
            .{
                .assignment_pattern = .{
                    .lhs = key,
                    .rhs = rhs,
                    .operator = try self.addToken(eq_token),
                },
            },
            key_token.start,
            end_pos,
        );
    }

    if (cur_token.tag == .@":") {
        return self.completePropertyPatternDef(key);
    }

    // Disallow stuff like "`{ if }`" (`{ if: x }` is valid)
    if (!(key_token.tag == .identifier or self.isKeywordIdentifier(key_token.tag))) {
        try self.emitDiagnosticOnToken(
            key_token,
            "Unexpected '{s}' in destructuring pattern",
            .{key_token.toByteSlice(self.source)},
        );

        return Error.UnexpectedToken;
    }

    return self.addNode(
        .{
            .object_property = ast.PropertyDefinition{
                .key = key,
                .value = key,
                .flags = .{ .is_shorthand = true },
            },
        },
        key_token.start,
        key_token.start + key_token.len,
    );
}

/// https://tc39.es/ecma262/#prod-ObjectAssignmentPattern
fn objectAssignmentPattern(self: *Self) Error!Node.Index {
    const lbrace = try self.next(); // eat '{'
    std.debug.assert(lbrace.tag == .@"{");

    var props = std.ArrayList(Node.Index).init(self.allocator);
    defer props.deinit();

    var end_pos = lbrace.start + lbrace.len;

    var destruct_kind = self.current_destructure_kind;

    var cur_token = self.current_token;
    while (cur_token.tag != .@"}") : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"..." => {
                try props.append(try self.restElement());
                destruct_kind.update(self.current_destructure_kind);

                // TODO: we should continue parsing after the rest element,
                // and report this error later.
                const rb = try self.expect(.@"}");
                end_pos = rb.start + rb.len;
                break;
            },

            .identifier,
            .string_literal,
            .numeric_literal,
            .legacy_octal_literal,
            .@"[",
            => {
                const prop = try self.destructuredPropertyDefinition();
                destruct_kind.update(self.current_destructure_kind);
                try props.append(prop);
            },

            else => {
                if (cur_token.tag.isKeyword()) {
                    const prop = try self.destructuredPropertyDefinition();
                    destruct_kind.update(self.current_destructure_kind);
                    try props.append(prop);
                } else {
                    try self.emitDiagnostic(
                        cur_token.startCoord(self.source),
                        "Unexpected '{s}' while parsing destructured object pattern",
                        .{cur_token.toByteSlice(self.source)},
                    );

                    return Error.InvalidAssignmentTarget;
                }
            },
        }

        const comma_or_rbrace = try self.expect2(.@"}", .@",");
        if (comma_or_rbrace.tag == .@"}") {
            end_pos = comma_or_rbrace.start + comma_or_rbrace.len;
            break;
        }
    } else {
        const rbrace = try self.next();
        end_pos = rbrace.start + rbrace.len;
    }

    const destructured_props = try self.addSubRange(props.items);
    self.current_destructure_kind = destruct_kind;

    return self.addNode(
        .{ .object_pattern = destructured_props },
        lbrace.start,
        end_pos,
    );
}

fn unaryExpression(self: *Self) Error!Node.Index {
    const token = self.peek();
    switch (token.tag) {
        .kw_delete,
        .kw_typeof,
        .kw_void,
        .@"-",
        .@"+",
        .@"~",
        .@"!",
        => {
            const op_token = try self.next();
            const expr = try self.unaryExpression();
            const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
            self.current_destructure_kind.setNoAssignOrDestruct();
            return try self.addNode(.{
                .unary_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = try self.addToken(op_token),
                },
            }, op_token.start, expr_end_pos);
        },
        else => {
            if (self.isAtToken(.kw_await) and self.context.is_await_reserved) {
                return self.awaitExpression();
            }

            return self.updateExpression();
        },
    }
}

/// Parse an await expression when `self.current_token`
/// is the `await` keyword.
fn awaitExpression(self: *Self) Error!Node.Index {
    const await_token = try self.next();
    std.debug.assert(await_token.tag == .kw_await);

    if (!self.context.is_await_reserved) {
        try self.emitDiagnostic(
            await_token.startCoord(self.source),
            "'await' expressions are only permitted inside async functions",
            .{},
        );
        return Error.IllegalAwait;
    }

    const operand = try self.unaryExpression();
    const end_pos = self.nodeSpan(operand).end;

    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{
        .await_expr = ast.UnaryPayload{
            .operator = try self.addToken(await_token),
            .operand = operand,
        },
    }, await_token.start, end_pos);
}

/// The ECMASCript262 standard describes a syntax directed operation
/// called `AssignmentTargetType`, which determines if a given expression
/// is "SIMPLE", a.k.a, valid in contexts like the operand of `<expr>++`.
fn isExprSimple(self: *Self) Error!Node.Index {
    _ = self;
}

fn updateExpression(self: *Self) Error!Node.Index {
    const token = self.peek();
    if (token.tag == .@"++" or token.tag == .@"--") {
        const op_token = try self.next();
        const expr = try self.unaryExpression();
        const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
        self.current_destructure_kind.setNoAssignOrDestruct();
        return self.addNode(.{
            .update_expr = ast.UnaryPayload{
                .operand = expr,
                .operator = try self.addToken(op_token),
            },
        }, op_token.start, expr_end_pos);
    }

    // post increment / decrement
    const expr_start_line = self.peek().line;
    const expr = try self.lhsExpression();
    const cur_token = self.peek();
    if ((cur_token.tag == .@"++" or cur_token.tag == .@"--") and
        cur_token.line == expr_start_line)
    {
        self.current_destructure_kind.setNoAssignOrDestruct();
        const op_token = try self.next();
        const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
        return self.addNode(.{
            .post_unary_expr = .{
                .operand = expr,
                .operator = try self.addToken(op_token),
            },
        }, op_token.start, expr_end_pos);
    }

    return expr;
}

fn lhsExpression(self: *Self) Error!Node.Index {
    var lhs_expr = try self.memberExpression();
    if (try self.tryCallExpression(lhs_expr)) |call_expr| {
        lhs_expr = call_expr;
    }

    if (self.isAtToken(.@"?.")) {
        lhs_expr = try self.optionalExpression(lhs_expr);
    }
    return lhs_expr;
}

fn superExpression(self: *Self) Error!Node.Index {
    const super_token = try self.next();
    // TODO: check if we're in a surrounding class.
    std.debug.assert(super_token.tag == .kw_super);

    const super_args, const start, const end = try self.parseArgs();
    return self.addNode(.{ .super_call_expr = super_args }, start, end);
}

/// Parse a 'new' expression if 'current_token' is 'new',
/// otherwise return `null`.
fn tryNewExpression(self: *Self) Error!?Node.Index {
    if (self.isAtToken(.kw_new)) {
        const new_token = try self.next();
        return try self.completeNewExpression(&new_token);
    }
    return null;
}

fn completeNewExpression(self: *Self, new_token: *const Token) Error!Node.Index {
    const expr = try self.memberExpression();
    const expr_end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
    return try self.addNode(.{
        .new_expr = .{
            .callee = expr,
            .arguments = if (self.isAtToken(.@"("))
                try self.args()
            else
                try self.addNode(
                    .{ .arguments = null },
                    new_token.start,
                    new_token.start,
                ),
        },
    }, new_token.start, expr_end_pos);
}

/// Try parsing a call expression. If the input is malformed, return a `Error`,
/// If no call expression was found, return `null`,
/// Otherwise, return the index of the call expression node.
/// NOTE: The call expression grammar might seem a little odd, because it
/// also has productions that parse member expressions:
/// https://262.ecma-international.org/15.0/index.html#prod-CallExpression
fn tryCallExpression(self: *Self, callee: Node.Index) Error!?Node.Index {
    const token = self.peek();
    if (token.tag != .@"(") return null;

    var call_expr = try self.coverCallAndAsyncArrowHead(callee);
    var cur_token = self.peek();
    var destruct_kind = self.current_destructure_kind;
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"(" => {
                call_expr = try self.completeCallExpression(call_expr);
                destruct_kind.setNoAssignOrDestruct();
            },
            .@"[" => {
                call_expr = try self.completeComputedMemberExpression(call_expr);
                destruct_kind.can_destruct = false;
                destruct_kind.can_be_assigned_to = true;
            },
            .@"." => {
                call_expr = try self.completeMemberExpression(call_expr);
                destruct_kind.can_destruct = false;
                destruct_kind.can_be_assigned_to = true;
            },
            .template_literal_part => {
                call_expr = try self.completeTaggedTemplate(call_expr);
                destruct_kind.setNoAssignOrDestruct();
            },
            else => break,
        }
    }

    self.current_destructure_kind = destruct_kind;
    return call_expr;
}

/// When we're at the '(' token, parse the arguments and return a complete call expression
/// ```js
/// myFunc(...
/////    ^-- already parsed the callee
/// ```
fn completeCallExpression(self: *Self, callee: Node.Index) Error!Node.Index {
    const start_pos = self.nodes.items(.start)[@intFromEnum(callee)];
    const call_args = try self.args();
    const call_expr = ast.CallExpr{
        .arguments = call_args,
        .callee = callee,
    };
    const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{ .call_expr = call_expr }, start_pos, end_pos);
}

// CoverCallAndAsyncArrowHead:  MemberExpression Arguments
fn coverCallAndAsyncArrowHead(self: *Self, callee: Node.Index) Error!Node.Index {
    const call_args = try self.args();
    const start_pos = self.nodes.items(.start)[@intFromEnum(callee)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];

    return self.addNode(.{
        .call_expr = .{
            .callee = callee,
            .arguments = call_args,
        },
    }, start_pos, end_pos);
}

/// https://262.ecma-international.org/15.0/index.html#prod-OptionalExpression
fn optionalExpression(self: *Self, object: Node.Index) Error!Node.Index {
    var expr = object;
    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"?." => expr = try self.completeOptionalChain(expr),
            else => return expr,
        }
    }

    self.current_destructure_kind.setNoAssignOrDestruct();
    return expr;
}

/// Assuming a `<object>?.<property>` has been consumed already, consume the
/// operators that are chained on top, and return a node which will be put into
/// an `optional_expr` field of `ast.Node`.
/// see: `Self.optionalChain`.
fn completeOptionalChain(self: *Self, prev_expr: Node.Index) Error!Node.Index {
    var expr = try self.optionalChain(prev_expr);
    const start_pos = self.nodes.items(.start)[@intFromEnum(expr)];

    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"[" => {
                const member_expr = try self.completeComputedMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"." => {
                const member_expr = try self.completeMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, start_pos, end_pos);
            },
            .@"(" => {
                const call_expr = try self.completeCallExpression(expr);
                const end_pos = self.nodes.items(.end)[@intFromEnum(call_expr)];
                expr = try self.addNode(.{ .optional_expr = call_expr }, start_pos, end_pos);
            },
            else => return expr,
        }
    }

    return expr;
}

/// Parse an OptionalExpression:
/// The expression before the `?.` operator is already parsed and passed as an argument.
///
/// See: https://262.ecma-international.org/15.0/index.html#prod-OptionalExpression
fn optionalChain(self: *Self, object: Node.Index) Error!Node.Index {
    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];

    const chain_op = try self.next();
    std.debug.assert(chain_op.tag == .@"?.");

    const cur_token = self.peek();

    switch (cur_token.tag) {
        .@"(" => {
            const call_args = try self.args();
            const end_pos = self.nodes.items(.end)[@intFromEnum(call_args)];
            const call_expr = try self.addNode(.{
                .call_expr = .{
                    .arguments = call_args,
                    .callee = object,
                },
            }, start_pos, end_pos);
            return self.addNode(.{ .optional_expr = call_expr }, start_pos, end_pos);
        },
        .@"[" => {
            const expr = try self.completeComputedMemberExpression(object);
            const end_pos = self.nodes.items(.end)[@intFromEnum(expr)];
            return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
        },
        else => {
            if (self.current_token.tag == .identifier or self.current_token.tag.isKeyword()) {
                const property_name_token = try self.next(); // eat the property name
                const end_pos = property_name_token.start + property_name_token.len;
                const expr = try self.addNode(.{ .member_expr = .{
                    .object = object,
                    .property = try self.addToken(property_name_token),
                } }, start_pos, end_pos);
                return self.addNode(.{ .optional_expr = expr }, start_pos, end_pos);
            }

            try self.emitDiagnostic(
                cur_token.startCoord(self.source),
                "Expected property access or function call after ?., but got {s}",
                .{cur_token.toByteSlice(self.source)},
            );
            return Error.UnexpectedToken;
        },
    }
}

fn memberExpression(self: *Self) Error!Node.Index {
    var member_expr = switch (self.current_token.tag) {
        .kw_new => try self.newTargetOrExpression(),
        .kw_import => try self.importMetaOrCall(),
        .kw_super => try self.superPropertyOrCall(),
        else => try self.primaryExpression(),
    };

    var token = self.current_token;
    while (token.tag != .eof) : (token = self.peek()) {
        switch (token.tag) {
            .@"." => member_expr = try self.completeMemberExpression(member_expr),
            .@"[" => member_expr = try self.completeComputedMemberExpression(member_expr),
            .template_literal_part => member_expr = try self.completeTaggedTemplate(member_expr),
            else => return member_expr,
        }
    }
    return member_expr;
}

/// A `new.target` meta property, or a regular NewExpression
/// https://tc39.es/ecma262/#prod-NewTarget
/// https://tc39.es/ecma262/#prod-NewExpression
fn newTargetOrExpression(self: *Self) Error!Node.Index {
    const new_token = try self.next();
    std.debug.assert(new_token.tag == .kw_new);
    if (self.isAtToken(.@"."))
        return self.parseMetaProperty(&new_token, "target");
    // No '.' after 'new', so we're parsing a regular old
    // NewExpression.
    return self.completeNewExpression(&new_token);
}

/// Parse an `import.meta` meta property, or an ImportCall:
/// https://tc39.es/ecma262/#prod-ImportMeta
/// https://tc39.es/ecma262/#prod-ImportCall
fn importMetaOrCall(self: *Self) Error!Node.Index {
    const import_token = try self.next();
    if (self.isAtToken(.@"."))
        return try self.parseMetaProperty(&import_token, "meta");

    // TODO: support "ImportCall":
    // https://tc39.es/ecma262/#prod-ImportCall
    try self.emitDiagnosticOnToken(import_token, "Unexpected 'import'", .{});
    return Error.UnexpectedToken;
}

/// Parse a super property or call expression.
/// https://tc39.es/ecma262/#prod-SuperProperty
/// https://tc39.es/ecma262/#prod-SuperCall
/// SuperProperty:
///  super [ Expression ]
///  super . IdentifierName
/// SuperCall:
///  super Arguments
fn superPropertyOrCall(self: *Self) Error!Node.Index {
    // TODO: disallow super outside classes that have a super class
    const super_token = try self.next();
    std.debug.assert(super_token.tag == .kw_super);

    switch (self.current_token.tag) {
        .@"[" => {
            const super = try self.makeSuper(&super_token);
            return self.completeComputedMemberExpression(super);
        },
        .@"." => {
            const super = try self.makeSuper(&super_token);
            return self.completeMemberExpression(super);
        },
        else => {
            const super_args, const start, const end = try self.parseArgs();
            return self.addNode(.{ .super_call_expr = super_args }, start, end);
        },
    }
}

/// Parse a 'MetaProperty' like `new.target` or `import.meta`.
/// Assumes that `self.current_token` is the `new` or `import` keyword,
/// and that current_token is a '.'
fn parseMetaProperty(
    self: *Self,
    meta_token: *const Token,
    wanted_property_name: []const u8,
) Error!Node.Index {
    const dot = try self.next();
    std.debug.assert(dot.tag == .@".");

    const property_token = try self.expect(.identifier);
    const property_name_str = property_token.toByteSlice(self.source);
    if (!std.mem.eql(u8, property_name_str, wanted_property_name)) {
        try self.emitDiagnostic(
            property_token.startCoord(self.source),
            "Unexpected {s}, did you mean to use the meta property '{s}.{s}'",
            .{
                property_name_str,
                meta_token.toByteSlice(self.source),
                wanted_property_name,
            },
        );
        return Error.InvalidMetaProperty;
    }

    if (self.source_type == .module and meta_token.tag == .kw_import) {
        try self.emitDiagnostic(
            meta_token.startCoord(self.source),
            "Cannot use 'import.meta' outside a module",
            .{},
        );

        return Error.InvalidMetaProperty;
    }

    const meta = try self.identifier(meta_token.*);
    const property = try self.identifier(property_token);

    const end_pos = property_token.start + property_token.len;
    return self.addNode(
        .{
            .meta_property = .{
                .meta = meta,
                .property = property,
            },
        },
        meta_token.start,
        end_pos,
    );
}

/// When `current_token` is a template literal part, and a member_expr has been parsed
/// this function parses a tagged template expression like "div`hello`"
fn completeTaggedTemplate(self: *Self, tag: Node.Index) Error!Node.Index {
    const template = try self.templateLiteral();
    const start_pos = self.nodes.items(.start)[@intFromEnum(tag)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(template)];

    return self.addNode(.{
        .tagged_template_expr = .{
            .tag = tag,
            .template = template,
        },
    }, start_pos, end_pos);
}

fn completeMemberExpression(self: *Self, object: Node.Index) Error!Node.Index {
    const dot = try self.next(); // eat "."
    std.debug.assert(dot.tag == .@".");

    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];

    const property_token_idx: Token.Index = blk: {
        const tok = try self.next();
        // Yes, keywords are valid property names...
        if (tok.tag == .identifier or tok.tag.isKeyword()) {
            break :blk try self.addToken(tok);
        }

        try self.emitDiagnostic(
            tok.startCoord(self.source),
            "Expected to see a property name after '.', got a '{s}' instead",
            .{tok.toByteSlice(self.source)},
        );
        return Error.UnexpectedToken;
    };

    const property_access = ast.PropertyAccess{
        .object = object,
        .property = property_token_idx,
    };

    const property_token = self.tokens.items[@intFromEnum(property_token_idx)];
    const end_pos = property_token.start + property_token.len;
    self.current_destructure_kind.can_destruct = false;
    self.current_destructure_kind.can_be_assigned_to = true;
    return self.addNode(.{ .member_expr = property_access }, start_pos, end_pos);
}

fn completeComputedMemberExpression(self: *Self, object: Node.Index) Error!Node.Index {
    // Apply the [+In] grammar parameter.
    // in expressions are allowed in places like: for([a in b] in c)
    const old_ctx = self.context;
    self.context.in = true;
    defer self.context = old_ctx;

    const tok = try self.next(); // eat "["
    std.debug.assert(tok.tag == .@"[");

    const property = try self.expression();
    _ = try self.expect(.@"]");

    const property_access = ast.ComputedPropertyAccess{
        .object = object,
        .property = property,
    };

    const start_pos = self.nodes.items(.start)[@intFromEnum(object)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(property)];
    self.current_destructure_kind.can_destruct = false;
    self.current_destructure_kind.can_be_assigned_to = true;
    return self.addNode(.{ .computed_member_expr = property_access }, start_pos, end_pos);
}

fn primaryExpression(self: *Self) Error!Node.Index {
    // If we're currently at a '/' or '/=' token,
    // we probably have mistaken a regex literal's opening '/' for an operator.
    // We'll rewind the tokenizer and try to parse a regex literal instead.
    const cur = &self.current_token;
    if (cur.tag == .@"/" or cur.tag == .@"/=") {
        // Go back to the beginning of '/'
        self.tokenizer.rewind(cur.start, cur.line);
        self.tokenizer.assume_bslash_starts_regex = true;
        self.current_token = try self.tokenizer.next();
        self.tokenizer.assume_bslash_starts_regex = false;
    }

    if (cur.tag == .template_literal_part) return self.templateLiteral();

    const token = try self.next();
    switch (token.tag) {
        .kw_class => return self.classExpression(token),
        .kw_this => {
            self.current_destructure_kind.setNoAssignOrDestruct();
            return self.addNode(
                .{ .this = try self.addToken(token) },
                token.start,
                token.start + token.len,
            );
        },
        .identifier => {
            if (self.isAtToken(.@"=>")) {
                // arrow function starting with an identifier:
                // 'x => 1'.
                return self.identifierArrowFunction(&token);
            }
            return self.identifier(token);
        },
        .legacy_octal_literal,
        .numeric_literal,
        .regex_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => return self.parseLiteral(&token),
        .@"[" => return self.arrayLiteral(token.start),
        .@"{" => return self.objectLiteral(token.start),
        .@"(" => return self.groupingExprOrArrowFunction(&token),
        .kw_async => return self.asyncExpression(&token),
        .kw_function => return self.functionExpression(token.start, .{}),
        else => {},
    }

    if (self.isKeywordIdentifier(token.tag)) {
        if (self.isAtToken(.@"=>")) {
            // arrow function starting with keyword identifier: 'yield => 1'
            return self.identifierArrowFunction(&token);
        }
        return self.identifier(token);
    }

    try self.emitDiagnostic(
        token.startCoord(self.source),
        "Unexpected '{s}'",
        .{token.toByteSlice(self.source)},
    );
    return Error.UnexpectedToken;
}

fn parseLiteral(self: *Self, token: *const Token) Error!Node.Index {
    if (self.context.strict and token.tag == .legacy_octal_literal) {
        try self.emitDiagnostic(
            token.startCoord(self.source),
            "Legacy octal literals are not allowed in strict mode",
            .{},
        );
        return Error.UnexpectedToken;
    }

    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(
        .{ .literal = try self.addToken(token.*) },
        token.start,
        token.start + token.len,
    );
}

fn stringLiteral(self: *Self) Error!Node.Index {
    const token = try self.expect(.string_literal);
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(
        .{ .literal = try self.addToken(token) },
        token.start,
        token.start + token.len,
    );
}

fn stringLiteralFromToken(self: *Self, token: Token) Error!Node.Index {
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(
        .{ .literal = try self.addToken(token) },
        token.start,
        token.start + token.len,
    );
}

/// Parse an arrow function that starts with an identifier token.
/// E.g: `x => 1` or `yield => 2`
fn identifierArrowFunction(self: *Self, token: *const Token) Error!Node.Index {
    const id = try self.identifier(token.*);
    const params_range = try self.addSubRange(&[_]Node.Index{id});
    // TODO: should functions with single parameters just not store a SubRange and store the
    // parameter directly?
    const params = try self.addNode(
        .{ .parameters = params_range },
        token.start,
        token.start + token.len,
    );
    return self.completeArrowFunction(token, token, params, .{ .is_arrow = true });
}

/// Parse a template literal expression.
fn templateLiteral(self: *Self) Error!Node.Index {
    // Handle the [+In] grammar parameter
    // to allow `for(`${foo in bar}` in baz)`;
    const ctx = self.context;
    self.context.in = true;
    defer self.context = ctx;

    // TODO: use the scratch space here.
    var template_parts = try std.ArrayList(Node.Index).initCapacity(self.allocator, 4);
    defer template_parts.deinit();

    var template_token = try self.next();
    std.debug.assert(template_token.tag == .template_literal_part);
    const start_pos = template_token.start;
    var end_pos = template_token.start + template_token.len;

    try template_parts.append(try self.addNode(
        .{ .template_element = try self.addToken(template_token) },
        start_pos,
        end_pos,
    ));

    while (!self.isTemplateEndToken(&template_token)) {
        // parse an interpolation expression.
        try template_parts.append(try self.expression());

        // The most recently processed (but unconsumed) token should be a '}'.
        // We want to rewind back one character, and make the tokenizer treat the '}'
        // as a part of a template literal.
        self.tokenizer.rewind(self.current_token.start, self.current_token.line);
        self.tokenizer.assume_rbrace_is_template_part = true;
        self.current_token = try self.tokenizer.next();
        self.tokenizer.assume_rbrace_is_template_part = false;

        template_token = try self.expect(.template_literal_part);

        // Now, parse the template part that follows
        try template_parts.append(try self.addNode(
            .{ .template_element = try self.addToken(template_token) },
            template_token.start,
            template_token.start + template_token.len,
        ));
        end_pos = template_token.start + template_token.len;
    }

    const elements = try self.addSubRange(template_parts.items);
    return try self.addNode(
        .{ .template_literal = elements },
        start_pos,
        end_pos,
    );
}

fn isTemplateEndToken(self: *const Self, token: *const Token) bool {
    return self.source[token.start + token.len - 1] == '`';
}

/// Parse a primary expression that starts with the 'async' token.
/// We have to be careful to not parse 'async' as an identifier
/// in places where it starts an async function expression.
/// All of these are valid:
/// ```js
/// async x => 1 // async arrow function
/// async (x) => x; // async arrow function
/// async(x); // call expression where callee is an identifier 'async'.
/// async function () { }; // async function expression
/// async; // async as an identifier.
/// async // expression statement
/// x => 1 // regular arrow function, not async
/// ```
fn asyncExpression(self: *Self, async_token: *const Token) Error!Node.Index {
    const async_line = async_token.line;
    if (self.current_token.tag == .kw_function and async_line == self.current_token.line) {
        _ = try self.next(); // eat 'function keyword'
        return self.functionExpression(async_token.start, .{ .is_async = true });
    }

    if (self.current_token.tag == .@"(") {
        const argsOrArrow = try self.callArgsOrAsyncArrowFunc(
            async_token,
            ast.FunctionFlags{
                .is_async = true,
                .is_arrow = true,
            },
        );

        const node_slice = self.nodes.slice();
        const parsed: *ast.NodeData = &node_slice.items(.data)[@intFromEnum(argsOrArrow)];
        switch (parsed.*) {
            .function_expr => return argsOrArrow,
            .arguments => {
                const async_identifier = try self.identifier(async_token.*);
                return self.addNode(
                    .{ .call_expr = .{ .callee = async_identifier, .arguments = argsOrArrow } },
                    async_token.start,
                    node_slice.items(.end)[@intFromEnum(argsOrArrow)],
                );
            },
            else => unreachable,
        }
    }

    if ((self.current_token.tag == .identifier or
        self.isKeywordIdentifier(self.current_token.tag)) and
        self.current_token.line == async_token.line)
    {
        // async x => ...
        const id_token = try self.next();
        const param = try self.identifier(id_token);
        const params_range = try self.addSubRange(&[_]Node.Index{param});
        const params = try self.addNode(
            .{ .parameters = params_range },
            id_token.start,
            id_token.start + id_token.len,
        );

        return self.completeArrowFunction(
            async_token,
            &id_token,
            params,
            .{ .is_async = true, .is_arrow = true },
        );
    }

    return self.identifier(async_token.*);
}

fn callArgsOrAsyncArrowFunc(
    self: *Self,
    async_token: *const Token,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    const lparen = try self.next();
    std.debug.assert(lparen.tag == .@"(");

    var sub_exprs = std.ArrayList(Node.Index).init(self.allocator);
    defer sub_exprs.deinit();

    var destructure_kind = DestructureKind{
        .can_destruct = true,
        .must_destruct = false,
        .can_be_assigned_to = true,
    };

    // store a aa spread-element that isn't immediately
    // followed by a ')'. We'll need it for reporting errors
    // if we see a '=>' after the part between '()' is parsed.
    // E.g: `async(...x, y)` -> this is call expression
    //  `async(...x, y) => 1` ->  this is a syntax error.
    var spread_elem_in_middle: ?Node.Index = null;

    while (!self.isAtToken(.@")")) {
        if (self.isAtToken(.@"...")) {
            const spread_elem = try self.spreadElement();
            try sub_exprs.append(spread_elem);

            if (self.isAtToken(.@")")) break;
            // spread element must be the last item in a parameter list.
            if (destructure_kind.must_destruct) {
                try self.emitDiagnosticOnNode(spread_elem, "Expected ')' after rest element");
                return Error.RestElementNotLast;
            }
            // If 'must_destruct' is not set, but we parsed a spread element
            // without seeing a ')' right after, then we must be inside a call-expression's
            // arguments.
            destructure_kind.setNoAssignOrDestruct();
            spread_elem_in_middle = spread_elem;
        } else {
            const expr = try self.assignmentExpression();
            destructure_kind.update(self.current_destructure_kind);
            try sub_exprs.append(expr);
        }

        if (destructure_kind.isMalformed()) {
            // TODO: this error message can be improved based
            // on whether the most recently parsed node was a
            // pattern.
            try self.emitDiagnosticOnNode(
                sub_exprs.items[sub_exprs.items.len - 1],
                "Unexpected expression or pattern inside '( ... )'",
            );
            return Error.UnexpectedToken;
        }

        if (self.isAtToken(.@","))
            _ = try self.next()
        else
            break;
    }

    const rparen = try self.expect(.@")");

    if (self.isAtToken(.@"=>")) {
        if (!destructure_kind.can_destruct) {
            if (spread_elem_in_middle) |node| {
                try self.emitDiagnosticOnNode(
                    node,
                    "Rest element must be the last item in a parameter list",
                );
                return Error.InvalidArrowParameters;
            }

            // TODO: improve the location of the diagnostic.
            // Which part exactly is invalid?
            try self.emitDiagnostic(
                lparen.startCoord(self.source),
                "Invalid arrow function parameters",
                .{},
            );
            return Error.InvalidArrowParameters;
        }

        // mutate the expressions so far to be interpreted as patterns.
        for (sub_exprs.items) |node| {
            self.reinterpretAsPattern(node);
        }

        const params_range = if (sub_exprs.items.len > 0)
            try self.addSubRange(sub_exprs.items)
        else
            null;
        const parameters = try self.addNode(
            .{ .parameters = params_range },
            lparen.start,
            rparen.start + 1,
        );

        return self.completeArrowFunction(
            async_token,
            &rparen,
            parameters,
            flags,
        );
    }

    if (destructure_kind.must_destruct) {
        // TODO: improve the location of the diagnostic.
        // Which part exaclty forces a destructuring pattern?
        try self.emitDiagnostic(
            lparen.startCoord(self.source),
            "function call arguments contain a destructuring pattern",
            .{},
        );
        return Error.UnexpectedPattern;
    }

    const call_args = if (sub_exprs.items.len > 0)
        try self.addSubRange(sub_exprs.items)
    else
        null;

    return self.addNode(
        .{ .arguments = call_args },
        lparen.start,
        rparen.start + 1,
    );
}

fn bindingIdentifier(self: *Self) Error!Node.Index {
    const token = try self.next();
    if (token.tag != .identifier and !self.isKeywordIdentifier(token.tag)) {
        try self.emitDiagnostic(
            token.startCoord(self.source),
            "Expected an identifier, got '{s}'",
            .{token.toByteSlice(self.source)},
        );
        return Error.InvalidBindingIdentifier;
    }

    return self.identifier(token);
}

/// Save `token` as an identifier node.
fn identifier(self: *Self, token: Token) Error!Node.Index {
    return self.addNode(
        .{ .identifier = try self.addToken(token) },
        token.start,
        token.start + token.len,
    );
}

fn makeSuper(self: *Self, super_token: *const Token) Error!Node.Index {
    std.debug.assert(super_token.tag == .kw_super);
    return self.addNode(
        .{ .super = try self.addToken(super_token.*) },
        super_token.start,
        super_token.start + super_token.len,
    );
}

/// Assuming the parameter list has been consumed, parse the body of
/// an arrow function and return the complete arrow function AST node id.
fn completeArrowFunction(
    self: *Self,
    params_start_token: *const Token, // a '(' or an identifier.
    params_end_token: *const Token, // a ')' or an identifier.
    params: Node.Index,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    std.debug.assert(flags.is_arrow);

    if (!self.isAtToken(.@"=>")) {
        if (params_start_token.tag == .@"(") {
            try self.emitDiagnostic(
                params_start_token.startCoord(self.source),
                "'()' is not a valid expression. Arrow functions start with '() => '",
                .{},
            );
        } else {
            try self.emitDiagnostic(
                params_start_token.startCoord(self.source),
                "Expected '=>' after arrow function parameters",
                .{},
            );
        }

        return Error.InvalidArrowFunction;
    }

    const fat_arrow = try self.next();
    if (params_end_token.line != fat_arrow.line) {
        try self.emitDiagnostic(
            fat_arrow.startCoord(self.source),
            "'=>' must be on the same line as the arrow function parameters",
            .{},
        );
        return Error.IllegalFatArrow;
    }

    const body = blk: {
        const body_start_token = self.current_token;
        // If an arrow function body starts with a '{',
        // we will attempt to parse it as a block statement, and not an object literal.
        if (body_start_token.tag == .@"{") {
            const context = self.context;
            defer self.context = context;
            self.context.@"return" = true;
            break :blk try self.blockStatement();
        }

        const context = self.context;
        defer self.context = context;
        self.context.is_yield_reserved = flags.is_generator;
        self.context.is_await_reserved = flags.is_async;

        const assignment = try self.assignmentExpression();
        if (self.current_destructure_kind.must_destruct) {
            try self.emitDiagnostic(
                body_start_token.startCoord(self.source),
                "Unexpected destructuring pattern in arrow function body",
                .{},
            );
            return Error.InvalidArrowFunction;
        }

        break :blk assignment;
    };

    const end_pos = self.nodes.items(.end)[@intFromEnum(body)];

    // cannot be destructured or assigned to.
    self.current_destructure_kind.reset();

    return self.addNode(ast.NodeData{
        .function_expr = ast.Function{
            .parameters = params,
            .body = body,
            .info = try self.addExtraData(.{
                .function = .{
                    .name = null,
                    .flags = flags,
                },
            }),
        },
    }, params_start_token.start, end_pos);
}

/// Parse a spread element when current_token is '...'
fn spreadElement(self: *Self) Error!Node.Index {
    const dotdotdot = try self.next();
    std.debug.assert(dotdotdot.tag == .@"...");
    const rest_arg = try self.assignExpressionNoPattern();
    const end_pos = self.nodes.items(.end)[@intFromEnum(rest_arg)];
    return self.addNode(.{ .spread_element = rest_arg }, dotdotdot.start, end_pos);
}

/// Parse a RestElement, assuming we're at the `...` token
fn restElement(self: *Self) Error!Node.Index {
    const dotdotdot = try self.next();
    std.debug.assert(dotdotdot.tag == .@"...");
    const rest_arg = try self.assignmentLhsExpr();
    const end_pos = self.nodes.items(.end)[@intFromEnum(rest_arg)];
    return self.addNode(.{ .rest_element = rest_arg }, dotdotdot.start, end_pos);
}

/// Once a '(' token has been eaten, parse the either an arrow function or a parenthesized expression.
fn completeArrowFuncOrGroupingExpr(self: *Self, lparen: *const Token) Error!Node.Index {
    const first_expr = try self.assignmentExpression();
    if (!self.current_destructure_kind.can_destruct) {
        const expr = try self.completeSequenceExpr(first_expr);
        _ = try self.expect(.@")");
        return expr;
    }

    var nodes = std.ArrayList(Node.Index).init(self.allocator);
    defer nodes.deinit();

    _ = try nodes.append(first_expr);

    var has_trailing_comma = false;

    var destructure_kind = self.current_destructure_kind;
    while (self.isAtToken(.@",")) {
        const comma_token = try self.next(); // eat ','
        // A ')' after comma is allowed in arrow function parameters,
        // but not in regular comma-separated expressions.
        if (self.isAtToken(.@")") and destructure_kind.can_destruct) {
            has_trailing_comma = true;
            break;
        }

        // A '...' at this point is either a rest parameter or a syntax error.
        if (self.isAtToken(.@"...")) {
            if (destructure_kind.can_destruct) {
                const rest_elem = try self.restElement();
                try nodes.append(rest_elem);

                if (!self.isAtToken(.@")")) {
                    try self.restParamNotLastError(&self.current_token);
                }

                break;
            }

            // A "..." inside a grouping expression is a syntax error.
            // The call to `assignmentExpression()` below will error out.
            // TODO: should I emit a better error message here anyway?
        }

        const rhs = try self.assignmentExpression();
        destructure_kind.update(self.current_destructure_kind);

        if (destructure_kind.isMalformed()) {
            // TODO: better location for the diagnostic.
            try self.emitDiagnostic(
                comma_token.startCoord(self.source),
                "Invalid object or destructuring pattern",
                .{},
            );
            return Error.InvalidObject;
        }
        try nodes.append(rhs);
    }

    const rparen = try self.expect(.@")");

    if (self.isAtToken(.@"=>")) {
        if (!destructure_kind.can_destruct) {
            // TODO: improve the location of the diagnostic.
            // Which part exactly is invalid?
            try self.emitDiagnostic(
                lparen.startCoord(self.source),
                "Invalid arrow function parameters",
                .{},
            );
            return Error.InvalidArrowParameters;
        }

        for (nodes.items) |node| {
            self.reinterpretAsPattern(node);
        }

        const params_range = try self.addSubRange(nodes.items);
        const parameters = try self.addNode(
            .{ .parameters = params_range },
            lparen.start,
            rparen.start + 1,
        );

        return self.completeArrowFunction(
            lparen,
            &rparen,
            parameters,
            .{ .is_arrow = true },
        );
    }

    if (destructure_kind.must_destruct) {
        // TODO: improve the location of the diagnostic.
        // Which part exaclty forces a destructuring pattern?
        try self.emitDiagnostic(
            lparen.startCoord(self.source),
            "Grouping expression contains a destructuring pattern",
            .{},
        );
        return Error.UnexpectedPattern;
    }

    // (a, b) cannot be assigned to or destructured,
    // but (a) = 1 is valid.
    if (nodes.items.len > 1) {
        self.current_destructure_kind.setNoAssignOrDestruct();
    }

    if (has_trailing_comma) {
        try self.emitDiagnostic(
            rparen.startCoord(self.source),
            "Trailing comma is not permitted in parenthesied expressions",
            .{},
        );
        return Error.UnexpectedToken;
    }

    if (nodes.items.len == 1) {
        return nodes.items[0];
    }

    const sequence_expr = try self.addSubRange(nodes.items);
    return self.addNode(.{ .sequence_expr = sequence_expr }, lparen.start, rparen.start + 1);
}

/// Parses either an arrow function or a parenthesized expression.
fn groupingExprOrArrowFunction(self: *Self, lparen: *const Token) Error!Node.Index {
    if (self.isAtToken(.@")")) {
        const rparen = try self.next();
        const end_pos = rparen.start + rparen.len;
        const params = try self.addNode(.{ .parameters = null }, lparen.start, end_pos);
        return self.completeArrowFunction(lparen, &rparen, params, .{ .is_arrow = true });
    }

    if (self.isAtToken(.@"...")) {
        const rest_elem = try self.restElement();
        const rparen = try self.expect(.@")");
        const params = try self.addSubRange(&[_]Node.Index{rest_elem});

        const parameters = try self.addNode(
            .{ .parameters = params },
            lparen.start,
            rparen.start + rparen.len,
        );
        return self.completeArrowFunction(lparen, &rparen, parameters, .{ .is_arrow = true });
    }

    return self.completeArrowFuncOrGroupingExpr(lparen);
}

/// Parse an object literal, assuming the `{` has already been consumed.
/// https://262.ecma-international.org/15.0/index.html#prod-ObjectLiteral
fn objectLiteral(self: *Self, start_pos: u32) Error!Node.Index {
    const properties = try self.propertyDefinitionList();
    const closing_brace = try self.expect(.@"}");
    const end_pos = closing_brace.start + closing_brace.len;
    return self.addNode(.{ .object_literal = properties }, start_pos, end_pos);
}

/// https://tc39.es/ecma262/#prod-PropertyDefinitionList
/// Parse a comma-separated list of properties.
/// Returns `null` if there's 0 properties in the object.
fn propertyDefinitionList(self: *Self) Error!?ast.SubRange {
    var property_defs = std.ArrayList(Node.Index).init(self.allocator);
    defer property_defs.deinit();

    var destructure_kind = self.current_destructure_kind;

    while (true) {
        switch (self.current_token.tag) {
            .identifier => {
                try property_defs.append(try self.identifierProperty());
                destructure_kind.update(self.current_destructure_kind);
            },

            .@"[" => {
                _ = try self.next();
                const key = try self.assignExpressionNoPattern();
                _ = try self.expect(.@"]");

                const property = try self.completePropertyDef(
                    key,
                    .{ .is_computed = true },
                );
                destructure_kind.update(self.current_destructure_kind);
                try property_defs.append(property);
            },

            .numeric_literal,
            .string_literal,
            .legacy_octal_literal,
            => {
                const key_token = try self.next();
                const key = try self.addNode(
                    .{ .literal = try self.addToken(key_token) },
                    key_token.start,
                    key_token.start + key_token.len,
                );

                const property_expr = try self.completePropertyDef(key, .{});
                destructure_kind.update(self.current_destructure_kind);
                try property_defs.append(property_expr);
            },

            // generator method
            .@"*" => {
                _ = try self.next(); // eat '*'
                const key = try self.classElementName();
                const generator_method = try self.parseMethodBody(
                    key,
                    .{ .is_method = true },
                    .{ .is_generator = true },
                );
                destructure_kind.can_destruct = false;
                try property_defs.append(generator_method);
            },

            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();

                destructure_kind.update(self.current_destructure_kind);

                const start = ellipsis_tok.start;
                const end = self.nodes.items(.end)[@intFromEnum(expr)];
                try property_defs.append(try self.addNode(.{ .spread_element = expr }, start, end));

                if (self.isAtToken(.@",")) {
                    // comma is not allowed after rest element in object patterns
                    destructure_kind.setNoAssignOrDestruct();
                }
            },
            else => {
                if (self.current_token.tag.isKeyword()) {
                    const id_property = try self.identifierProperty();
                    try property_defs.append(id_property);
                    destructure_kind.update(self.current_destructure_kind);
                } else {
                    break;
                }
            },
        }

        const maybe_comma = self.peek();
        if (maybe_comma.tag == .@",") {
            _ = try self.next();
        } else {
            break;
        }
    }

    if (destructure_kind.isMalformed()) {
        // TODO: emit a diagnostic.
        return Error.InvalidObject;
    }

    self.current_destructure_kind = destructure_kind;

    if (property_defs.items.len == 0) return null;
    return try self.addSubRange(property_defs.items);
}

/// Parse an the property of an object literal or object pattern that starts with an identifier.
fn identifierProperty(self: *Self) Error!Node.Index {
    const key_token = try self.next();
    std.debug.assert(key_token.tag == .identifier or key_token.tag.isKeyword());

    const cur_token = self.current_token;
    if (cur_token.tag != .@":" and cur_token.tag != .@"(" and
        cur_token.tag != .@"," and cur_token.tag != .@"}" and
        cur_token.tag != .@"=")
    {
        if (key_token.tag == .kw_async and key_token.tag.isValidPropertyName()) {
            // handle `async f() { ... }`
            const property_key = try self.identifier(try self.next());
            const property_val = try self.parseMethodBody(
                property_key,
                .{ .is_method = true },
                .{ .is_async = true },
            );

            const end_pos = self.nodes.items(.end)[@intFromEnum(property_val)];

            // object literals with async methods are not valid object patterns.
            self.current_destructure_kind.setNoAssignOrDestruct();
            return self.addNode(.{
                .object_property = .{
                    .key = property_key,
                    .value = property_val,
                },
            }, key_token.start, end_pos);
        }

        const maybe_getter_or_setter = try self.getterOrSetter(key_token);
        if (maybe_getter_or_setter) |getter_or_setter| {
            // object literals with getters or setters are not valid object patterns.
            self.current_destructure_kind.setNoAssignOrDestruct();
            return getter_or_setter;
        }

        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "Unexpected '{s}' in property definition",
            .{self.current_token.toByteSlice(self.source)},
        );
        return Error.UnexpectedToken;
    }

    const key_end_pos = key_token.start + key_token.len;
    const key = try self.identifier(key_token);

    const cur_token_tag = self.current_token.tag;
    switch (cur_token_tag) {
        .@":", .@"(" => {
            return self.completePropertyDef(key, .{
                .is_method = cur_token_tag == .@"(",
            });
        },

        .@"=" => {
            const op_token = try self.next(); // eat '='
            if (!key_token.tag.isValidPropertyName()) {
                try self.emitBadTokenDiagnostic("property name", &key_token);
                return Error.UnexpectedToken;
            }

            const value = try self.assignmentExpression();
            const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
            const end_pos = self.nodes.items(.end)[@intFromEnum(value)];

            const assignment_pattern = ast.NodeData{
                .assignment_pattern = .{
                    .lhs = key,
                    .rhs = value,
                    .operator = try self.addToken(op_token),
                },
            };

            // 'Identifier = AssignmentExpression' is allowed in object patterns but not in object literals
            self.current_destructure_kind = DestructureKind.MustDestruct;
            return self.addNode(assignment_pattern, start_pos, end_pos);
        },

        else => {
            const kv_node = ast.PropertyDefinition{
                .key = key,
                .value = key,
                .flags = .{ .is_shorthand = true },
            };

            // { k }
            //   ^-- Is a valid property in a destructuring pattern
            self.current_destructure_kind.can_destruct = true;
            self.current_destructure_kind.can_be_assigned_to = true;
            return self.addNode(
                .{ .object_property = kv_node },
                key_token.start,
                key_end_pos,
            );
        },
    }
}

/// Tries to parse a getter or setter, assuming `token` is an identifier.
/// If no getter or setter is found, returns `null`.
/// If there is a parse error, emits a diagnostic and returns the error.
fn getterOrSetter(self: *Self, token: Token) Error!?Node.Index {
    const kind: ast.PropertyDefinitionKind = blk: {
        const token_str = token.toByteSlice(self.source);
        if (std.mem.eql(u8, token_str, "get")) {
            break :blk .get;
        }

        if (std.mem.eql(u8, token_str, "set")) {
            break :blk .set;
        }

        return null;
    };

    const key = try self.classElementName();
    return try self.parseMethodBody(
        key,
        .{ .is_method = true, .kind = kind },
        .{},
    );
}

/// Checks if a token can start a class element name.
fn canStartClassElementName(token: *const Token) bool {
    return token.tag.isValidPropertyName() or
        token.tag == .@"[" or
        token.tag == .private_identifier;
}

/// https://tc39.es/ecma262/#prod-ClassElementName
fn classElementName(self: *Self) Error!Node.Index {
    const token = try self.next();
    switch (token.tag) {
        .identifier, .private_identifier => return self.identifier(token),
        .@"[" => {
            const expr = try self.assignmentExpression();
            _ = try self.expect(.@"]");
            return expr;
        },
        .string_literal,
        .numeric_literal,
        .legacy_octal_literal,
        => {
            return self.addNode(
                .{ .literal = try self.addToken(token) },
                token.start,
                token.start + token.len,
            );
        },
        else => {
            if (token.tag.isKeyword())
                return self.identifier(token);

            try self.emitDiagnostic(
                token.startCoord(self.source),
                "Expected property name, got '{s}'",
                .{token.toByteSlice(self.source)},
            );
            return Error.UnexpectedToken;
        },
    }
}

/// Parse a method body, assuming we're at the '(' node.
/// Returns an `object_property` Node.
fn parseMethodBody(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
    fn_flags: ast.FunctionFlags,
) Error!Node.Index {
    const start_pos = self.current_token.start;
    const func_expr = try self.parseFunctionBody(
        start_pos,
        null,
        fn_flags,
        false,
    );
    const end_pos = self.nodeSpan(func_expr).end;

    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = func_expr,
        .flags = flags,
    };

    if (flags.kind == .get or flags.kind == .set) {
        // verify the number of parameters for getters and setters.
        try self.checkGetterOrSetterParams(func_expr, flags.kind);
    }

    const key_start = self.nodes.items(.start)[@intFromEnum(key)];
    return self.addNode(
        .{ .object_property = kv_node },
        key_start,
        end_pos,
    );
}

/// Verify the number of parameters in a getter or setter.
/// Emit a diagnostic, then return an error if the number is invalid.
/// A Getter must have exaclty 0 parameters, and a setter only one.
fn checkGetterOrSetterParams(
    self: *Self,
    func_expr: Node.Index,
    kind: ast.PropertyDefinitionKind,
) Error!void {
    const func = &self.getNode(func_expr).data.function_expr;
    const n_params = func.getParameterCount(self);
    if (kind == .get and n_params != 0) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "A 'get' accessor should have no parameter, but got {d}",
            .{n_params},
        );
        return Error.InvalidGetter;
    }

    if (kind == .set and n_params != 1) {
        try self.emitDiagnostic(
            self.current_token.startCoord(self.source),
            "A 'set' accessor should have exaclty one parameters, but got {d}",
            .{n_params},
        );
        return Error.InvalidSetter;
    }
}

/// Assuming that the key has been parsed, complete the property definition.
fn completePropertyDef(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
) Error!Node.Index {
    if (self.current_token.tag == .@"(") {
        self.current_destructure_kind.setNoAssignOrDestruct();
        return self.parseMethodBody(key, .{
            .is_method = true,
            .is_computed = flags.is_computed,
            .is_shorthand = flags.is_shorthand,
            .kind = flags.kind,
        }, .{});
    }

    _ = try self.expect(.@":");

    const value = try self.assignmentExpression();
    // the assignmentExpression() will have updated self.current_destructure_kind

    const start_pos = self.nodes.items(.start)[@intFromEnum(key)];
    const end_pos = self.nodes.items(.end)[@intFromEnum(value)];
    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = value,
        .flags = flags,
    };
    return self.addNode(.{ .object_property = kv_node }, start_pos, end_pos);
}

/// Parse an ArrayLiteral:
/// https://262.ecma-international.org/15.0/index.html#prod-ArrayLiteral
fn arrayLiteral(self: *Self, start_pos: u32) Error!Node.Index {
    var elements = std.ArrayList(Node.Index).init(self.allocator);
    defer elements.deinit();

    var destructure_kind = self.current_destructure_kind;

    var end_pos = start_pos;
    while (true) {
        while (self.isAtToken(.@",")) {
            // elision: https://262.ecma-international.org/15.0/index.html#prod-Elision
            const comma = try self.next();
            try elements.append(try self.addNode(
                .{ .empty_array_item = {} },
                comma.start,
                comma.start + comma.len,
            ));
        }

        if (self.isAtToken(.@"]")) {
            const end_tok = try self.next();
            end_pos = end_tok.start + end_tok.len;
            break;
        }

        switch (self.peek().tag) {
            // Spread element
            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();
                const start = ellipsis_tok.start;
                const end = self.nodes.items(.end)[@intFromEnum(expr)];
                destructure_kind.update(self.current_destructure_kind);
                if (self.isAtToken(.@",")) {
                    // "," is not allowed after rest element in array patterns
                    destructure_kind.setNoAssignOrDestruct();
                }

                try elements.append(try self.addNode(.{ .spread_element = expr }, start, end));
            },

            else => {
                const item = try self.assignmentExpression();
                destructure_kind.update(self.current_destructure_kind);
                try elements.append(item);
            },
        }

        const next_token = try self.expect2(.@",", .@"]");
        if (next_token.tag == .@"]") {
            end_pos = next_token.start + next_token.len;
            break;
        }
    }

    const nodes = try self.addSubRange(elements.items);
    self.current_destructure_kind = destructure_kind;
    return self.addNode(.{ .array_literal = nodes }, start_pos, end_pos);
}

/// Assuming the parser is at the `function` keyword,
/// parse a function expression.
fn functionExpression(
    self: *Self,
    start_pos: u32,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.next(); // eat '*'
        fn_flags.is_generator = true;
    }

    const saved_ctx = self.context;
    // 'await' and 'yield' are always allowed as
    // function expression names
    self.context.is_await_reserved = false;
    self.context.is_yield_reserved = false;

    const name_token: ?Token.Index =
        if (self.current_token.tag == .identifier or
        self.isKeywordIdentifier(self.current_token.tag))
        try self.addToken(try self.next())
    else
        null;

    self.context = saved_ctx;

    defer self.current_destructure_kind = DestructureKind.CannotDestruct;
    return self.parseFunctionBody(start_pos, name_token, fn_flags, false);
}

/// parses the arguments and body of a function expression (or declaration),
/// assuming the `function` keyword (and/or the function/method name) has been consumed.
fn parseFunctionBody(
    self: *Self,
    start_pos: u32,
    name_token: ?Token.Index,
    flags: ast.FunctionFlags,
    is_decl: bool,
) Error!Node.Index {
    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.is_yield_reserved = flags.is_generator;
    self.context.is_await_reserved = flags.is_async;
    // disallow: while (1) { function f() { break; } }
    self.context.@"break" = false;
    self.context.@"continue" = false;

    const params = try self.parseFormalParameters();
    if (flags.is_arrow) unreachable; // not supported yet :)

    // Allow return statements inside function
    const ctx = self.context;
    defer self.context = ctx;
    self.context.@"return" = true;

    // TODO: make the body a statement list.
    const body = try self.blockStatement();
    const end_pos = self.nodeSpan(body).end;

    const function_data = try self.addExtraData(
        ast.ExtraData{
            .function = .{
                .name = name_token,
                .flags = flags,
            },
        },
    );

    const function = ast.Function{
        .parameters = params,
        .body = body,
        .info = function_data,
    };

    const node_data: ast.NodeData = if (is_decl)
        .{ .function_declaration = function }
    else
        .{ .function_expr = function };
    return self.addNode(node_data, start_pos, end_pos);
}

fn parseParameter(self: *Self) Error!Node.Index {
    const param = try self.assignmentLhsExpr();
    if (!self.current_destructure_kind.can_destruct) {
        try self.emitDiagnosticOnNode(
            param,
            "function parameter must be name, assignment pattern, or rest element",
        );

        return Error.InvalidFunctionParameter;
    }

    if (!self.isAtToken(.@"=")) return param;

    const eq_op = try self.next(); // eat '='

    const defaultValue = try self.assignmentExpression();
    if (self.current_destructure_kind.must_destruct) {
        try self.emitDiagnosticOnNode(
            defaultValue,
            "Default parameter value cannot be a destructuring pattern",
        );

        return Error.InvalidFunctionParameter;
    }

    const node_starts: []u32 = self.nodes.items(.start);

    const assignment_pattern: ast.BinaryPayload = .{
        .lhs = param,
        .rhs = defaultValue,
        .operator = try self.addToken(eq_op),
    };

    return self.addNode(
        .{ .assignment_pattern = assignment_pattern },
        node_starts[@intFromEnum(param)],
        node_starts[@intFromEnum(defaultValue)],
    );
}

/// Starting with the '(' token , parse formal parameters of a function.
fn parseFormalParameters(self: *Self) Error!Node.Index {
    const lparen = try self.expect(.@"(");
    const start_pos = lparen.start;

    var params = std.ArrayList(Node.Index).init(self.allocator);
    defer params.deinit();

    const rparen = blk: {
        if (self.isAtToken(.@")")) break :blk try self.next();

        while (true) {
            if (self.isAtToken(.@"...")) {
                const rest_elem = try self.restElement();
                try params.append(rest_elem);

                if (!self.isAtToken(.@")")) {
                    try self.restParamNotLastError(&self.current_token);
                }

                break :blk try self.next();
            }

            const param = try self.parseParameter();
            try params.append(param);

            const comma_or_rpar = try self.expect2(.@",", .@")");
            if (comma_or_rpar.tag == .@")") break :blk comma_or_rpar;
        }
    };

    const end_pos = rparen.start + rparen.len;

    const param_list = if (params.items.len > 0)
        try self.addSubRange(params.items)
    else
        null;

    self.current_destructure_kind.reset();
    return self.addNode(.{ .parameters = param_list }, start_pos, end_pos);
}

/// Get a pointer to a node by its index.
/// The returned value can be invalidated by any call to `addNode`, `restoreState`, `addSubRange`.
pub fn getNode(self: *const Self, index: Node.Index) ast.Node {
    return self.nodes.get(@intFromEnum(index));
}

pub fn getExtraData(
    self: *const Self,
    index: ast.ExtraData.Index,
) *const ast.ExtraData {
    return &self.extra_data.items[@intFromEnum(index)];
}

/// From a `SubRange` object, get a slice that contains the ID for each node
/// in that list.
pub fn getSubRange(
    self: *const Self,
    from: ast.SubRange.Index,
    to: ast.SubRange.Index,
) []const Node.Index {
    const from_: usize = @intFromEnum(from);
    const to_: usize = @intFromEnum(to);
    return self.node_lists.items[from_..to_];
}

pub fn getToken(self: *const Self, index: Token.Index) Token {
    return self.tokens.items[@intFromEnum(index)];
}

/// Get the start and end byte offset of a node in the source file.
fn nodeSpan(self: *const Self, index: Node.Index) types.Span {
    const start = self.nodes.items(.start)[@intFromEnum(index)];
    const end = self.nodes.items(.end)[@intFromEnum(index)];
    return .{ .start = start, .end = end };
}

/// Return the tag that identifies the type of a node.
fn nodeTag(self: *const Self, index: Node.Index) std.meta.Tag(NodeData) {
    return std.meta.activeTag(self.nodes.items(.data)[@intFromEnum(index)]);
}

/// Parses arguments for a function call, assuming the current_token is '('
fn args(self: *Self) Error!Node.Index {
    const args_node, const start, const end = try self.parseArgs();
    return self.addNode(.{ .arguments = args_node }, start, end);
}

/// Parse arguments for a function call, then return it alongside the start and end locations.
fn parseArgs(self: *Self) Error!struct { ast.SubRange, u32, u32 } {
    // The "+In" grammar attribute.
    // This is to allow things like `for (f(a in b) in c)`
    const old_ctx = self.context;
    self.context.in = true;
    defer self.context = old_ctx;

    const start_pos = (try self.expect(.@"(")).start;

    var arg_list = std.ArrayList(Node.Index).init(self.allocator);
    defer arg_list.deinit();

    while (!self.isAtToken(.@")")) {
        if (self.isAtToken(.@"...")) {
            const spread_elem = try self.spreadElement();
            try arg_list.append(spread_elem);
        } else {
            const expr = try self.assignExpressionNoPattern();
            try arg_list.append(expr);
        }
        if (!self.isAtToken(.@","))
            break;
        _ = try self.next(); // eat ','
    }

    const close_paren = try self.expect(.@")"); // eat closing ')'
    const end_pos = close_paren.start + close_paren.len;

    return .{ try self.addSubRange(arg_list.items), start_pos, end_pos };
}

/// make a right associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeRightAssoc(
    comptime toktag: Token.Tag,
    comptime l: *const ParseFn,
) *const ParseFn {
    const Parselet = struct {
        fn parseFn(self: *Self) Error!Node.Index {
            var node = try l(self);

            var token = self.peek();
            while (true) : (token = self.peek()) {
                if (token.tag != toktag) break;
                _ = try self.next();

                const rhs = try parseFn(self);
                const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
                const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
                node = try self.addNode(.{
                    .binary_expr = .{
                        .lhs = node,
                        .rhs = rhs,
                        .operator = try self.addToken(token),
                    },
                }, start_pos, end_pos);
            }

            return node;
        }
    };

    return &Parselet.parseFn;
}

/// make a left associative parse function for an infix operator represented
/// by tokens of tag `toktag`
fn makeLeftAssoc(
    comptime tag_min: Token.Tag,
    comptime tag_max: Token.Tag,
    comptime nextFn: *const ParseFn,
) *const ParseFn {
    const min: u32 = @intFromEnum(tag_min);
    const max: u32 = @intFromEnum(tag_max);

    const S = struct {
        fn parseFn(self: *Self) Error!Node.Index {
            var node = try nextFn(self);

            var token = self.peek();
            while (true) : (token = self.peek()) {
                const itag: u32 = @intFromEnum(token.tag);
                if (itag >= min and itag <= max) {
                    self.current_destructure_kind.setNoAssignOrDestruct();

                    _ = try self.next();
                    const rhs = try nextFn(self);
                    const start_pos = self.nodes.items(.start)[@intFromEnum(node)];
                    const end_pos = self.nodes.items(.end)[@intFromEnum(rhs)];
                    node = try self.addNode(.{
                        .binary_expr = .{
                            .lhs = node,
                            .rhs = rhs,
                            .operator = try self.addToken(token),
                        },
                    }, start_pos, end_pos);
                } else {
                    break;
                }
            }

            return node;
        }
    };

    return &S.parseFn;
}

// -----
// Tests
// -----

const t = std.testing;

const pretty = @import("./pretty.zig");
fn runTestOnFile(tests_dir: std.fs.Dir, file_path: []const u8) !void {
    const source_code = try tests_dir.readFileAlloc(
        t.allocator,
        file_path,
        std.math.maxInt(u32),
    );
    defer t.allocator.free(source_code);

    var parser = try Self.init(t.allocator, source_code, .{ .source_type = .script });
    defer parser.deinit();

    const root_node = parser.parse() catch |err| {
        if (std.mem.startsWith(u8, std.mem.trim(u8, source_code, "\n\t "), "// ERROR")) {
            // error was expected.
            // TODO: check the error message as well.
            return;
        }

        for (parser.diagnostics.items) |diagnostic| {
            std.log.err("({d}:{d}): {s}\n", .{
                diagnostic.coord.line,
                diagnostic.coord.column,
                diagnostic.message,
            });
        }
        return err;
    };

    // 1. prettify the AST as a JSON string
    const ast_json = try pretty.toJsonString(t.allocator, &parser, root_node);
    defer t.allocator.free(ast_json);

    // 2. For every `<filename>.js`, read the corresponding `<filename>.json` file
    const json_file_path = try std.mem.concat(
        t.allocator,
        u8,
        &.{ file_path[0 .. file_path.len - 3], ".json" },
    );
    defer t.allocator.free(json_file_path);

    const expected_ast_json = tests_dir.readFileAlloc(
        t.allocator,
        json_file_path,
        std.math.maxInt(u32),
    ) catch |err| {
        std.debug.print("failed to read file: {s}\n", .{json_file_path});
        return err;
    };
    defer t.allocator.free(expected_ast_json);

    const expected_ast_json_trimmed = std.mem.trim(u8, expected_ast_json, "\n\t ");

    // 3. ensure the AST JSON is equal to the expected JSON
    try t.expectEqualStrings(expected_ast_json_trimmed, ast_json);
}

test StringHelper {
    _ = StringHelper;
}

test parse {
    var root_dir = std.fs.cwd();
    var tests_dir = try root_dir.openDir("parser-tests", .{ .iterate = true });
    defer tests_dir.close();

    var iter = tests_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .directory) {
            continue;
        }

        var dir = try tests_dir.openDir(entry.name, .{ .iterate = true });
        defer dir.close();

        var d_iter = dir.iterate();
        while (try d_iter.next()) |test_case_entry| {
            std.debug.assert(test_case_entry.kind == .file);
            if (!std.mem.endsWith(u8, test_case_entry.name, ".js")) continue;
            runTestOnFile(dir, test_case_entry.name) catch |err| {
                std.log.err("Error comparing ASTs for file: {s}\n", .{test_case_entry.name});
                return err;
            };
        }
    }
}
