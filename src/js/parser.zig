// A recursive decent parser for JavaScript.

const util = @import("util");
const std = @import("std");

const Tokenizer = @import("./tokenize.zig");
const ast = @import("./ast.zig");

const Token = @import("./token.zig").Token;
const TokenMask = @import("./token.zig").Mask;

const types = util.types;
const Node = ast.Node;
const NodeData = ast.NodeData;
pub const Tree = ast.Tree;

const assert = std.debug.assert;
const panic = std.debug.panic;
const meta = std.meta;

const Self = @This();

const TokenWithId = struct {
    token: Token,

    /// Byte offset where the token starts.
    inline fn startByte(self: TokenWithId) u32 {
        return self.token.start;
    }

    /// Byte offset just past the token.
    inline fn endByte(self: TokenWithId) u32 {
        return self.token.start + self.token.len;
    }

    /// Byte span of the token.
    inline fn span(self: TokenWithId) ast.Span {
        return .{ .start = self.startByte(), .end = self.endByte() };
    }
};

/// Byte span covering two tokens: from the start of [first]
/// to just past the end of [last].
inline fn spanBetween(first: TokenWithId, last: TokenWithId) ast.Span {
    return .{ .start = first.startByte(), .end = last.endByte() };
}

const ParseError = error{
    UnexpectedToken,
    /// `async` keyword used on a property that isn't a method
    InvalidAsyncProperty,
    /// Keywords cannot be used as shorthand property names
    InvalidKeywordShorthandProperty,
    /// 'static' modifier used twice on a class field or method
    DuplicateStaticModifier,
    /// `async` keyword used on a getter or setter
    AsyncGetterOrSetter,
    /// Invalid modifier like 'async'
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
    /// Using '??' with '||' or '&&' without parentheses
    InvalidCoalesceExpression,
    /// Invalid operand for a rest expression (e.g: ...1)
    InvalidRestOperand,
    /// Invalid operand for an update expression (e.g: 1++)
    InvalidUpdateOperand,
    // Incorrect use of a meta property like 'new.target'
    InvalidMetaProperty,
    /// 5 = ...
    InvalidAssignmentTarget,
    ///  (1) => ...
    InvalidArrowParameters,
    MalformedArrowFnBody,
    /// Saw a '()' without a '=>'
    MissingArrow,
    /// Setter must have exactly one parameter
    InvalidSetter,
    /// Setter cannot have rest parameters
    InvalidSetterRestParam,
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
    /// 'let' bound lexicall (e.g: `const let = ...`)
    LexicallyBoundLet,
    /// let x = { x =  1} <- pattern where an object literal should've been
    UnexpectedPattern,
    /// let x = 5 let 6 = 5 <- no ';' between statements
    ExpectedSemicolon,
    /// `export` keyword without a following declaration
    ExpectedExport,
    /// `import` keyword without a following specifier
    ExpectedImportSpecifier,
    /// let [a, ...as, b] = [1, 2, 3] <- 'as' must be the last
    RestElementNotLast,
    /// Rest element must be the last item in a function parameter list
    RestElementNotLastInParams,
    /// let { x, y } <- destructuring pattern must have an initializer
    MissingInitializerInBinding,
    /// const x <- const declaration must have an initializer
    MissingInitializerInConst,
    /// Multiple default clauses in a switch statement.
    /// switch (x) { default: 1 default: 2 }
    MultipleDefaults,
    /// Class definition with multiple constructors
    MultipleConstructors,
    /// Classes cannot have a field named ""
    InvalidClassFieldNamedConstructor,
    /// "with" statement used in strict mode.
    WithInStrictMode,
    /// Attempt to delete a local variable in strict mode.
    IllegalDeleteInStrictMode,
    /// Number with a leading '0' in strict mode
    IllegalStrictModeNumber,
    /// Legacy octal number in strict mode.
    IllegalStrictModeOctal,
    /// Invalid expression or pattern inside '(...)'
    InvalidExpressionInsideParens,
    JsxExpectedTagName,
    JsxExpectedGt,
    /// Saw a '<' in non-jsx source
    LAngleWithoutJsx,

    TypeScriptNotImplemented,
    // The parser instance has already been used
    AlreadyParsed,
};

pub const Error =
    ParseError ||
    Tokenizer.Error ||
    error{ OutOfMemory, Overflow };

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
pub const ParseContext = packed struct(u16) {
    @"return": bool = false,
    @"break": bool = false,
    @"continue": bool = false,
    is_await_reserved: bool = false,
    is_yield_reserved: bool = false,
    /// Are we parsing a module? (`false` = parsing a script).
    module: bool = false,
    /// Are we in strict mode?
    /// NOTE: The parser must not modify this directly.
    /// Instead, `contextEnterStrictMode` and `contextExitStrictMode` should be used.
    strict: bool = false,
    /// Are `in` expressions allowed?
    /// Used to parse for-in loops.
    in: bool = true,
    // padding
    _: u8 = 0,
};

pub const SourceType = enum { script, module };
/// Configuration options for the parser.
pub const Config = struct {
    /// Whether we're parsing a script or a module.
    source_type: SourceType = .module,
    /// Enable strict mode in the entire file
    strict_mode: bool = false,
    /// Enable JSX support
    jsx: bool = true,
    /// Enable typescript support
    typescript: bool = false,
    /// Add a special ast node for expressions wrapped in '()'.
    /// Parses `(a)` as `parenthesized_expression {  identifier }` instead of  '{ identifier }'
    preserve_parens: bool = true,
    /// Populate `tree.tokens` with every significant token (plus comments)
    /// in source order. Nodes carry byte spans; the list is a pure output
    /// for consumers that want to walk the token stream.
    tokens: bool = false,

    pub fn isTsx(self: Config) bool {
        return self.jsx and self.typescript;
    }
};

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
nodes: *std.MultiArrayList(Node),
/// Extra information about a node, if any. See: `ast.ExtraData`.
extra_data: *std.ArrayList(ast.ExtraData),
/// List of tokens that are necessary to keep around
/// e.g - identifiers, literals, function names, etc.
tokens: *std.ArrayList(Token),
/// Arguments for function calls, new-expressions, etc.
node_lists: *std.ArrayList(Node.Index),
/// List of error messages and warnings generated during parsing.
diagnostics: std.ArrayList(Diagnostic),
/// Temporary array-list for intermediate allocations
/// throughout the parser, e.g: list of case-blocks, list of statements.
scratch: std.ArrayList(Node.Index),
/// The token that we're currently at.
/// Calling `next()` or `peek()` will return this token.
current: TokenWithId,
/// Line number of the token that was consumed previously.
/// When being accessed, this property can be thought of as the starting line
/// of the first token in the expression/statement that was last parsed.
/// Useful for Automatic Semicolon Insertion.
prev_token_line: u32 = 0,
/// The current nesting depth of JSX expressions.
jsx_nesting_depth: u32 = 0,
/// The current grammatical context of the parser. See struct `ParseContext`.
context: ParseContext = .{},
/// The kind of destructuring that is allowed for the expression
/// that was just parsed by the parser.
current_destructure_kind: DestructureKind = @bitCast(@as(u8, 0)),
/// Whether we're parsing a module or a script.
source_type: SourceType,
/// The parse result
tree: ?*Tree,
/// Used in `asyncExpression`.
/// Reset to `false` after an async arrow function is parsed.
is_current_arrow_func_async: bool = false,

config: Config,

/// Bit-flags to keep track of whether the
/// most recently parsed expression can be destructured.
const DestructureKind = packed struct(u8) {
    pub const MustDestruct = DestructureKind{
        .can_destruct = true,
        .can_be_assigned_to = true,
        .must_destruct = true,
        .is_simple_expression = false,
    };

    pub const CannotDestruct = DestructureKind{
        .can_destruct = false,
        .must_destruct = false,
        .can_be_assigned_to = false,
        .is_simple_expression = false,
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
    /// Whether the expression matches one of the following productions:
    /// ```
    /// Identifier
    /// CallExpression [ Expression ]
    /// CallExpression . IdentifierName
    /// CallExpression . PrivateIdentifier
    /// MemberExpression [ Expression ]
    /// MemberExpression . IdentifierName
    /// SuperProperty
    /// MemberExpression . PrivateIdentifier
    /// ```
    /// This is used to figure out the "AssignmentTargetType" of an expression:
    /// https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype
    is_simple_expression: bool = true,
    /// padding
    _: u4 = 0,

    /// Update the flags to indiciate that the current expression
    /// can neither be destructured nor assigned to.
    pub inline fn setNoAssignOrDestruct(self: *DestructureKind) void {
        self.can_destruct = false;
        self.can_be_assigned_to = false;
        self.is_simple_expression = false;
    }

    /// Sets it to a state where that implies an expression
    /// can neither be destructured, nor be assigned to.
    pub inline fn reset(self: *DestructureKind) void {
        self.* = .{
            .can_destruct = false,
            .can_be_assigned_to = false,
            .must_destruct = false,
            .is_simple_expression = false,
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
        if (!other.is_simple_expression) self.is_simple_expression = false;
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
    // First, allocate a struct to store the resulting parse tree.
    var parse_tree = try allocator.create(Tree);
    parse_tree.* = Tree{
        .allocator = allocator,
        .source = source,
        .root = Node.Index.empty,
        .nodes = .{},
        .node_indices = try std.ArrayList(Node.Index).initCapacity(allocator, 32),
        .extras = try std.ArrayList(ast.ExtraData).initCapacity(allocator, 32),
        .tokens = try std.ArrayList(Token).initCapacity(allocator, 1024),
        .string_pool = try util.StringPool.init(allocator),
        .source_type = config.source_type,
    };

    try parse_tree.nodes.ensureTotalCapacity(allocator, 512);

    // Pre-reserve the big lists from source size. Average non-minified
    // JavaScript holds roughly one AST node per 8 bytes of source and one
    // node index per 28. Minified code is denser, so these are lower
    // bounds: the lists grow when needed, each growth copying the list
    // and wasting the old arena buffer until deinit.
    if (source.len > 4096) {
        try parse_tree.nodes.ensureTotalCapacity(allocator, source.len / 8);
        try parse_tree.node_indices.ensureTotalCapacity(allocator, source.len / 28);
        if (config.tokens) {
            try parse_tree.tokens.ensureTotalCapacity(allocator, source.len / 6);
        }
    }

    // Pre-reserve the big lists based on source size.
    // Growing these by doubling inside an arena is expensive: each growth
    // copies the whole list AND leaks the old buffer until arena deinit,
    // roughly doubling both memmove traffic and peak memory.
    if (source.len > 4096) {
        // Observed on typescript.js (9 MB): 1.35M stored tokens (comments +
        // significant, ~6.7 bytes/token), 1.06M nodes (~8.5 bytes/node),
        // 323K node indices (~28 bytes/index).
        const est_tokens = source.len / 6;
        const est_nodes = source.len / 8;
        const est_node_indices = source.len / 28;
        try parse_tree.tokens.ensureTotalCapacity(allocator, est_tokens);
        try parse_tree.nodes.ensureTotalCapacity(allocator, est_nodes);
        try parse_tree.node_indices.ensureTotalCapacity(allocator, est_node_indices);
    }

    var self = Self{
        .tokenizer = try Tokenizer.init(source, config),
        // SAFETY: initialized below in the call to `self.startTokenizer()`
        .current = undefined,
        .config = config,

        .allocator = allocator,
        .source = source,
        .tree = parse_tree,

        .source_type = config.source_type,

        .nodes = &parse_tree.nodes,
        .node_lists = &parse_tree.node_indices,
        .extra_data = &parse_tree.extras,
        .tokens = &parse_tree.tokens,

        .diagnostics = try std.ArrayList(Diagnostic).initCapacity(allocator, 2),
        .scratch = try std.ArrayList(Node.Index).initCapacity(allocator, 32),
    };

    errdefer self.deinit();

    // "module"s are always parsed in strict mode.
    const is_strict = config.strict_mode or config.source_type == SourceType.module;
    _ = self.contextEnterStrictMode(is_strict);

    if (config.typescript) {
        return Error.TypeScriptNotImplemented;
    }

    // the `null` node always lives at index-0.
    // see: ast.NodeData.none
    const i = try self.addNode(.{ .none = {} }, .{ .start = 0, .end = 0 });
    assert(i == Node.Index.empty);

    // this call will initialize `current_token`.
    try self.startTokenizer();
    return self;
}

pub fn deinit(self: *Self) void {
    if (self.tree) |tree| {
        tree.deinit();
        self.allocator.destroy(tree);
    }

    self.scratch.deinit(self.allocator);
    for (self.diagnostics.items) |d| {
        self.allocator.free(d.message);
    }
    self.diagnostics.deinit(self.allocator);
}

pub const Result = struct {
    tree: *Tree,
    allocator: std.mem.Allocator,
    pub fn deinit(self: *Result) void {
        self.tree.deinit();
        return self.allocator.destroy(self.tree);
    }
};

/// Parse the input program.
/// IMPORTANT: this function can only be called *once* on a single parser instance.
/// Calling it more than once returns an error.
/// Returned value is owned by the caller.
pub fn parse(self: *Self) Error!Result {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    if (self.source_type == .module) {
        while (!self.isAtToken(.eof)) {
            const stmt = try self.moduleItem();
            try self.scratch.append(self.allocator, stmt);
        }
    } else {
        // first, check for "use strict" directive
        const has_use_strict =
            blk: while (!self.isAtToken(.eof)) {
                const stmt = try self.statementOrDeclaration();
                try self.scratch.append(self.allocator, stmt);

                switch (self.checkDirective(stmt)) {
                    .use_strict => break :blk true,
                    .directive => {},
                    .not_directive => break :blk false,
                }
            } else break :blk false;

        const ctx = self.contextEnterStrictMode(has_use_strict);
        defer self.contextExitStrictMode(ctx);

        while (!self.isAtToken(.eof)) {
            const stmt = try self.statementOrDeclaration();
            try self.scratch.append(self.allocator, stmt);
        }
    }

    const statements = self.scratch.items[prev_scratch_len..];
    const stmt_list = try self.addSubRange(statements);
    const root_node = try self.addNode(.{ .program = stmt_list }, .{ .start = 0, .end = @intCast(self.source.len) });

    const tree = self.tree orelse return Error.AlreadyParsed;
    tree.root = root_node;

    self.tree = null; // avoid free-ing the tree when parser is deinit-ed.
    return Result{ .allocator = self.allocator, .tree = tree };
}

pub fn moduleItem(self: *Self) Error!Node.Index {
    return switch (self.current.token.tag) {
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
    assert(import_kw.token.tag == .kw_import);

    // import "foo";
    if (self.isAtToken(.string_literal)) {
        return self.completeImportDeclaration(import_kw.startByte(), &.{});
    }

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    // If there is a trailing ',' after the default import,
    // we must parse a list of import specifiers inside '{}'
    var trailing_comma = false;

    // Whether we have at least one import (either default, namespace, or specifier list)
    // This is to avoid parsing `import from "foo";` as a valid import statement.
    var has_imports = false;

    const cur = self.current.token.tag;
    if (cur.isIdentifier() or self.isKeywordIdentifier(cur)) {
        has_imports = true;
        const default_specifier = try self.defaultImportSpecifier();
        try self.scratch.append(self.allocator, default_specifier);

        trailing_comma = self.isAtToken(.@",");
        if (trailing_comma) {
            _ = try self.nextToken();
        }
    }

    // import * as foo from "foo"
    if (self.isAtToken(.@"*")) {
        has_imports = true;
        const specifier = try self.starImportSpecifier();
        try self.scratch.append(self.allocator, specifier);

        _ = try self.expectToken(.kw_from);
        return self.completeImportDeclaration(
            import_kw.startByte(),
            self.scratch.items[prev_scratch_len..],
        );
    }

    if (trailing_comma or self.isAtToken(.@"{")) {
        has_imports = true;
        _ = try self.expectToken(.@"{");

        while (self.current.token.tag != .@"}") {
            const specifier = try self.importSpecifier();
            try self.scratch.append(self.allocator, specifier);
            const comma_or_rb = try self.expect2(.@",", .@"}");
            if (comma_or_rb.token.tag == .@"}")
                break;
        } else {
            _ = try self.next(); // eat '}'
        }
    }

    if (!has_imports) {
        return self.errorOnToken(import_kw.token, ParseError.ExpectedImportSpecifier);
    }

    const specifiers = self.scratch.items[prev_scratch_len..];
    _ = try self.expectToken(.kw_from);
    return self.completeImportDeclaration(import_kw.startByte(), specifiers);
}

/// Assuming 'current_token' is '*', parse a import namespace specifier
/// like `* as Identifier`
fn starImportSpecifier(self: *Self) Error!Node.Index {
    const star_token = try self.expect(.@"*");
    _ = try self.expectToken(.kw_as);

    const name_token = try self.expectIdentifier();
    const name = try self.bindingIdentifier(name_token.span());

    const specifier = try self.addNode(.{
        .import_namespace_specifier = .{ .name = name },
    }, spanBetween(star_token, name_token));

    return specifier;
}

/// Assuming everything upto 'from' has been consumed,
/// parse the source of the import declaration, and return the import node.
fn completeImportDeclaration(
    self: *Self,
    start_byte: u32,
    specifiers: []const Node.Index,
) Error!Node.Index {
    const source = try self.stringLiteral();
    var end_pos = self.nodes.items(.end)[@backingInt(source)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(.{
        .import_declaration = .{
            .source = source,
            .specifiers = try self.addSubRange(specifiers),
        },
    }, .{ .start = start_byte, .end = end_pos });
}

/// Parse the next identifier token as a default import specifier.
fn defaultImportSpecifier(self: *Self) Error!Node.Index {
    const name_token = try self.next();
    const name = try self.bindingIdentifier(name_token.span());
    return self.addNode(.{
        .import_default_specifier = .{ .name = name },
    }, spanBetween(name_token, name_token));
}

fn importSpecifier(self: *Self) Error!Node.Index {
    const name, const name_token = blk: {
        if (self.isAtToken(.string_literal)) {
            const name_tok = try self.next();
            break :blk .{ try self.stringLiteralFromToken(name_tok.span()), name_tok };
        }

        const name_tok = try self.expectIdOrKeyword();

        // `import {foo as bar }` // <- `foo` is an identifier node
        // `import { foo }` // <- `foo` is a BINDING identifier node
        const name_node = try if (self.isAtToken(.kw_as))
            self.identifier(name_tok.span())
        else
            self.bindingIdentifier(name_tok.span());

        break :blk .{ name_node, name_tok };
    };

    if (self.isAtToken(.kw_as) or !self.isIdentifier(name_token.token.tag)) {
        _ = try self.expectToken(.kw_as); // eat 'as'
        const alias_token = try self.expectIdentifier();
        const alias = try self.bindingIdentifier(alias_token.span());

        return self.addNode(.{
            .import_specifier = .{
                .local = alias,
                .imported = null,
            },
        }, spanBetween(name_token, alias_token));
    }

    return self.addNode(.{ .import_specifier = .{ .local = name, .imported = null } }, spanBetween(name_token, name_token));
}

/// A string literal or an identifier reference.
/// https://262.ecma-international.org/15.0/index.html#prod-ModuleExportName
fn moduleExportName(self: *Self) Error!Node.Index {
    if (self.isAtToken(.string_literal))
        return self.stringLiteral();

    const name_token = try self.next();
    if (!name_token.token.tag.isIdentifier() and
        name_token.token.tag != .kw_default and
        !self.isKeywordIdentifier(name_token.token.tag))
    {
        try self.emitBadTokenDiagnostic("an identifier", &name_token.token);
        return Error.UnexpectedToken;
    }

    return self.identifierReference(name_token.span());
}

/// Parse an 'export' declaration:
/// https://tc39.es/ecma262/#prod-ExportDeclaration
fn exportDeclaration(self: *Self) Error!Node.Index {
    const export_kw = try self.next();
    assert(export_kw.token.tag == .kw_export);

    if (self.isAtToken(.kw_default)) {
        return try self.defaultExport(export_kw);
    }

    const maybe_decl = blk: {
        switch (self.current.token.tag) {
            .kw_var, .kw_let, .kw_const => {
                const var_kw = try self.next();
                break :blk try self.variableStatement(var_kw);
            },
            else => break :blk try self.declarationStatement(),
        }
    };

    if (maybe_decl) |decl| {
        return self.addNode(.{ .export_declaration = .{ .declaration = decl } }, .{ .start = export_kw.startByte(), .end = self.nodes.items(.end)[@backingInt(decl)] });
    }

    var end_pos = export_kw.endByte();

    if (self.isAtToken(.@"{")) {
        _ = try self.expectToken(.@"{");
        const specifiers = try self.namedExportList();
        const rbrace = try self.expect(.@"}");
        end_pos = rbrace.endByte();

        // A "from" after an export specifier list implies
        // an export statement like:
        // ```js
        // export { x, y } from "foo";
        // ```
        if (self.isAtToken(.kw_from)) {
            self.rewriteRefsInExportList(specifiers);

            _ = try self.expect(.kw_from);
            end_pos = self.current.endByte();
            const source = try self.stringLiteral();

            end_pos = try self.semiColon(end_pos);
            return self.addNode(.{
                .export_from_declaration = .{
                    .specifiers = specifiers,
                    .source = source,
                },
            }, .{ .start = export_kw.startByte(), .end = end_pos });
        }

        end_pos = try self.semiColon(end_pos);
        const decl = ast.NodeData{
            .export_list_declaration = .{ .specifiers = specifiers },
        };
        return self.addNode(decl, .{ .start = export_kw.startByte(), .end = end_pos });
    }

    if (self.isAtToken(.@"*")) {
        return self.starExportDeclaration(export_kw);
    }

    return self.errorOnToken(self.current.token, ParseError.ExpectedExport);
}

/// Parse an export all statement, assuming the `export` token has been consumed already
/// `export * as foo from "module"`
fn starExportDeclaration(self: *Self, export_kw: TokenWithId) Error!Node.Index {
    _ = try self.expectToken(.@"*");

    const name = blk: {
        if (self.isAtToken(.kw_as)) {
            _ = try self.nextToken();
            const name_token = try self.next();
            if (!(name_token.token.tag.isIdentifier() or
                self.isKeywordIdentifier(name_token.token.tag)))
            {
                try self.emitBadTokenDiagnostic("an identifier", &name_token.token);
                return Error.UnexpectedToken;
            }

            break :blk try self.identifier(name_token.span());
        }

        break :blk null;
    };

    _ = try self.expectToken(.kw_from);

    const source = try self.stringLiteral();
    var end_pos = self.nodes.items(.end)[@backingInt(source)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(.{
        .export_all_declaration = .{
            .name = name,
            .source = source,
        },
    }, .{ .start = export_kw.startByte(), .end = end_pos });
}

/// After the `export` keyword has already been consumed,
/// parse a default export statement, assuming the parser is currently
/// on the 'default' keyword.
fn defaultExport(self: *Self, export_kw: TokenWithId) Error!Node.Index {
    const default_kw = try self.next();
    assert(default_kw.token.tag == .kw_default);
    var end_pos = default_kw.endByte();

    const exported = blk: {
        switch (self.current.token.tag) {
            .kw_class => {
                const class = try self.classExpression();
                end_pos = self.nodes.items(.end)[@backingInt(class)];
                break :blk class;
            },
            .kw_function => {
                const func_kw = try self.next();
                const func = try self.functionExpression(func_kw.startByte(), .{});
                end_pos = self.nodes.items(.end)[@backingInt(func)];
                break :blk func;
            },
            .kw_async => {
                const async_line = self.current.token.line;
                const lookahead = try self.lookAhead();
                if (lookahead.tag == .kw_function and lookahead.line == async_line) {
                    const async_token = try self.next(); // eat 'async'
                    _ = try self.nextToken(); // eat 'function'
                    const func = try self.functionExpression(
                        async_token.startByte(),
                        .{ .is_async = true },
                    );
                    end_pos = self.nodes.items(.end)[@backingInt(func)];
                    break :blk func;
                }
                const expr = try self.assignExpressionNoPattern();
                end_pos = self.nodes.items(.end)[@backingInt(expr)];
                end_pos = try self.semiColon(end_pos);
                break :blk expr;
            },
            else => {
                const expr = try self.assignExpressionNoPattern();
                end_pos = self.nodes.items(.end)[@backingInt(expr)];
                end_pos = try self.semiColon(end_pos);
                break :blk expr;
            },
        }
    };

    return self.addNode(.{
        .export_declaration = .{
            .declaration = exported,
            .default = true,
        },
    }, .{ .start = export_kw.startByte(), .end = end_pos });
}

/// Parse a list of export specifiers:
/// https://tc39.es/ecma262/#prod-ExportsList
fn namedExportList(self: *Self) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (self.current.token.tag != .@"}") {
        const specifier = try self.exportSpecifier();
        try self.scratch.append(self.allocator, specifier);
        if (self.isAtToken(.@"}")) break;
        _ = try self.expectToken(.@",");
    }

    const specifiers = self.scratch.items[prev_scratch_len..];
    return try self.addSubRange(specifiers);
}

/// Go through all specifiers in an export declaration, and convert
/// every `identifier_reference` to an `identifier`.
/// This done to appropriately parse cases like:
/// ```js
/// export { foo, bar as y } from "baz"
/// ```
/// Before we've seen the `from` keyword, we don't know if `foo` and `bar`
/// are identifiers (i.e references into the "baz" file), or whether they're
/// `identifier_reference`s, referencing top-level variables declared within the same file.
/// So, we parse them as `identifier_reference` nodes, and then rewrite them
/// if we see a `from` keyword after the specifier list.
fn rewriteRefsInExportList(self: *Self, specifiers: ast.SubRange) void {
    const from: usize = @backingInt(specifiers.from);
    const to: usize = @backingInt(specifiers.to);
    const node_pls: []ast.NodeData = self.nodes.items(.data);
    for (from..to) |i| {
        const node_id = self.node_lists.items[i];
        const node_pl = &node_pls[@backingInt(node_id)];
        assert(meta.activeTag(node_pl.*) == .export_specifier);

        identifierRefToIdentifier(node_pls, node_pl.export_specifier.local);

        if (node_pl.export_specifier.exported) |exported_id| {
            identifierRefToIdentifier(node_pls, exported_id);
        }
    }
}

/// Convert an existing `identifier_reference` node to an `identifier` node.
/// This just updates the tag of the node's `NodeData` union.
fn identifierRefToIdentifier(node_pls: []NodeData, id: Node.Index) void {
    const pl = &node_pls[@backingInt(id)];
    assert(meta.activeTag(pl.*) == .identifier_reference);
    const token_idx = pl.identifier_reference;
    pl.* = .{ .identifier = token_idx };
}

/// Parse an export specifier.
/// `export { *x as x_alias* } from "foo"`
fn exportSpecifier(self: *Self) Error!Node.Index {
    const local = try self.moduleExportName();

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(local)];
    var end_pos = nodes.items(.end)[@backingInt(local)];

    const alias = blk: {
        if (!self.isAtToken(.kw_as)) break :blk null;

        _ = try self.nextToken(); // eat 'as'
        const alias_ = try self.moduleExportName();
        end_pos = self.nodes.items(.end)[@backingInt(alias_)];
        break :blk alias_;
    };

    return self.addNode(.{
        .export_specifier = .{
            .local = local,
            .exported = alias,
        },
    }, .{ .start = start_pos, .end = end_pos });
}

/// https://tc39.es/ecma262/#prod-Statement
fn statementOrDeclaration(self: *Self) Error!Node.Index {
    if (try self.declarationStatement()) |decl| return decl;
    return self.statement();
}

fn statement(self: *Self) Error!Node.Index {
    return switch (self.current.token.tag) {
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
        .kw_var => self.variableStatement(try self.next()),
        .kw_try => self.tryStatement(),
        .kw_switch => self.switchStatement(),
        .kw_with => self.withStatement(),
        .kw_throw => self.throwStatement(),
        .kw_function, .kw_class => {
            try self.emitBadTokenDiagnostic("statement", &self.current.token);
            return Error.UnexpectedToken;
        },
        .kw_async => {
            const lookahead = try self.lookAhead();
            if (lookahead.tag == .kw_function) {
                try self.emitBadTokenDiagnostic("statement", &self.current.token);
                return Error.UnexpectedToken;
            }

            return self.labeledOrExpressionStatement();
        },
        else => self.labeledOrExpressionStatement(),
    };
}

/// If the input starts with `Identifier :`, parse a labeled statement,
/// otherwise parse an expression statement.
fn labeledOrExpressionStatement(self: *Self) Error!Node.Index {
    const cur = self.current.token.tag;
    if ((cur.isIdentifier() or self.isKeywordIdentifier(cur)) and
        self.peekAfterTriviaIs(':'))
    {
        return self.labeledStatement();
    }

    return self.expressionStatement();
}

/// https://262.ecma-international.org/15.0/index.html#prod-LabelledStatement
/// LabeledStatement:
///     LabelIdentifier ':' LabelledItem
fn labeledStatement(self: *Self) Error!Node.Index {
    const label = try self.next();
    _ = try self.expectToken(.@":");

    const ctx = self.context;
    defer self.context = ctx;

    self.context.@"break" = true;

    const body = try self.labeledItem();
    const end_pos = self.nodes.items(.end)[@backingInt(body)];

    return self.addNode(.{
        .labeled_statement = .{
            .body = body,
            .label = try self.identifier(label.span()),
        },
    }, .{ .start = label.startByte(), .end = end_pos });
}

/// https://262.ecma-international.org/15.0/index.html#prod-LabelledItem
/// LabelledItem:
///     Statement
///     FunctionDeclaration
fn labeledItem(self: *Self) Error!Node.Index {
    if (self.isAtToken(.kw_function)) {
        const lookahead = try self.lookAhead();
        if ((self.context.strict or self.source_type == .script) and lookahead.tag != .@"*") {
            const fn_token = try self.next();
            return self.functionDeclaration(fn_token.startByte(), .{});
        }

        try self.emitDiagnosticOnToken(
            self.current.token,
            "{s} cannot be labeled",
            .{if (lookahead.tag == .@"*") "generators" else "function declarations"},
        );

        return Error.UnexpectedToken;
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
    switch (self.current.token.tag) {
        .kw_class => return try self.classDeclaration(),
        .kw_function => {
            const fn_token = try self.next();
            return try self.functionDeclaration(fn_token.startByte(), .{});
        },
        .kw_async => {
            if ((try self.lookAhead()).tag == .kw_function) {
                const async_token = try self.next(); // eat 'async'
                _ = try self.nextToken(); // eat 'function'
                return try self.functionDeclaration(async_token.startByte(), .{ .is_async = true });
            }
            return null;
        },
        .kw_let => {
            const let_kw = try self.startLetBinding() orelse return null;
            return try self.variableStatement(let_kw);
        },
        .kw_var, .kw_const => return try self.variableStatement(try self.next()),
        else => return null,
    }
}

/// https://tc39.es/ecma262/#sec-class-definitions
fn classDeclaration(self: *Self) Error!Node.Index {
    const class_kw = try self.next();
    assert(class_kw.token.tag == .kw_class);

    const name_token = try self.next();
    if (!name_token.token.tag.isIdentifier() and
        !self.isKeywordIdentifier(name_token.token.tag))
    {
        try self.emitBadTokenDiagnostic("a class name", &name_token.token);
        return Error.UnexpectedToken;
    }

    const name = try self.bindingIdentifier(name_token.span());

    const super_class: Node.Index, const meta_end_pos: u32 = blk: {
        if (self.current.token.tag == .kw_extends) {
            const heritage = try self.classHeritage();
            break :blk .{ heritage, self.nodes.items(.end)[@backingInt(heritage)] };
        }

        break :blk .{ Node.Index.empty, name_token.endByte() };
    };

    const class_body = try self.classBody();
    const rb = try self.expect(.@"}");

    const class_meta = try self.addNode(ast.NodeData{
        .class_meta = ast.ClassMeta{
            .name = name,
            .super_class = super_class,
        },
    }, .{ .start = name_token.startByte(), .end = meta_end_pos });

    return try self.addNode(.{
        .class_declaration = .{
            .meta = class_meta,
            .body = class_body,
        },
    }, spanBetween(class_kw, rb));
}

fn classExpression(self: *Self) Error!Node.Index {
    const class_kw = try self.next();
    assert(class_kw.token.tag == .kw_class);

    const name, const meta_start_pos = blk: {
        const cur = self.current.token.tag;
        if (cur.isIdentifier() or self.isKeywordIdentifier(cur)) {
            const name_token = try self.next();
            break :blk .{ try self.bindingIdentifier(name_token.span()), name_token.endByte() };
        }

        break :blk .{ Node.Index.empty, class_kw.endByte() };
    };

    const super_class: Node.Index, const meta_end_pos: u32 = blk: {
        if (self.current.token.tag == .kw_extends) {
            const heritage = try self.classHeritage();
            break :blk .{ heritage, self.nodes.items(.end)[@backingInt(heritage)] };
        }

        break :blk .{ Node.Index.empty, class_kw.endByte() };
    };

    const class_body = try self.classBody();
    const rb = try self.expect(.@"}");

    const class_meta = try self.addNode(ast.NodeData{
        .class_meta = ast.ClassMeta{
            .name = name,
            .super_class = super_class,
        },
    }, .{ .start = meta_start_pos, .end = meta_end_pos });

    return try self.addNode(.{
        .class_expression = .{
            .meta = class_meta,
            .body = class_body,
        },
    }, spanBetween(class_kw, rb));
}

fn classHeritage(self: *Self) Error!Node.Index {
    _ = try self.expectToken(.kw_extends);
    const super_class = try self.parseLhsExpression();
    return super_class;
}

fn classBody(self: *Self) Error!ast.SubRange {

    // class bodies are always parsed in strict mode.
    const ctx = self.contextEnterStrictMode(true);
    defer self.contextExitStrictMode(ctx);

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    _ = try self.expectToken(.@"{");

    // TODO: refactor this
    var n_constructors: usize = 0;
    while (self.current.token.tag != .@"}") {
        if (self.current.token.tag == .@";") {
            _ = try self.nextToken(); // eat ';'
            continue;
        }

        var saw_constructor = false;
        const element = try self.classElement(&saw_constructor);
        if (saw_constructor) n_constructors += 1;

        if (n_constructors > 1) {
            return self.errorOnNode(element, ParseError.MultipleConstructors);
        }
        try self.scratch.append(self.allocator, element);
    }

    const elements = self.scratch.items[prev_scratch_len..];
    return try self.addSubRange(elements);
}

// `saw_constructor` is an in-out parameter that is set to true if a constructor was seen.
// TODO: remove this saw_constructor_inout parameter
fn classElement(self: *Self, saw_constructor_inout: *bool) Error!Node.Index {
    switch (self.current.token.tag) {
        .kw_constructor => {
            saw_constructor_inout.* = true;
            const ctor = try self.next();
            const ctor_key = try self.identifier(ctor.span());

            if (!self.isAtToken(.@"(")) {
                return self.errorOnToken(self.current.token, ParseError.InvalidClassFieldNamedConstructor);
            }

            return self.parseClassMethodBody(
                ctor_key,
                .{
                    .start_position = ctor.startByte(),
                    .method_flags = .{},
                    .field_flags = .{ .is_static = false, .kind = .constructor },
                },
            );
        },
        else => return self.classPropertyWithModifier(.{ .start_position = self.current.startByte() }),
    }
}

/// Temporary state maintained when parsing
/// a class property of method.
const ClassFieldModifiers = struct {
    start_position: u32,
    field_flags: ast.ClassFieldFlags = .{},
    method_flags: ast.FunctionFlags = .{},
};

/// Parse a class property definition that has 0 or more modifiers like 'async', 'static', etc.
/// This function is called when the parser is at the start of a class element, i.e the first modifier token
/// (or name token, if the field has no modifiers).
fn classPropertyWithModifier(self: *Self, modifiers: ClassFieldModifiers) Error!Node.Index {
    return switch (self.current.token.tag) {
        .kw_async => try self.asyncClassProperty(modifiers),
        .kw_static => try self.staticClassProperty(modifiers),
        .@"*" => try self.generatorClassProperty(modifiers),
        else => try self.classProperty(modifiers),
    };
}

/// Parse a class property with an 'async' modifier.
fn asyncClassProperty(self: *Self, modifiers: ClassFieldModifiers) Error!Node.Index {
    const async_kw = try self.next();
    if ((canStartClassElementName(&self.current.token) or
        self.current.token.tag == .@"*") and
        self.current.token.line == async_kw.token.line)
    {
        // An async method, like `class A { async f() { return 1; } }`
        if (modifiers.method_flags.is_async) {
            try self.emitBadTokenDiagnostic(
                "Duplicate 'async' modifier",
                &async_kw.token,
            );
            return Error.UnexpectedToken;
        }

        var new_modifiers = modifiers;
        new_modifiers.method_flags.is_async = true;
        return self.classPropertyWithModifier(new_modifiers);
    } else {
        // A regular field named 'async', like `class A { async = 5; }`
        // or `class A { async() { return 1; } }`
        const key = try self.identifierReference(async_kw.span());
        return self.completeClassProperty(key, modifiers);
    }
}

/// Parse a class property with a 'static' keyword.
/// This can be a static field, static method, or a field with the name "static".
/// All of these are valid:
/// ```
/// class A {
///    static x = 5;
///    static() { return 1; }
///    static f() { return 1; }
/// }
/// ```
fn staticClassProperty(self: *Self, modifiers: ClassFieldModifiers) Error!Node.Index {
    const static_kw = try self.next();
    if ((canStartClassElementName(&self.current.token) or
        self.current.token.tag == .@"*") and
        self.current.token.line == static_kw.token.line)
    {
        if (modifiers.field_flags.is_static) {
            return self.errorOnToken(static_kw.token, ParseError.DuplicateStaticModifier);
        }

        if (modifiers.method_flags.is_async) {
            try self.emitDiagnosticOnToken(
                static_kw.token,
                "'static' modifier must precede 'async' modifier",
                .{},
            );
            return Error.UnexpectedToken;
        }

        var new_modifiers = modifiers;
        new_modifiers.field_flags.is_static = true;
        return self.classPropertyWithModifier(new_modifiers);
    } else {
        const key = try self.identifierReference(static_kw.span());
        return self.completeClassProperty(key, modifiers);
    }
}

/// Parse a class method starting with a '*' token.
fn generatorClassProperty(self: *Self, modifiers: ClassFieldModifiers) Error!Node.Index {
    const star_token = try self.nextToken();
    assert(star_token.tag == .@"*");

    if (!canStartClassElementName(&self.current.token)) {
        try self.emitBadTokenDiagnostic("a method name after '*'", &self.current.token);
        return Error.UnexpectedToken;
    }

    const key = try self.classElementName();
    // arrow functions cannot be generators.
    // this should've raised an error already instead of getting this
    // far in the parser.
    assert(!modifiers.method_flags.is_arrow);

    var new_modifiers = modifiers;
    new_modifiers.method_flags.is_generator = true;
    return self.parseClassMethodBody(key, new_modifiers);
}

fn classProperty(self: *Self, modifiers: ClassFieldModifiers) Error!Node.Index {
    // Check for 'get' or 'set'
    const kind: ast.ClassFieldKind = blk: {
        const token_str = self.current.token.toByteSlice(self.source);
        if (std.mem.eql(u8, token_str, "get"))
            break :blk .get;
        if (std.mem.eql(u8, token_str, "set"))
            break :blk .set;
        break :blk .init;
    };

    if (kind == .get or kind == .set) {
        assert(!modifiers.method_flags.is_generator);

        const get_or_set = try self.next(); // eat 'get' or 'set'
        if (canStartClassElementName(&self.current.token)) {
            // Getter or Setter.
            const key = try self.classElementName();
            var new_state = modifiers;
            new_state.field_flags.kind = kind;
            return self.parseClassMethodBody(key, new_state);
        }

        // Probably a regular field named 'get' or 'set',
        // like `class A { get() { return 1 }; set = 2;  } `
        const key = try self.identifierReference(get_or_set.span());
        return self.completeClassProperty(key, modifiers);
    }

    const key = try self.classElementName();
    return self.completeClassProperty(key, modifiers);
}

/// Assuming that the key has been parsed, complete the property definition.
fn completeClassProperty(
    self: *Self,
    key: Node.Index,
    modifiers: ClassFieldModifiers,
) Error!Node.Index {
    if (self.current.token.tag == .@"(") {
        self.current_destructure_kind.setNoAssignOrDestruct();
        return self.parseClassMethodBody(key, modifiers);
    }

    if (modifiers.method_flags.is_async) {
        // disallow fields like `async x = 5;`
        return self.errorOnNode(key, ParseError.InvalidAsyncProperty);
    }

    const start_pos = modifiers.start_position;
    if (self.current.token.tag == .@"=") {
        _ = try self.nextToken();
        const value = try self.assignExpressionNoPattern();
        var end_pos = self.nodes.items(.end)[@backingInt(value)];
        end_pos = try self.semiColon(end_pos);
        const kv_node = ast.ClassFieldDefinition{
            .key = key,
            .value = value,
            .flags = modifiers.field_flags,
        };
        return self.addNode(.{ .class_field = kv_node }, .{ .start = start_pos, .end = end_pos });
    }

    var end_pos = self.nodes.items(.end)[@backingInt(key)];
    end_pos = try self.semiColon(end_pos);
    const kv_node = ast.ClassFieldDefinition{
        .key = key,
        .value = key,
        .flags = ast.ClassFieldFlags{
            .kind = .init,
            .is_computed = modifiers.field_flags.is_computed,
            .is_static = modifiers.field_flags.is_static,
        },
    };

    return self.addNode(.{ .class_field = kv_node }, .{ .start = start_pos, .end = end_pos });
}

/// Parse the method body of a class, assuming we're at the '(' token.
/// Returns a `class_method` Node.
fn parseClassMethodBody(
    self: *Self,
    key: Node.Index,
    modifiers: ClassFieldModifiers,
) Error!Node.Index {
    assert(self.current.token.tag == .@"(");

    const func_expr = try self.parseFunctionBody(
        modifiers.start_position,
        null,
        modifiers.method_flags,
        false,
    );

    const end_pos = self.nodes.items(.end)[@backingInt(func_expr)];
    const kv_node = ast.ClassFieldDefinition{
        .key = key,
        .value = func_expr,
        .flags = modifiers.field_flags,
    };

    if (modifiers.field_flags.kind == .get or modifiers.field_flags.kind == .set) {
        if (modifiers.method_flags.is_async) {
            return self.errorOnNode(key, ParseError.AsyncGetterOrSetter);
        }

        // verify the number of parameters for getters and setters.
        // (the if expression looks stupid but is necessary because those are different enum types with same member names)
        try self.checkGetterOrSetterParams(
            func_expr,
            if (modifiers.field_flags.kind == .get) .get else .set,
        );
    }

    return self.addNode(.{ .class_method = kv_node }, .{ .start = modifiers.start_position, .end = end_pos });
}

fn ifStatement(self: *Self) Error!Node.Index {
    const if_kw = try self.next();
    assert(if_kw.token.tag == .kw_if);

    _ = try self.expectToken(.@"(");
    const cond = try self.expression();
    _ = try self.expectToken(.@")");

    const consequent = try self.statement();
    var end_pos = self.nodeSpan(consequent).end;

    var alternate = Node.Index.empty;
    if (self.peek().tag == .kw_else) {
        _ = try self.nextToken();
        alternate = try self.statement();
        end_pos = self.nodeSpan(alternate).end;
    }

    return self.addNode(.{
        .if_statement = .{
            .condition = cond,
            .consequent = consequent,
            .alternate = alternate,
        },
    }, .{ .start = if_kw.startByte(), .end = end_pos });
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
    assert(for_kw.token.tag == .kw_for);

    const loop_kind, const iterator = try self.forLoopIterator();

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.statement();
    const start_pos = for_kw.startByte();
    const end_pos = self.nodes.items(.end)[@backingInt(body)];

    const for_payload = ast.ForStatement{
        .body = body,
        .iterator = iterator,
    };

    const node_data: ast.NodeData = switch (loop_kind) {
        .basic => .{ .for_statement = for_payload },
        .for_of => .{ .for_of_statement = for_payload },
        .for_in => .{ .for_in_statement = for_payload },
    };

    return self.addNode(node_data, .{ .start = start_pos, .end = end_pos });
}

/// Once a 'for' keyword has been consumed, this parses the part inside '()', including the parentheses.
/// Returns a tuple where the first item is the kind of for loop (for-in, for-of, or basic),
/// and the second item is the iterator (index of an `ast.ExtraData`).
fn forLoopIterator(self: *Self) Error!struct { ForLoopKind, ast.Node.Index } {
    const lpar = try self.expect(.@"(");

    var lhs_starts_with_let = false;

    // Check for productions that start with a variable declarator.
    // for (var/let/const x = 0; x < 10; x++)
    // for (var/let/const x in y)
    // for (var/let/const x of y)
    const maybe_vardecl_keyword = blk: {
        const curr = self.current.token.tag;
        if (curr == .kw_var or curr == .kw_const) {
            break :blk try self.next();
        }

        if (curr == .kw_let) {
            lhs_starts_with_let = true;
            break :blk try self.startLetBinding();
        }

        break :blk null;
    };

    if (maybe_vardecl_keyword) |decl_kw| {
        var loop_kind = ForLoopKind.basic;
        const iterator = try self.completeVarDeclLoopIterator(
            lpar.startByte(),
            decl_kw,
            &loop_kind,
        );
        return .{ loop_kind, iterator };
    }

    // 'for (;' indicates a basic 3-part for loop with empty initializer.
    if (self.isAtToken(.@";")) {
        const iterator = try self.completeBasicLoopIterator(
            Node.Index.empty,
            lpar.startByte(),
        );
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

    switch (self.current.token.tag) {
        .@";" => {
            // parse <Expression> ; <Expression> ; <Expression>
            if (self.current_destructure_kind.must_destruct) {
                return self.errorOnNode(expr, ParseError.UnexpectedPattern);
            }

            const iterator = try self.completeBasicLoopIterator(expr, lpar.startByte());
            return .{ ForLoopKind.basic, iterator };
        },

        .kw_of, .kw_in => {
            // parse <Expression> <in/of> <Expression>
            if (!self.current_destructure_kind.can_be_assigned_to) {
                return self.errorOnNode(expr, ParseError.InvalidLoopLhs);
            }

            if (self.isAtToken(.kw_of) and lhs_starts_with_let) {
                try self.emitDiagnosticOnToken(
                    self.current.token,
                    "Left-hand side of for-of loop cannot start with 'let'",
                    .{},
                );
                return Error.UnexpectedToken;
            }

            self.reinterpretAsPattern(expr);

            const loop_kind = if (self.isAtToken(.kw_of))
                ForLoopKind.for_of
            else
                ForLoopKind.for_in;

            _ = try self.nextToken();
            const rhs = try self.expression();
            const rpar = try self.expect(.@")");

            const iterator = try self.addNode(.{
                .for_in_of_iterator = .{
                    .left = expr,
                    .right = rhs,
                },
            }, spanBetween(lpar, rpar));
            return .{ loop_kind, iterator };
        },

        else => {
            try self.emitBadTokenDiagnostic("'of', 'in' or ';'", &self.current.token);
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
fn completeVarDeclLoopIterator(
    self: *Self,
    // The '(' that open the for loop
    lpar_id: u32,
    // The 'var'/'let'/'const' keyword
    decl_kw: TokenWithId,
    loop_kind: *ForLoopKind,
) Error!ast.Node.Index {
    assert(decl_kw.token.tag == .kw_let or
        decl_kw.token.tag == .kw_var or
        decl_kw.token.tag == .kw_const);

    const lhs = try self.bindingIdentifierOrPattern();

    const lhs_span = self.nodeSpan(lhs);
    switch (self.current.token.tag) {
        .kw_in, .kw_of => {
            loop_kind.* = if (self.current.token.tag == .kw_in)
                ForLoopKind.for_in
            else
                ForLoopKind.for_of;

            const declarator = try self.addNode(.{ .variable_declarator = .{ .lhs = lhs, .init = null } }, .{ .start = lhs_span.start, .end = lhs_span.end });

            const decl_range = try self.addSubRange(&[_]Node.Index{declarator});
            const declaration = try self.addNode(.{
                .variable_declaration = .{
                    .kind = varDeclKind(decl_kw.token.tag),
                    .declarators = decl_range,
                },
            }, .{ .start = decl_kw.startByte(), .end = lhs_span.end });

            _ = try self.nextToken(); // eat 'in' or 'of'
            const rhs = try self.expression();
            const rpar = try self.expect(.@")");
            return self.addNode(.{
                .for_in_of_iterator = .{
                    .left = declaration,
                    .right = rhs,
                },
            }, .{ .start = lpar_id, .end = rpar.endByte() });
        },
        else => {
            // TODO: can some of this code be shared with `variableDeclarator`?
            var end_pos = lhs_span.end;
            var rhs: ?Node.Index = null;
            if (self.isAtToken(.@"=")) {
                _ = try self.nextToken();
                const init_expr = try self.assignmentExpression();

                if (self.current_destructure_kind.must_destruct) {
                    try self.emitBadDestructureDiagnostic(init_expr);
                    return Error.UnexpectedPattern;
                }

                end_pos = self.nodeSpan(init_expr).end;
                rhs = init_expr;
            } else if (self.nodeTag(lhs) != .binding_identifier) {
                return self.errorOnNode(lhs, ParseError.MissingInitializerInBinding);
            } else if (decl_kw.token.tag == .kw_const) {
                return self.errorOnNode(lhs, ParseError.MissingInitializerInConst);
            }

            const first_decl = try self.addNode(.{ .variable_declarator = .{ .lhs = lhs, .init = rhs } }, .{ .start = self.nodes.items(.start)[@backingInt(lhs)], .end = end_pos });

            const for_init = try self.completeLoopInitializer(&decl_kw, first_decl);
            return self.completeBasicLoopIterator(for_init, lpar_id);
        },
    }
}

/// A "Basic" loop iterator is a plain old (init; cond; update).
/// Given the `init` expression or statement, this function parses the rest of the iterator
/// upto the closing ')'.
fn completeBasicLoopIterator(
    self: *Self,
    // The first expression after the '('
    for_init: Node.Index,
    // The '(' that opens the for loop iterator
    lpar_id: u32,
) Error!ast.Node.Index {
    _ = try self.expectToken(.@";");

    const for_cond = switch (self.current.token.tag) {
        .@";" => Node.Index.empty,
        else => try self.expression(),
    };

    _ = try self.expectToken(.@";");

    const for_update = switch (self.current.token.tag) {
        .@")" => Node.Index.empty,
        else => try self.expression(),
    };

    const rpar = try self.expect(.@")");

    return self.addNode(.{
        .for_iterator = ast.ForIterator{
            .init = for_init,
            .condition = for_cond,
            .update = for_update,
        },
    }, .{ .start = lpar_id, .end = rpar.endByte() });
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
fn completeLoopInitializer(self: *Self, kw: *const TokenWithId, first_decl: Node.Index) Error!Node.Index {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    try self.scratch.append(self.allocator, first_decl);
    const var_kind = varDeclKind(kw.token.tag);

    while (self.current.token.tag == .@",") {
        _ = try self.nextToken();
        const decl = try self.variableDeclarator(var_kind);
        try self.scratch.append(self.allocator, decl);
    }

    const decl_nodes = self.scratch.items[prev_scratch_len..];
    const decls = try self.addSubRange(decl_nodes);

    const last_decl = decl_nodes[decl_nodes.len - 1];
    const end_pos = self.nodes.items(.end)[@backingInt(last_decl)];

    return self.addNode(.{
        .variable_declaration = .{
            .kind = var_kind,
            .declarators = decls,
        },
    }, .{ .start = kw.startByte(), .end = end_pos });
}

fn whileStatement(self: *Self) Error!Node.Index {
    const while_kw = try self.next();
    assert(while_kw.token.tag == .kw_while);

    _ = try self.expectToken(.@"(");
    const cond = try self.expression();
    _ = try self.expectToken(.@")");

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.statement();
    const end_pos = self.nodeSpan(body).end;

    return self.addNode(.{ .while_statement = .{ .condition = cond, .body = body } }, .{ .start = while_kw.startByte(), .end = end_pos });
}

fn doWhileStatement(self: *Self) Error!Node.Index {
    const do_kw = try self.next();
    assert(do_kw.token.tag == .kw_do);

    const saved_context = self.context;
    defer self.context = saved_context;

    self.context.@"break" = true;
    self.context.@"continue" = true;

    const body = try self.statementOrDeclaration();
    _ = try self.expectToken(.kw_while);
    _ = try self.expectToken(.@"(");
    const cond = try self.expression();
    const rb = try self.expect(.@")");

    var end_pos = rb.endByte();

    if (self.isAtToken(.@";"))
        end_pos = (try self.next()).endByte();

    return self.addNode(.{ .do_while_statement = .{ .condition = cond, .body = body } }, .{ .start = do_kw.startByte(), .end = end_pos });
}

/// DebuggerStatement: 'debugger' ';'
fn debuggerStatement(self: *Self) Error!Node.Index {
    const token = try self.next();
    const end_pos = try self.semiColon(token.endByte());
    return self.addNode(.{ .debugger_statement = {} }, .{ .start = token.startByte(), .end = end_pos });
}

/// Parse a VariableStatement, where `kw` is the keyword used to declare the variable (let, var, or const).
fn variableStatement(self: *Self, kw: TokenWithId) Error!Node.Index {
    const kw_token = kw.token;
    assert(kw_token.tag == .kw_let or
        kw_token.tag == .kw_var or
        kw_token.tag == .kw_const);

    const kind = varDeclKind(kw_token.tag);
    const decls = try self.variableDeclaratorList(kind);
    const decl_slice = self.subRangeToSlice(decls);
    const last_decl = decl_slice[decl_slice.len - 1];

    var end_pos: u32 = self.nodes.items(.end)[@backingInt(last_decl)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(.{
        .variable_declaration = .{
            .kind = kind,
            .declarators = decls,
        },
    }, .{ .start = kw.startByte(), .end = end_pos });
}

/// Parse a list of variable declarators after the 'var', 'let', or 'const'
/// keyword has been eaten.
fn variableDeclaratorList(self: *Self, kind: ast.VarDeclKind) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (true) {
        const decl = try self.variableDeclarator(kind);
        try self.scratch.append(self.allocator, decl);
        if (self.current.token.tag == .@",") {
            _ = try self.nextToken();
        } else {
            break;
        }
    }

    const declarators = self.scratch.items[prev_scratch_len..];
    const decls = try self.addSubRange(declarators);
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
    const start_pos = try_kw.startByte();
    assert(try_kw.token.tag == .kw_try);

    const body = try self.blockStatement();

    var end_pos = self.nodes.items(.end)[@backingInt(body)];
    var catch_clause = Node.Index.empty;
    var finalizer = Node.Index.empty;

    switch (self.current.token.tag) {
        .kw_catch => {
            catch_clause = try self.catchClause();
            if (self.isAtToken(.kw_finally)) {
                finalizer = try self.finallyBlock();
                end_pos = self.nodes.items(.end)[@backingInt(finalizer)];
            } else {
                end_pos = self.nodes.items(.end)[@backingInt(catch_clause)];
            }
        },

        .kw_finally => {
            finalizer = try self.finallyBlock();
            end_pos = self.nodes.items(.end)[@backingInt(finalizer)];
        },

        else => {
            try self.emitBadTokenDiagnostic("'catch' or 'finally'", &self.current.token);
            return Error.UnexpectedToken;
        },
    }

    return self.addNode(.{
        .try_statement = ast.TryStatement{
            .finalizer = finalizer,
            .catch_clause = catch_clause,
            .body = body,
        },
    }, .{ .start = start_pos, .end = end_pos });
}

fn catchClause(self: *Self) Error!Node.Index {
    const catch_kw = try self.expect(.kw_catch);
    const param = try self.catchParameter();

    const body = try self.statementList();
    return self.addNode(.{
        .catch_clause = ast.CatchClause{
            .param = param,
            .body = body,
        },
    }, .{ .start = catch_kw.startByte(), .end = self.nodeSpan(body).end });
}

/// https://262.ecma-international.org/15.0/index.html#prod-CatchParameter
///
/// CatchParameter:
///     BindingIdentifier
///     BindingPattern
fn catchParameter(self: *Self) Error!?Node.Index {
    if (!self.isAtToken(.@"(")) return null;
    _ = try self.nextToken();

    const param = try self.bindingIdentifierOrPattern();
    _ = try self.expect(.@")");
    return param;
}

fn finallyBlock(self: *Self) Error!Node.Index {
    const finally_kw = try self.nextToken();
    assert(finally_kw.tag == .kw_finally);
    const body = try self.blockStatement();
    return body;
}

fn switchStatement(self: *Self) Error!Node.Index {
    const switch_kw = try self.next();
    assert(switch_kw.token.tag == .kw_switch);

    _ = try self.expectToken(.@"(");
    const discriminant = try self.expression();
    _ = try self.expectToken(.@")");

    _ = try self.expectToken(.@"{");

    // allow break statements inside switch-case.
    const ctx = self.context;
    defer self.context = ctx;
    self.context.@"break" = true;

    const cases = try self.caseBlock();
    const rbrace = try self.expect(.@"}");
    const end_pos = rbrace.endByte();

    return self.addNode(.{ .switch_statement = .{ .discriminant = discriminant, .cases = cases } }, .{ .start = switch_kw.startByte(), .end = end_pos });
}

// Parse the all cases inside the body of a switch statement.
// Does not consume the opening '{' and closing '}',
// assumes the opening '{' has already been consumed.
fn caseBlock(self: *Self) Error!ast.SubRange {
    // restore the scratch space after parsing the case-block.
    const scratch_prev_len = self.scratch.items.len;
    defer self.scratch.items.len = scratch_prev_len;

    var saw_default = false;
    while (self.current.token.tag != .@"}") {
        const case_node = blk: {
            switch (self.current.token.tag) {
                .kw_case => break :blk try self.caseClause(),
                .kw_default => {
                    if (saw_default) {
                        // TODO: emit this error and keep going. No need to stop parsing.
                        return self.errorOnToken(self.current.token, ParseError.MultipleDefaults);
                    }

                    saw_default = true;
                    break :blk try self.defaultCase();
                },
                else => {
                    try self.emitBadTokenDiagnostic("'case' or 'default'", &self.current.token);
                    return Error.UnexpectedToken;
                },
            }
        };

        try self.scratch.append(self.allocator, case_node);
    }

    const cases = try self.addSubRange(self.scratch.items[scratch_prev_len..]);
    return cases;
}

/// Parse a single case clause inside a switch statement.
/// A case clause begins with the 'case' (or 'default')  keyword:
/// 'case' Expression ':' StatementList
fn caseClause(self: *Self) Error!Node.Index {
    const case_kw = try self.next();
    assert(case_kw.token.tag == .kw_case);

    const test_expr = try self.expression();

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    _ = try self.expectToken(.@":");
    while (self.current.token.tag != .@"}" and
        self.current.token.tag != .kw_case and
        self.current.token.tag != .kw_default)
    {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(self.allocator, stmt);
    }

    const consequent_stmts = self.scratch.items[prev_scratch_len..];

    const end_pos =
        if (consequent_stmts.len > 0)
            self.nodes.items(.end)[@backingInt(consequent_stmts[consequent_stmts.len - 1])]
        else
            case_kw.endByte();

    const case_node = try self.addNode(.{
        .switch_case = ast.SwitchCase{
            .expression = test_expr,
            .consequent = try self.addSubRange(consequent_stmts),
        },
    }, .{ .start = case_kw.startByte(), .end = end_pos });

    return case_node;
}

/// Parse the 'default' case inside a switch statement.
fn defaultCase(self: *Self) Error!Node.Index {
    const default_kw = try self.next();
    assert(default_kw.token.tag == .kw_default);

    _ = try self.expectToken(.@":");

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    var cur = self.current.token.tag;
    while (cur != .@"}" and cur != .kw_case and cur != .kw_default) : (cur = self.current.token.tag) {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(self.allocator, stmt);
    }

    const consequent_stmts = self.scratch.items[prev_scratch_len..];
    const end_pos =
        if (consequent_stmts.len > 0)
            self.nodes.items(.end)[@backingInt(consequent_stmts[consequent_stmts.len - 1])]
        else
            default_kw.endByte();

    return self.addNode(.{
        .default_case = ast.SwitchDefaultCase{
            .consequent = try self.addSubRange(consequent_stmts),
        },
    }, .{ .start = default_kw.startByte(), .end = end_pos });
}

fn withStatement(self: *Self) Error!Node.Index {
    const with_kw = try self.next();
    assert(with_kw.token.tag == .kw_with);

    _ = try self.expectToken(.@"(");
    const obj = try self.expression();
    _ = try self.expectToken(.@")");

    const body = try self.statement();
    const end_pos = self.nodeSpan(body).end;

    const stmt = try self.addNode(.{ .with_statement = .{ .object = obj, .body = body } }, .{ .start = with_kw.startByte(), .end = end_pos });

    if (self.context.strict) {
        // TODO: emit this error but continue parsing.
        return self.errorOnNode(stmt, ParseError.WithInStrictMode);
    }

    return stmt;
}

fn throwStatement(self: *Self) Error!Node.Index {
    const throw_kw = try self.next();
    assert(throw_kw.token.tag == .kw_throw);

    if (self.current.token.line != throw_kw.token.line) {
        try self.emitDiagnostic(
            throw_kw.token.startCoord(self.source),
            "Newline is not allowed after 'throw'",
            .{},
        );
        return Error.IllegalNewline;
    }

    const expr = try self.expression();
    var end_pos = self.nodes.items(.end)[@backingInt(expr)];
    end_pos = try self.semiColon(end_pos);

    return self.addNode(.{ .throw_statement = expr }, .{ .start = throw_kw.startByte(), .end = end_pos });
}

/// Parse a statement that starts with the `let` keyword.
/// This cannot be a variable declaration, let is either an identifier
/// or a label here.
fn letStatement(self: *Self) Error!Node.Index {
    assert(self.current.token.tag == .kw_let);
    const let_kw = try self.startLetBinding() orelse {
        // If the `let` keyword doesn't start a declaration,
        // then its an identifier.
        if (self.peekAfterTriviaIs(':')) {
            // This is a labeled statement: 'let: ...'
            return self.labeledStatement();
        }

        return self.expressionStatement();
    };

    try self.emitDiagnosticOnToken(
        let_kw.token,
        "Variable declaration is not permitted here",
        .{},
    );
    return Error.UnexpectedToken;
}

/// Checks if the parser is at the beginning of let binding,
/// consumes the `let` token if so, and then returns it.
/// Otherwise, it returns `null` and consumes nothing.
///
/// Must be called when `self.current.token` is a 'let' keyword.
fn startLetBinding(self: *Self) Error!?TokenWithId {
    assert(self.current.token.tag == .kw_let);

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
fn variableDeclarator(self: *Self, kind: ast.VarDeclKind) Error!Node.Index {
    const lhs = try self.bindingIdentifierOrPattern();

    const lhs_span = self.nodeSpan(lhs);
    const start_pos = lhs_span.start;
    var end_pos = lhs_span.end;

    const rhs: ?Node.Index = blk: {
        if (self.isAtToken(.@"=")) {
            _ = try self.nextToken();
            const init_expr = try self.assignExpressionNoPattern();
            end_pos = self.nodeSpan(init_expr).end;
            break :blk init_expr;
        } else if (self.nodeTag(lhs) != .binding_identifier) {
            // Assignment patterns must have an initializer.
            return self.errorOnNode(lhs, ParseError.MissingInitializerInBinding);
        } else if (kind == .@"const") {
            // Constants must have an initializer.
            return self.errorOnNode(lhs, ParseError.MissingInitializerInConst);
        }

        break :blk null;
    };

    return self.addNode(.{ .variable_declarator = .{ .lhs = lhs, .init = rhs } }, .{ .start = start_pos, .end = end_pos });
}

/// EmptyStatement: ';'
fn emptyStatement(self: *Self) Error!Node.Index {
    const semicolon = try self.next();
    assert(semicolon.token.tag == .@";");

    return self.addNode(.{ .empty_statement = {} }, spanBetween(semicolon, semicolon));
}

/// ExpressionStatement: Expression ';'
inline fn expressionStatement(self: *Self) Error!Node.Index {
    const expr = try self.expression();
    const expr_start = self.nodes.items(.start)[@backingInt(expr)];
    const expr_end = self.nodes.items(.end)[@backingInt(expr)];

    const end_pos = try self.semiColon(expr_end);

    return addNode(self, .{ .expression_statement = expr }, .{ .start = expr_start, .end = end_pos });
}

/// BlockStatement:
///   '{' StatementList? '}'
fn blockStatement(self: *Self) Error!Node.Index {
    const stats = try self.parseStatements();
    const block_node = ast.NodeData{ .block_statement = stats.statements };
    return self.addNode(block_node, .{ .start = stats.start, .end = stats.end });
}

/// Parse a
///   '{' StatementList? '}'
fn statementList(self: *Self) Error!Node.Index {
    const stats = try self.parseStatements();
    const block_node = ast.NodeData{ .statement_list = stats.statements };
    return self.addNode(block_node, .{ .start = stats.start, .end = stats.end });
}

const Statements = struct {
    statements: ast.SubRange,
    start: u32,
    end: u32,
};

/// Parse a list of statements inside  '{}'
fn parseStatements(self: *Self) Error!Statements {
    const lbrac = try self.expect(.@"{");
    const start_pos = lbrac.startByte();

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (self.current.token.tag != .@"}") {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(self.allocator, stmt);
    }

    const rbrace = try self.next();
    assert(rbrace.token.tag == .@"}");
    const end_pos = rbrace.endByte();

    const statements = self.scratch.items[prev_scratch_len..];
    const statement_nodes = try self.addSubRange(statements);
    return .{ .statements = statement_nodes, .start = start_pos, .end = end_pos };
}

/// Assuming the parser is at the `function` keyword,
/// parse a function declaration statement.
fn functionDeclaration(
    self: *Self,
    start_byte: u32,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.nextToken();
        fn_flags.is_generator = true;
    }

    const func_name = try self.functionName();
    return self.parseFunctionBody(start_byte, func_name, fn_flags, true);
}

fn functionName(self: *Self) Error!Node.Index {
    const context = self.context;
    defer self.context = context;

    const token = try self.next();
    if (self.isIdentifier(token.token.tag))
        return self.bindingIdentifier(token.span());

    try self.emitBadTokenDiagnostic("function name", &token.token);
    return Error.UnexpectedToken;
}

/// ReturnStatement:
///    'return' ';'
///    'return' [no LineTerminator here] Expression? ';'
fn returnStatement(self: *Self) Error!Node.Index {
    const return_kw = try self.next();
    assert(return_kw.token.tag == .kw_return);

    if (!self.context.@"return") {
        // todo: maybe we should just emit a diagnostic here and continue parsing?
        // that way we can catch more parse errors than just this.
        return self.errorOnToken(return_kw.token, ParseError.IllegalReturn);
    }

    if (self.current.token.line != return_kw.token.line or
        self.current.token.tag == .@";" or
        self.current.token.tag == .@"}")
    {
        const end_pos = try self.semiColon(return_kw.endByte());
        return self.addNode(.{ .return_statement = null }, .{ .start = return_kw.startByte(), .end = end_pos });
    }

    const operand = try self.expression();
    const operand_end = self.nodes.items(.end)[@backingInt(operand)];
    const end_pos = try self.semiColon(operand_end);
    return self.addNode(.{ .return_statement = operand }, .{ .start = return_kw.startByte(), .end = end_pos });
}

/// BreakStatement: 'break' ';'
fn breakStatement(self: *Self) Error!Node.Index {
    const break_kw = try self.next();
    assert(break_kw.token.tag == .kw_break);

    if (!self.context.@"break") {
        try self.emitDiagnostic(
            break_kw.token.startCoord(self.source),
            "'break' is not allowed outside loops and switch statements",
            .{},
        );
        return Error.IllegalBreak;
    }

    const start_pos = break_kw.startByte();
    var end_pos = break_kw.endByte();

    const label = blk: {
        const cur = self.current.token;
        if ((cur.tag.isIdentifier() or self.isKeywordIdentifier(cur.tag)) and
            cur.line == break_kw.token.line)
        {
            // TODO: check if this label exists.
            const token = try self.next();
            const id = try self.identifier(token.span());
            end_pos = token.endByte();
            break :blk id;
        }

        break :blk null;
    };

    end_pos = try self.semiColon(end_pos);
    return self.addNode(.{ .break_statement = .{ .label = label } }, .{ .start = start_pos, .end = end_pos });
}

/// ContinueStatement: 'continue' ';'
fn continueStatement(self: *Self) Error!Node.Index {
    const continue_kw = try self.next();
    assert(continue_kw.token.tag == .kw_continue);

    if (!self.context.@"continue") {
        try self.emitDiagnostic(
            continue_kw.token.startCoord(self.source),
            "'continue' is not allowed outside loops",
            .{},
        );
        return Error.IllegalContinue;
    }

    const start_pos = continue_kw.startByte();
    var end_pos = continue_kw.endByte();

    const label = blk: {
        const cur = self.current.token;
        if ((cur.tag.isIdentifier() or self.isKeywordIdentifier(cur.tag)) and
            cur.line == continue_kw.token.line)
        {
            // TODO: check if this label exists.
            const token = try self.next();
            const id = try self.identifier(token.span());
            end_pos = token.endByte();
            break :blk id;
        }

        break :blk null;
    };

    end_pos = try self.semiColon(end_pos);
    return self.addNode(.{ .continue_statement = .{ .label = label } }, .{ .start = start_pos, .end = end_pos });
}

// ------------------------
// Common helper functions
// ------------------------

fn errorMessage(err: ParseError) []const u8 {
    return switch (err) {
        ParseError.InvalidAsyncProperty => "The async modifier is only allowed on methods",
        ParseError.DuplicateStaticModifier => "Duplicate 'static' modifier",
        ParseError.InvalidKeywordShorthandProperty => "Keywords cannot be used as shorthand property names",
        ParseError.AsyncGetterOrSetter => "The async modifier cannot be used on getters and setters",
        ParseError.IllegalReturn => "Return statement is not allowed outside of a function",
        ParseError.IllegalBreak => "'break' is not allowed outside loops and switch statements",
        ParseError.IllegalContinue => "'continue' is not allowed outside switch statements",
        ParseError.IllegalAwait => "'await' expressions are not allowed outside async functions",
        ParseError.InvalidSetter => "Setter functions must have exactly one parameter",
        ParseError.InvalidSetterRestParam => "Setter functions cannot have reset parameters",
        ParseError.InvalidGetter => "Getter functions must have no parameters",
        ParseError.InvalidLoopLhs => "left hand side of for-loop must be a name or assignment pattern",
        ParseError.MissingInitializerInConst => "Missing initializer in const declaration",
        ParseError.MissingInitializerInBinding => "Binding patterns must have an initializer",
        ParseError.MultipleDefaults => "Multiple 'default' clauses are not allowed in a switch statement",
        ParseError.UnexpectedToken => "Unexpected token",
        ParseError.WithInStrictMode => "With statements are not allowed in strict mode",
        ParseError.ExpectedSemicolon => "Expected a ';' or a newline",
        ParseError.ExpectedExport => "`export` keyword must be followed by an export declaration",
        ParseError.UnexpectedPattern => "Expected expression, found binding pattern",
        ParseError.RestElementNotLast => "Rest element must be the last element in a destructuring pattern",
        ParseError.IllegalDeleteInStrictMode => "Cannot delete a variable in strict mode",
        ParseError.IllegalStrictModeNumber => "Numbers with a leading '0' are not allowed in strict mode",
        ParseError.IllegalStrictModeOctal => "Legacy octal numbers are not allowed in strict mode",
        ParseError.IllegalFatArrow => "'=>' must appear immediately after arrow function parameters",
        ParseError.IllegalNewline => "Unexpected newline",
        ParseError.InvalidCoalesceExpression => "Logical operators cannot be mixed with the nullish coalescing operator",
        ParseError.InvalidRestOperand => "A '...' here must be followed by a valid assignment target",
        ParseError.InvalidUpdateOperand => "Invalid operand for update expression",
        ParseError.InvalidAssignmentTarget => "Invalid assignment target",
        ParseError.LexicallyBoundLet => "'let' cannot be used as a lexically bound variable name",
        ParseError.MultipleConstructors => "Classes cannot multiple constructors",
        ParseError.InvalidClassFieldNamedConstructor => "Classes cannot have a field with the name 'constructor'",
        ParseError.InvalidObject => "Expression is neither an object literal, nor a destructuring pattern",
        ParseError.InvalidMetaProperty => "Invalid meta property",
        ParseError.TypeScriptNotImplemented => "TypeScript support is not implemented",
        ParseError.InvalidArrowParameters => "Invalid arrow function parameters",
        ParseError.MissingArrow => "Missing '=>' after arrow function parameter list",
        ParseError.MalformedArrowFnBody => "Malformed arrow function body",
        ParseError.LetInStrictMode => "'let' is not allowed in strict mode",
        ParseError.InvalidFunctionParameter => "Function parameter must be a name or binding pattern",
        ParseError.IllegalLabeledStatement => "Labelled statements are not allowed here",
        ParseError.AlreadyParsed => "The parser has already been used",
        ParseError.ExpectedImportSpecifier => "Expected an import specifier, '*', or file path",
        ParseError.InvalidExpressionInsideParens => "Invalid expression or pattern inside (...)",
        ParseError.RestElementNotLastInParams => "Rest element must be the last item in a function parameter list",
        ParseError.JsxExpectedTagName => "Expected a tag name after '<' in JSX code",
        ParseError.JsxExpectedGt => "Expected a '>' to close the JSX opening tag",
        ParseError.LAngleWithoutJsx => "Found a '<' in non-JSX source code",
    };
}

fn emitError(self: *Self, coord: types.Coordinate, err: ParseError) Error {
    const msg = errorMessage(err);
    try self.emitDiagnostic(coord, "{s}", .{msg});
    return err;
}

fn errorOnToken(self: *Self, token: Token, err: ParseError) Error {
    const coord = token.startCoord(self.source);
    return self.emitError(coord, err);
}

fn errorOnNode(self: *Self, node: Node.Index, err: ParseError) Error {
    const start_byte = self.nodes.items(.start)[@backingInt(node)];
    const start_coord = util.offsets.byteIndexToCoordinate(self.source, start_byte);
    return self.emitError(start_coord, err);
}

/// Enter strict mode by modifying the current context.
/// Returns the previous context that must be passed to `contextExitStrictMode`.
/// ```zig
/// const ctx = self.contextExitStrictMode();
/// defer self.contextExitStrictMode(ctx);
/// ```
fn contextEnterStrictMode(self: *Self, strict: bool) ParseContext {
    const ctx = self.context;
    self.context.strict = strict;
    self.tokenizer.is_in_strict_mode = strict;
    return ctx;
}

/// Exit strict mode and restore the previous context
fn contextExitStrictMode(self: *Self, prev_ctx: ParseContext) void {
    self.context = prev_ctx;
    self.tokenizer.is_in_strict_mode = self.context.strict;
}

fn subRangeToSlice(self: *Self, range: ast.SubRange) []const Node.Index {
    return self.node_lists.items[@backingInt(range.from)..@backingInt(range.to)];
}

/// Helper function to finish a statement with a semi-colon.
/// `end_pos` is the end-position of the statement node,
/// if a semi-colon is found, returns the end position of the semi-colon,
/// otherwise returns `end_pos`.
/// If the ASI rules cannot be applied, returns a parse error.
fn semiColon(self: *Self, end_pos: u32) Error!u32 {
    return if (try self.eatSemiAsi()) |semi_end|
        semi_end
    else
        end_pos;
}

/// Eat a semicolon token and return its span.
/// If there if there is no semi-colon token, it will check if we're allowed to assume an implicit ';' exists
/// https://tc39.es/ecma262/multipage/ecmascript-language-lexical-grammar.html#sec-automatic-semicolon-insertion
/// If no semi-colon can be inserted, a parse error is returned instead.
fn eatSemiAsi(self: *Self) Error!?u32 {
    if (self.current.token.tag == .@";") {
        const semi = try self.next();
        return semi.endByte();
    }

    if (self.current.token.line != self.prev_token_line or
        self.current.token.tag == .@"}" or
        self.current.token.tag == .eof)
        return null;

    try self.emitBadTokenDiagnostic("a ';' or a newline", &self.current.token);
    return Error.ExpectedSemicolon;
}

/// Returns whether a token with the tag `tag` can start a variable declarator
/// ({, or [, or an identifier).
fn isDeclaratorStart(self: *Self, tag: Token.Tag) bool {
    // TODO: can use TokenMask bitmasks for the first part and short-circuit in most cases
    return tag == .@"[" or tag == .@"{" or
        tag.isIdentifier() or self.isKeywordIdentifier(tag);
}

/// Returns whether the parser is currently parsing strict mode code.
fn isInStrictMode(self: *const Self) bool {
    return self.context.strict or self.context.module;
}

/// Returns `true` of `tag` is a keyword that can be allowed as an identifier
/// in the current context.
fn isKeywordIdentifier(self: *const Self, tag: Token.Tag) bool {
    switch (tag) {
        .kw_await => return !(self.context.is_await_reserved or self.source_type == .module),
        .kw_yield => return !(self.context.is_yield_reserved or self.context.strict),
        else => {
            if (tag.isContextualKeyword()) return true;
            if (self.isInStrictMode()) return false;
            // check if tag is a keyword, but that keyword can be used as an identifier in non-strict mode.
            return tag.isStrictModeKeyword() or tag == .kw_let;
        },
    }
}

inline fn isIdentifier(self: *const Self, tag: Token.Tag) bool {
    return tag.isIdentifier() or self.isKeywordIdentifier(tag);
}

/// Check if `expr` is an invalid assignment target when wrapped in parentheses.
/// Assumes that `expr` on its own is a valid assignment target *without* '()'.
fn isValidAssignTargetInParens(self: *const Self, expr: Node.Index) bool {
    const node = self.nodeTag(expr);
    return switch (node) {
        .assignment_expr, .object_literal, .array_literal => true,
        else => false,
    };
}

fn addSubRange(self: *Self, nodes: []const Node.Index) error{OutOfMemory}!ast.SubRange {
    const from: ast.SubRange.Index = @fromBackingInt(@intCast(self.node_lists.items.len));
    try self.node_lists.appendSlice(self.allocator, nodes);
    const to: ast.SubRange.Index = @fromBackingInt(@intCast(self.node_lists.items.len));
    return ast.SubRange{ .from = from, .to = to };
}

fn addToken(self: *Self, token: Token) error{OutOfMemory}!Token.Index {
    try self.tokens.append(self.allocator, token);
    return @fromBackingInt(@intCast(self.tokens.items.len - 1));
}

/// Append a token to `tree.tokens`. The list is only populated when
/// the parser is configured with `tokens = true`; nothing reads it otherwise.
inline fn saveToken(self: *Self, token: Token) error{OutOfMemory}!void {
    if (!self.config.tokens) return;
    try self.tokens.append(self.allocator, token);
}

/// Append a node to the flat node list.
fn addNode(self: *Self, node: NodeData, span: ast.Span) error{OutOfMemory}!Node.Index {
    try self.nodes.append(
        self.allocator,
        .{ .data = node, .start = span.start, .end = span.end },
    );
    return @fromBackingInt(@intCast(self.nodes.len - 1));
}

/// Append an ExtraData item to the list, and return its index.
fn addExtraData(self: *Self, data: ast.ExtraData) error{OutOfMemory}!ast.ExtraData.Index {
    try self.extra_data.append(self.allocator, data);
    return @fromBackingInt(@intCast(self.extra_data.items.len - 1));
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
    const start_byte = self.nodes.items(.start)[@backingInt(expr)];
    const start_coord = util.offsets.byteIndexToCoordinate(self.source, start_byte);
    return self.emitDiagnostic(start_coord, "Unexpected destructuring pattern", .{});
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
    try self.diagnostics.append(self.allocator, Diagnostic{
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
    try self.diagnostics.append(self.allocator, Diagnostic{
        .coord = token.startCoord(self.source),
        .message = message,
    });
}

fn expect(self: *Self, tag: Token.Tag) Error!TokenWithId {
    if (self.current.token.tag == tag) {
        return try self.next();
    } else {
        try self.emitDiagnostic(
            self.current.token.startCoord(self.source),
            "Expected a '{s}', but found a '{s}'",
            .{ @tagName(tag), self.current.token.toByteSlice(self.source) },
        );
        return Error.UnexpectedToken;
    }
}

/// Expect the current token to have a specific tag, and continue tokenization
/// as if we're inside a JSX text.
///
/// [tag] MUST be one of .jsx_identifier, .@"<", .@">", .@"/", .@"{", .@"}",
/// or .jsx_text.
fn expectJsx(self: *Self, tag: Token.Tag) Error!TokenWithId {
    if (self.current.token.tag == tag) {
        return try self.nextJsx();
    } else {
        try self.emitDiagnostic(
            self.current.token.startCoord(self.source),
            "Expected a '{s}', but found a '{s}'",
            .{ @tagName(tag), self.current.token.toByteSlice(self.source) },
        );
        return Error.UnexpectedToken;
    }
}

/// Expect a '>' token to close a JSX opening tag.
fn expectJsxRAngleForOpeningTag(self: *Self) Error!TokenWithId {
    if (self.current.token.tag == .@">") {
        return try self.nextJsx();
    }

    switch (self.current.token.tag) {
        .@">>", .@">>>", .@">=", .@">>=", .@">>>=" => {
            self.reScanJsxGt(); // re-lex '>>=' as '>'...
            return try self.nextJsx();
        },

        else => {
            return self.errorOnToken(self.current.token, ParseError.JsxExpectedGt);
        },
    }
}

/// Emit a parse error if the current token does not match `tag`.
fn expectToken(self: *Self, tag: Token.Tag) Error!Token {
    if (self.current.token.tag == tag) {
        return try self.nextToken();
    }

    try self.emitDiagnostic(
        self.current.token.startCoord(self.source),
        "Expected a '{s}', but found a '{s}'",
        .{ @tagName(tag), self.current.token.toByteSlice(self.source) },
    );
    return Error.UnexpectedToken;
}

/// Emit a parse error if the current token is not an identifier.
/// If the current token is an identifier, consume and return it.
fn expectIdentifier(self: *Self) Error!TokenWithId {
    if (self.current.token.tag.isIdentifier()) {
        return try self.next();
    }

    try self.emitDiagnostic(
        self.current.token.startCoord(self.source),
        "Expected an identifier, but found a '{s}'",
        .{self.current.token.toByteSlice(self.source)},
    );
    return Error.UnexpectedToken;
}

fn expectIdOrKeyword(self: *Self) Error!TokenWithId {
    if (self.current.token.tag.isIdentifier() or self.current.token.tag.isKeyword()) {
        return self.next();
    }

    try self.emitBadTokenDiagnostic("identifier", &self.current.token);
    return Error.UnexpectedToken;
}

/// Emit a parse error if the current token does not match `tag1` or `tag2`.
fn expect2(self: *Self, tag1: Token.Tag, tag2: Token.Tag) Error!TokenWithId {
    const token = try self.next();
    if (token.token.tag == tag1 or token.token.tag == tag2) {
        return token;
    }

    try self.emitDiagnostic(
        token.token.startCoord(self.source),
        "Expected a '{s}' or a '{s}', but found a '{s}'",
        .{
            @tagName(tag1),
            @tagName(tag2),
            token.token.toByteSlice(self.source),
        },
    );
    return Error.UnexpectedToken;
}

/// Consume the next token from the lexer, skipping all comments and whitespaces.
/// Returns the CURRENT token which was consumed in the previous call to 'next',
/// and advances the pointer to the next token.
inline fn next(self: *Self) Error!TokenWithId {
    const prev = self.current;
    self.prev_token_line = prev.token.line;

    // The tokenizer scans directly into the parser's token slot;
    // comment tokens are stored and scanned past.
    try self.tokenizer.nextSkippingWhitespaceInto(&self.current.token);
    while (self.current.token.tag == .comment) {
        try self.saveToken(self.current.token);
        try self.tokenizer.nextSkippingWhitespaceInto(&self.current.token);
    }
    try self.saveToken(self.current.token);

    return prev;
}

/// Consume the next JSX token from the lexer, skipping all comments and whitespaces.
fn nextJsx(self: *Self) Error!TokenWithId {
    var next_token = try self.tokenizer.nextJsxChild();
    while (next_token.tag.is(TokenMask.Trivia)) {
        try self.saveToken(next_token);
        next_token = try self.tokenizer.nextJsxChild();
    }

    return self.advanceToToken(next_token);
}

/// Discard the '/' or '/=' token that was just scanned,
/// and re-tokenize it as a regex literal.
///
/// Mutates `self.current` and `self.tokens`
fn reScanRegexLiteral(self: *Self) Error!void {
    const token = &self.current.token;
    assert(token.tag == .@"/" or token.tag == .@"/=");

    const regex_token = try self.tokenizer.reScanRegexLiteral(token);
    assert(regex_token.tag == .regex_literal);

    if (self.config.tokens) {
        self.tokens.items[self.tokens.items.len - 1] = regex_token;
    }
    self.current.token = regex_token;
}

/// Discard the '}' token that was just scanned, and replace it
/// with a re-tokenized `.template_literal_part` token.
///
/// Mutates `self.current` and `self.tokens`
fn reScanTemplatePart(self: *Self) Error!void {
    const cur = &self.current.token;
    assert(cur.tag == .@"}");

    const template_part = try self.tokenizer.reScanTemplatePart(cur);
    assert(template_part.tag == .template_literal_part);

    // replate the '}' token in the buffer with the template part token
    if (self.config.tokens) {
        self.tokens.items[self.tokens.items.len - 1] = template_part;
    }
    self.current.token = template_part;
}

/// Re tokenize the current '>>', '>>>', '>=', '>>=', or '>>>=' token
/// as a single '>' token that closes a JSX opening tag.
fn reScanJsxGt(self: *Self) void {
    const cur = &self.current.token;
    assert(switch (cur.tag) {
        .@">=", .@">>=", .@">>>=", .@">>", .@">>>" => true,
        else => false,
    });

    const jsx_gt = self.tokenizer.reScanJsxGt(cur);
    assert(jsx_gt.tag == .@">");

    // replate the '>>=' etc. token in the buffer with the template part token
    if (self.config.tokens) {
        self.tokens.items[self.tokens.items.len - 1] = jsx_gt;
    }
    self.current.token = jsx_gt;
}

/// Set [next_token] as the current token, and update `self.current`, and `self.prev_token_line`
/// Returns the old value of `self.current`.
fn advanceToToken(self: *Self, next_token: Token) error{OutOfMemory}!TokenWithId {
    try self.saveToken(next_token);

    const prev = self.current;
    self.current = .{ .token = next_token };
    self.prev_token_line = prev.token.line;

    return prev;
}

/// Intialize `self.current` by consuming the first token.
fn startTokenizer(self: *Self) Error!void {
    var token: Token = undefined;
    try self.tokenizer.nextSkippingWhitespaceInto(&token);
    while (token.tag == .comment) {
        // trivia handling mirrors `next`: store comments, skip whitespace.
        try self.saveToken(token);
        try self.tokenizer.nextSkippingWhitespaceInto(&token);
    }

    try self.saveToken(token);
    self.current = .{ .token = token };
    self.prev_token_line = 0;
}

inline fn nextToken(self: *Self) Error!Token {
    return (try self.next()).token;
}

/// Return the next non-whitespace and non-comment token,
/// without consuming it, or advancing in the source.
///
/// NOTE: The token returned by this function should only be used for
/// inspection, and must not be added to the AST.
/// Check whether the first non-trivia byte after the current token is [c],
/// by scanning raw source bytes. Skips whitespace and comments.
/// Cheaper than `lookAhead` for one-byte questions; the tokenizer must be
/// positioned right after the current token (true for freshly scanned tokens).
fn peekAfterTriviaIs(self: *Self, c: u8) bool {
    const src = self.source;
    var i = self.tokenizer.index;
    while (i < src.len) {
        switch (src[i]) {
            ' ', '\t', '\r', '\n' => i += 1,
            '/' => {
                if (i + 1 < src.len and src[i + 1] == '/') {
                    i += 2;
                    while (i < src.len and src[i] != '\n') i += 1;
                } else if (i + 1 < src.len and src[i + 1] == '*') {
                    i += 2;
                    while (i + 1 < src.len and !(src[i] == '*' and src[i + 1] == '/')) i += 1;
                    i = @min(i + 2, src.len);
                } else return src[i] == c;
            },
            else => return src[i] == c,
        }
    }
    return false;
}

fn lookAhead(self: *Self) Error!Token {
    const line = self.tokenizer.line;
    const index = self.tokenizer.index;

    var next_token = try self.tokenizer.nextSkippingWhitespace();
    while (next_token.tag == .comment) : (next_token = try self.tokenizer.nextSkippingWhitespace()) {}

    // restore the tokenizer state.
    self.tokenizer.rewind(index, line);
    return next_token;
}

fn peek(self: *Self) Token {
    return self.current.token;
}

fn isAtToken(self: *Self, tag: Token.Tag) bool {
    return self.current.token.tag == tag;
}

// --------------------------------------------------------------------------------------
// Expression parsing.
// https://262.ecma-international.org/15.0/index.html#sec-ecmascript-language-expressions
// --------------------------------------------------------------------------------------

// Expression : AssignmentExpression
//            | Expression, AssignmentExpression
inline fn expression(self: *Self) Error!Node.Index {
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

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    _ = try self.scratch.append(self.allocator, first_expr);

    var last_expr = first_expr;
    while (self.isAtToken(.@",")) {
        _ = try self.nextToken(); // eat ','
        const rhs = try self.assignExpressionNoPattern();
        last_expr = rhs;
        try self.scratch.append(self.allocator, rhs);
    }

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(first_expr)];
    const end_pos = nodes.items(.end)[@backingInt(last_expr)];

    const sub_exprs = self.scratch.items[prev_scratch_len..];
    const expr_list = try self.addSubRange(sub_exprs);
    return try self.addNode(.{ .sequence_expr = expr_list }, .{ .start = start_pos, .end = end_pos });
}

/// A `BindingPattern` or `BindingIdentifier`.
/// Used on the left-hand-side of a variable declarator.
fn bindingIdentifierOrPattern(self: *Self) Error!Node.Index {
    // start with the assumption that we're parsing a valid, destruct-able
    // binding pattern. If we encounter any invalid nodes, like "{ a: 2 }",
    // we will bail later.
    self.current_destructure_kind = .{
        .must_destruct = false,
        .can_be_assigned_to = true,
        .can_destruct = true,
        .is_simple_expression = true,
    };

    const old_destruct_kind = self.current_destructure_kind;
    defer self.current_destructure_kind = old_destruct_kind;

    const token: *const Token = &self.current.token;
    switch (token.tag) {
        .@"{" => return self.objectBindingPattern(),
        .@"[" => return self.arrayBindingPattern(),
        else => {
            if (self.isIdentifier(token.tag))
                return self.bindingIdentifier((try self.next()).span());

            try self.emitBadTokenDiagnostic("a variable name or binding target", token);
            return Error.InvalidAssignmentTarget;
        },
    }
}

fn reinterpretPattern(
    self: *Self,
    node_pls: []NodeData,
    node_id: Node.Index,
    is_binding_pattern: bool,
) void {
    const node: *ast.NodeData = &node_pls[@backingInt(node_id)];
    switch (node.*) {
        .member_expr,
        .empty_array_item,
        .computed_member_expr,
        .call_expr,
        => return,

        .assignment_pattern => |pattern| {
            self.reinterpretPattern(node_pls, pattern.lhs, is_binding_pattern);
        },

        .rest_element => |rest_pl| {
            self.reinterpretPattern(node_pls, rest_pl, is_binding_pattern);
        },

        .identifier_reference => |id_token| {
            if (is_binding_pattern) {
                node.* = .{ .binding_identifier = id_token };
            }
        },

        .identifier => |id_token| {
            // ({ x:y } = 1) // <- convert y to an identifier REFERENCE
            // ({ x:y }) => 1 // <- convert y to a BINDING identifier
            node.* = if (is_binding_pattern)
                .{ .binding_identifier = id_token }
            else
                .{ .identifier_reference = id_token };
        },
        // TODO: import.meta is not assignable. raise an error here
        .meta_property => return,
        .object_literal => |object_pl| {
            node.* = .{ .object_pattern = object_pl };
            const elems = self.subRangeToSlice(object_pl orelse return);
            for (elems) |elem_id| {
                self.reinterpretPattern(node_pls, elem_id, is_binding_pattern);
            }
        },
        .array_literal => |array_pl| {
            node.* = .{ .array_pattern = array_pl };
            const elems = self.subRangeToSlice(array_pl orelse return);
            for (elems) |elem_id| {
                self.reinterpretPattern(node_pls, elem_id, is_binding_pattern);
            }
        },
        .object_property => |property_definiton| {
            const key_id = property_definiton.key;
            const value_id = property_definiton.value;
            if (key_id == value_id) {
                // foo({x}) // <- `x` is an IDENTIFIER REFERENCE
                // ({x} = 1) // <- `x` is a BINDING IDENTIFIER
                const key = &node_pls[@backingInt(property_definiton.key)];
                switch (key.*) {
                    .identifier,
                    .identifier_reference,
                    => |key_token| {
                        key.* = if (is_binding_pattern)
                            ast.NodeData{ .binding_identifier = key_token }
                        else
                            ast.NodeData{ .identifier_reference = key_token };
                    },
                    // when the key and value are the same,the key must be an identifier
                    else => unreachable,
                }
            } else {
                self.reinterpretPattern(node_pls, value_id, is_binding_pattern);
            }
        },
        .assignment_expr => |assign_pl| {
            // ({x:y}=1) // <- LHS goes from object expression to object pattern
            self.reinterpretPattern(node_pls, assign_pl.lhs, is_binding_pattern);
            node.* = .{ .assignment_pattern = assign_pl };
        },
        .spread_element => |spread_pl| {
            self.reinterpretPattern(node_pls, spread_pl, is_binding_pattern);
            node.* = .{ .rest_element = spread_pl };
        },
        .shorthand_property => |o| {
            if (is_binding_pattern) {
                const name_token_pl = node_pls[@backingInt(o.name)];
                // TODO(@injuly): for non-binding shorthand properties, the name should be smth like a 'shorthand_id_ref' (RW)
                assert(meta.activeTag(name_token_pl) == .identifier_reference);
                node_pls[@backingInt(o.name)] = .{ .binding_identifier = name_token_pl.identifier_reference };
            }
        },
        else => {},
    }
}

/// Mutate existing nodes to convert a PrimaryExpression to an
/// AssignmentPattern (or Identifier) (e.g, .object_literal => .object_pattern)
/// This will also convert some `identifier` nodes to `binding` identifier nodes.
/// E.g: `{ x }` <- `x` becomes a `binding_identifier` node.
fn reinterpretAsBindingPattern(self: *Self, node_id: Node.Index) void {
    const node_pls = self.nodes.items(.data);
    self.reinterpretPattern(node_pls, node_id, true);
}

/// Mutate existing nodes to convert a PrimaryExpression to an
/// AssignmentPattern (or Identifier) (e.g, .object_literal => .object_pattern)
fn reinterpretAsPattern(self: *Self, node_id: Node.Index) void {
    const node_pls = self.nodes.items(.data);
    self.reinterpretPattern(node_pls, node_id, false);
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
    // `yield` expressions can only start an assignment expression.
    if (self.current.token.tag == .kw_yield and self.context.is_yield_reserved) {
        return self.yieldExpression();
    }

    // Start with the assumption that we're parsing a valid destructurable expression.
    // Subsequent parsing functions will update this to refect the actual destructure-kind
    // of `lhs`.
    self.current_destructure_kind = .{
        .can_destruct = true,
        .can_be_assigned_to = true,
        .must_destruct = false,
    };

    var left = try self.parsePostfixExpression();

    while (true) {
        const tag = self.current.token.tag;
        if (!tag.is(TokenMask.IsInfix)) break;

        // arrow function: '(a, b) => ...' or 'x => ...'
        if (tag == .@"=>") {
            if (self.nodeTag(left) == .identifier_reference) {
                // 'x => 1': reinterpret the identifier reference as a binding
                // in place, the same way the '(x) => 1' path does, so both
                // produce the same node sequence.
                if (self.current.token.line != self.prev_token_line) {
                    return self.errorOnToken(self.current.token, ParseError.IllegalFatArrow);
                }
                const span = self.nodeData(left).identifier_reference;
                const node_pls = self.nodes.items(.data);
                node_pls[@backingInt(left)] = .{ .binding_identifier = span };
                const param_list = try self.addSubRange(&[_]Node.Index{left});
                left = try self.addNode(.{ .parameters = param_list }, span);
            }
            return self.completeArrowFunction(left);
        }

        if (tag.isAssignmentOperator()) {
            if (!self.current_destructure_kind.can_be_assigned_to) {
                try self.emitDiagnostic(
                    self.current.token.startCoord(self.source),
                    "Invalid assignment target",
                    .{},
                );
                return Error.InvalidAssignmentTarget;
            }

            self.reinterpretAsPattern(left);

            const op_token = try self.next(); // eat assignment operator
            if (op_token.token.tag != .@"=" and !self.current_destructure_kind.is_simple_expression) {
                try self.emitDiagnosticOnToken(
                    op_token.token,
                    "Invalid target for '{s}'. Did you mean to use '=' instead?",
                    .{op_token.token.toByteSlice(self.source)},
                );
                return Error.InvalidAssignmentTarget;
            }

            const rhs = try self.assignExpressionNoPattern();
            const start = self.nodes.items(.start)[@backingInt(left)];
            const end = self.nodes.items(.end)[@backingInt(rhs)];

            // An assignment expression is a valid destructuring target.
            // `[x = 1] = [2]` is valid.
            // assignment patterns inside object literals are handled separately
            // in `Parser.identifierProperty`
            self.current_destructure_kind = .{
                .can_destruct = op_token.token.tag == .@"=",
                .can_be_assigned_to = op_token.token.tag == .@"=",
                .must_destruct = false,
            };

            return self.addNode(.{
                .assignment_expr = .{
                    .lhs = left,
                    .rhs = rhs,
                    .operator = ast.Operator.fromTag(op_token.token.tag),
                },
            }, .{ .start = start, .end = end });
        }

        // right-associative exponentiation: 'a ** b ** c' is 'a ** (b ** c)'
        if (tag == .@"**") {
            const op_token = try self.next();
            const rhs = try self.parseExponentExpression();

            const nodes = self.nodes.slice();
            const start_pos = nodes.items(.start)[@backingInt(left)];
            const end_pos = nodes.items(.end)[@backingInt(rhs)];

            left = try self.addNode(.{
                .binary_expr = ast.BinaryPayload{
                    .lhs = left,
                    .rhs = rhs,
                    .operator = ast.Operator.fromTag(op_token.token.tag),
                },
            }, .{ .start = start_pos, .end = end_pos });
            continue;
        }

        // binary and logical operators; consumes the whole chain
        if (tag.is(TokenMask.IsBinaryOp)) {
            // `in` is not allowed when parsing the LHS of for-loop
            // iterators ([~In] context); leave it for the caller.
            if (tag == .kw_in and !self.context.in) break;
            left = try self.completeBinaryExpression(left, tag, 0);
            continue;
        }

        // conditional expression
        if (tag == .@"?") {
            self.current_destructure_kind.setNoAssignOrDestruct();

            _ = try self.nextToken(); // eat '?'
            const true_expr = try self.assignExpressionNoPattern();
            _ = try self.expectToken(.@":");

            // [+In] grammar parameter
            const context = self.context;
            self.context.in = true;
            const false_expr = try self.assignExpressionNoPattern();
            self.context = context;

            const nodes = self.nodes.slice();
            const start_pos = nodes.items(.start)[@backingInt(left)];
            const end_pos = nodes.items(.end)[@backingInt(false_expr)];

            left = try self.addNode(.{
                .conditional_expr = .{
                    .condition = left,
                    .consequent = true_expr,
                    .alternate = false_expr,
                },
            }, .{ .start = start_pos, .end = end_pos });
            continue;
        }

        break;
    }

    const lhs_tag = self.nodeTag(left);
    assert(lhs_tag != .parameters);
    return left;
}

/// Parse an assignment expression, and ensure that its not a pattern
/// that must be destructured (e.g: `{x=1}`).
inline fn assignExpressionNoPattern(self: *Self) Error!Node.Index {
    const expr = try self.assignmentExpression();
    if (self.current_destructure_kind.must_destruct) {
        const expr_start = self.nodes.items(.start)[@backingInt(expr)];
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

/// Parse a binary expression with the given precedence, where the left-hand side
/// is [lhs], which may or may not be followed by a binary operator.
fn completeBinaryExpression(
    self: *Self,
    lhs: Node.Index,
    op_token: Token.Tag,
    current_prec: u8,
) Error!Node.Index {
    var expr = lhs;
    const start = self.nodes.items(.start)[@backingInt(lhs)];
    while (self.current.token.tag.is(TokenMask.IsBinaryOp)) {
        const cur = &self.current;
        const prec = cur.token.tag.precedence();

        if (prec <= current_prec) return expr;

        // `in` expressions are not allowed when parsing the LHS
        // for `for-loop` iterators.
        if (!self.context.in and cur.token.tag == .kw_in) break;

        const current_op = try self.next(); // eat the operator.
        if ((current_op.token.tag.isLogicalOperator() and op_token == .@"??") or
            (op_token.isLogicalOperator() and current_op.token.tag == .@"??"))
        {
            return self.errorOnToken(current_op.token, ParseError.InvalidCoalesceExpression);
        }

        const rhs_start_expr = try self.parseExponentExpression();
        // Parse the right-hand side of the binary expression.
        // 2 + 3 * 4 + 5
        //   ^---------  At this point, we parse `3` as `rhs_start`,
        //               then the next call to `completeBinaryExpression` parses
        //               `3 * 4` as `rhs` (that's as far as we can go with the current precedence),
        //                and then we return to this loop to parse `5` as the remainder.
        const rhs = try self.completeBinaryExpression(
            rhs_start_expr,
            current_op.token.tag,
            @intCast(prec),
        );

        const nodes = self.nodes.slice();
        const end = nodes.items(.end)[@backingInt(rhs)];

        const binary_pl = ast.BinaryPayload{ .lhs = expr, .rhs = rhs, .operator = ast.Operator.fromTag(current_op.token.tag) };
        expr = try self.addNode(.{ .binary_expr = binary_pl }, .{ .start = start, .end = end });
    }

    return expr;
}

fn yieldExpression(self: *Self) Error!Node.Index {
    const yield_kw = try self.next();
    assert(
        yield_kw.token.tag == .kw_yield and
            self.context.is_yield_reserved,
    );

    var has_operand = false;
    var is_delegated = false;
    if (self.current.token.line == yield_kw.token.line) {
        if (self.current.token.tag == .@"*") {
            _ = try self.nextToken();
            is_delegated = true;
            has_operand = true;
        } else {
            // `[ a = yield ] = 5;` is valid JS syntax,
            // and we shouldn't try to parse an expression
            // even though the '}' and 'yield' are on the same line.
            has_operand = switch (self.current.token.tag) {
                .@"]", .@"}", .@",", .@")", .@";" => false,
                else => true,
            };
        }
    }

    var end_byte = yield_kw.endByte();
    const operand: ?Node.Index = blk: {
        if (has_operand) {
            const operand_expr = try self.assignExpressionNoPattern();
            end_byte = self.nodes.items(.end)[@backingInt(operand_expr)];
            break :blk operand_expr;
        }

        break :blk null;
    };

    self.current_destructure_kind.setNoAssignOrDestruct();

    return self.addNode(.{
        .yield_expr = .{
            .value = operand,
            .is_delegated = is_delegated,
        },
    }, .{ .start = yield_kw.startByte(), .end = end_byte });
}

/// ConditionalExpression:
///     ShortCircuitExpression
///     ShortCircuitExpression '?' AssignmentExpression ':' AssignmentExpression
/// BindingPattern:
///     ArrayBindingPattern
///     ObjectBindingPattern
fn bindingPattern(self: *Self) Error!Node.Index {
    return switch (self.current.token.tag) {
        .@"{" => self.objectBindingPattern(),
        .@"[" => self.arrayBindingPattern(),
        else => {
            try self.emitBadTokenDiagnostic("binding pattern", &self.current.token);
            return Error.InvalidAssignmentTarget;
        },
    };
}

/// https://tc39.es/ecma262/#prod-ArrayBindingPattern
fn arrayBindingPattern(self: *Self) Error!Node.Index {
    const lbrac = try self.next(); // eat '['
    assert(lbrac.token.tag == .@"[");

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    var token = self.peek();
    while (true) : (token = self.peek()) {
        switch (token.tag) {
            .@"," => {
                const comma = try self.next(); // eat ','
                if (self.isAtToken(.@"]")) break;
                try self.scratch.append(self.allocator, try self.addNode(.{ .empty_array_item = {} }, spanBetween(comma, comma)));
            },

            .@"..." => {
                try self.scratch.append(self.allocator, try self.bindingRestElement());
                if (self.isAtToken(.@",")) {
                    const comma_tok = try self.nextToken();
                    try self.emitDiagnostic(
                        comma_tok.startCoord(self.source),
                        "a ',' is not allowed after a rest element",
                        .{},
                    );
                    return Error.InvalidAssignmentTarget;
                }

                break;
            },

            .@"]" => break,

            else => {
                try self.scratch.append(self.allocator, try self.bindingElement());
            },
        }

        if (self.current.token.tag == .@",") {
            _ = try self.nextToken();
        } else if (self.current.token.tag == .@"]") {
            break;
        }
    }

    const rbrac = try self.expect(.@"]"); // eat ']'
    const items = self.scratch.items[prev_scratch_len..];
    const array_items = try self.addSubRange(items);
    return self.addNode(.{ .array_pattern = array_items }, spanBetween(lbrac, rbrac));
}

fn completePropertyPatternDef(self: *Self, key: Node.Index) Error!Node.Index {
    _ = try self.expectToken(.@":");

    const value = try self.bindingElement();
    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(key)];
    const end_pos = nodes.items(.end)[@backingInt(value)];

    return self.addNode(.{ .object_property = .{ .key = key, .value = value } }, .{ .start = start_pos, .end = end_pos });
}

fn destructuredPropertyDefinition(self: *Self) Error!Node.Index {
    switch (self.current.token.tag) {
        // TODO(@injuly): we can use a mask check here
        .string_literal,
        .decimal_literal,
        .octal_literal,
        .hex_literal,
        .binary_literal,
        .legacy_octal_literal,
        => {
            const key_token = try self.next();

            const prev_destructure_kind = self.current_destructure_kind;
            const key = try self.parseLiteral(key_token);
            self.current_destructure_kind = prev_destructure_kind;

            return self.completePropertyPatternDef(key);
        },
        .@"[" => {
            _ = try self.nextToken(); // eat '['

            // The destructuring kind of the expression inside "[]" doesn't
            // matter. Only "value" (after ':') determines whether
            // this pattern is a valid object pattern for destructuring/assignment.
            // For example:
            // `{ [x]: null }` is an invalid LHS to '=', but
            // `{ [null]: x }` is valid.
            // But since calling `assignExpressionNoPattern` will update the
            // parser's 'current_destructure_kind', we need to save it and restore it later.
            const prev_destruct_kind = self.current_destructure_kind;
            const key = try self.assignExpressionNoPattern();
            self.current_destructure_kind = prev_destruct_kind;

            _ = try self.expectToken(.@"]");
            return self.completePropertyPatternDef(key);
        },
        else => {
            if (self.current.token.tag.isIdentifier() or
                self.current.token.tag.isKeyword())
            {
                return self.destructuredIdentifierProperty();
            }

            return try self.bindingElement();
        },
    }
}

/// Parse a destructured object property starting with an identifier.
/// Assumes that self.current.token is the .identifier.
fn destructuredIdentifierProperty(self: *Self) Error!Node.Index {
    const key_token = try self.next();

    const cur_token = self.peek();

    const key = try self.identifierReference(key_token.span());
    if (cur_token.tag == .@"=") {
        const eq_token = try self.next(); // eat '='
        const rhs = try self.assignmentExpression();
        const end_pos = self.nodes.items(.end)[@backingInt(rhs)];

        self.current_destructure_kind.must_destruct = true;
        self.current_destructure_kind.can_destruct = true;
        self.current_destructure_kind.can_be_assigned_to = true;

        return self.addNode(.{
            .assignment_pattern = .{
                .lhs = key,
                .rhs = rhs,
                .operator = ast.Operator.fromTag(eq_token.token.tag),
            },
        }, .{ .start = key_token.startByte(), .end = end_pos });
    }

    if (cur_token.tag == .@":") {
        return self.completePropertyPatternDef(key);
    }

    // Disallow stuff like "`{ if }`" (but `{ if: x }` is valid, handled above)
    if (!(key_token.token.tag.isIdentifier() or
        self.isKeywordIdentifier(key_token.token.tag)))
    {
        // TODO: use 'self.errorOnToken'
        try self.emitDiagnosticOnToken(
            key_token.token,
            "Unexpected '{s}' in destructuring pattern",
            .{key_token.token.toByteSlice(self.source)},
        );

        return Error.UnexpectedToken;
    }

    return self.addNode(.{ .shorthand_property = ast.ShorthandProperty{ .name = key } }, spanBetween(key_token, key_token));
}

/// https://tc39.es/ecma262/#prod-ObjectAssignmentPattern
fn objectBindingPattern(self: *Self) Error!Node.Index {
    const lbrace = try self.next(); // eat '{'
    assert(lbrace.token.tag == .@"{");

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    var end_pos = lbrace.endByte();

    var destruct_kind = self.current_destructure_kind;

    var cur_token = self.current.token;
    while (cur_token.tag != .@"}") : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"..." => {
                try self.scratch.append(self.allocator, try self.restElement());
                destruct_kind.update(self.current_destructure_kind);

                // TODO: we can continue parsing after the rest element,
                // and report this error later.
                const rb = try self.expect(.@"}");
                end_pos = rb.endByte();
                break;
            },

            .non_ascii_identifier,
            .identifier,
            .string_literal,
            .decimal_literal,
            .octal_literal,
            .hex_literal,
            .binary_literal,
            .legacy_octal_literal,
            .@"[",
            => {
                // TODO(@injuly): instead of calling `destructuredPropertyDefinition`,
                // for both binding patterns and regular patterns, write a separate
                // `destructuredBindingProperty` function that handles binding properties.
                const prop = try self.destructuredPropertyDefinition();
                self.reinterpretAsBindingPattern(prop);
                destruct_kind.update(self.current_destructure_kind);
                try self.scratch.append(self.allocator, prop);
            },

            else => {
                if (cur_token.tag.isKeyword()) {
                    const prop = try self.destructuredPropertyDefinition();
                    self.reinterpretAsBindingPattern(prop);
                    destruct_kind.update(self.current_destructure_kind);
                    try self.scratch.append(self.allocator, prop);
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
        if (comma_or_rbrace.token.tag == .@"}") {
            end_pos = comma_or_rbrace.endByte();
            break;
        }
    } else {
        const rbrace = try self.next();
        end_pos = rbrace.endByte();
    }

    const props = self.scratch.items[prev_scratch_len..];
    const destructured_props = try self.addSubRange(props);
    self.current_destructure_kind = destruct_kind;

    return self.addNode(.{ .object_pattern = destructured_props }, .{ .start = lbrace.startByte(), .end = end_pos });
}

/// BindingElement:
///     SingleNameBinding
///     BindingPattern (= AssignmentExpression)?
/// https://tc39.es/ecma262/#prod-BindingElement
fn bindingElement(self: *Self) Error!Node.Index {
    if (self.isIdentifier(self.current.token.tag)) {
        return self.singleNameBinding();
    }

    const pattern = try self.bindingPattern();
    if (!self.isAtToken(.@"=")) return pattern;

    const operator = try self.next(); // eat '='
    const rhs = try self.assignExpressionNoPattern();

    const nodes = self.nodes.slice();

    return self.addNode(.{
        .assignment_pattern = .{
            .lhs = pattern,
            .rhs = rhs,
            .operator = ast.Operator.fromTag(operator.token.tag),
        },
    }, .{ .start = nodes.items(.start)[@backingInt(pattern)], .end = nodes.items(.end)[@backingInt(rhs)] });
}

/// SingleNameBinding:
///     BindingIdentifier ('=' AssignmentExpression)?
/// Ref: https://tc39.es/ecma262/#prod-SingleNameBinding
fn singleNameBinding(self: *Self) Error!Node.Index {
    const id_token = try self.next();
    assert(self.isIdentifier(id_token.token.tag));

    const id = try self.bindingIdentifier(id_token.span());
    if (!self.isAtToken(.@"=")) return id;

    const op_token = try self.next(); // eat '='
    const rhs = try self.assignExpressionNoPattern();

    const start_pos = id_token.startByte();
    const end_pos = self.nodes.items(.end)[@backingInt(rhs)];

    return self.addNode(.{
        .assignment_pattern = .{
            .lhs = id,
            .rhs = rhs,
            .operator = ast.Operator.fromTag(op_token.token.tag),
        },
    }, .{ .start = start_pos, .end = end_pos });
}

// TODO: this isn't 100% spec compliant right now, but that's an easy
// fix really.
/// Parse an ExponentExpression: a unary-level operand, optionally
/// followed by a right-associative '**' chain.
fn parseExponentExpression(self: *Self) Error!Node.Index {
    const lhs = try self.parsePostfixExpression();
    if (self.current.token.tag != .@"**") return lhs;

    const op_token = try self.next();
    const rhs = try self.parseExponentExpression();

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(lhs)];
    const end_pos = nodes.items(.end)[@backingInt(rhs)];

    return self.addNode(.{
        .binary_expr = ast.BinaryPayload{
            .lhs = lhs,
            .rhs = rhs,
            .operator = ast.Operator.fromTag(op_token.token.tag),
        },
    }, .{ .start = start_pos, .end = end_pos });
}

/// Parse a unary-level operand: an optional prefix operator (unary,
/// prefix update, await) applied to a member/call chain, optionally
/// followed by a postfix '++'/'--'.
fn parsePostfixExpression(self: *Self) Error!Node.Index {
    switch (self.current.token.tag) {
        .kw_delete,
        .kw_typeof,
        .kw_void,
        .@"-",
        .@"+",
        .@"~",
        .@"!",
        => {
            // prefix unary operator; its operand includes the full
            // member/call chain ('-a.b' is '-(a.b)').
            const op = try self.next();
            const expr = try self.parsePostfixExpression();

            // Ensure that "delete Identifier" is not used in
            // strict mode code.
            const slice = self.nodes.slice();
            const expr_tag = meta.activeTag(slice.items(.data)[@backingInt(expr)]);
            if (self.context.strict and
                op.token.tag == .kw_delete and
                expr_tag == .identifier)
            {
                // TODO: instead of returning en error here.
                // we can continue parsing.
                return self.errorOnNode(expr, ParseError.IllegalDeleteInStrictMode);
            }

            const expr_end = slice.items(.end)[@backingInt(expr)];
            self.current_destructure_kind.setNoAssignOrDestruct();
            return try self.addNode(.{
                .unary_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = ast.Operator.fromTag(op.token.tag),
                },
            }, .{ .start = op.startByte(), .end = expr_end });
        },

        // prefix update expression
        .@"++",
        .@"--",
        => {
            const op_token = try self.next();
            const expr = try self.parsePostfixExpression();

            const is_operand_simple = self.current_destructure_kind.is_simple_expression;
            if (!is_operand_simple) {
                return self.errorOnToken(op_token.token, ParseError.InvalidUpdateOperand);
            }

            const expr_end_pos = self.nodes.items(.end)[@backingInt(expr)];
            self.current_destructure_kind.setNoAssignOrDestruct();

            return self.addNode(.{
                .update_expr = ast.UnaryPayload{
                    .operand = expr,
                    .operator = ast.Operator.fromTag(op_token.token.tag),
                },
            }, .{ .start = op_token.startByte(), .end = expr_end_pos });
        },

        .kw_await => if (self.context.is_await_reserved) {
            return self.awaitExpression();
        },

        else => {},
    }

    const expr_start_line = self.current.token.line;
    const expr = try self.parseLhsExpression();

    // postfix update: only valid on the same line as the expression start
    const cur_token = self.current.token;
    if ((cur_token.tag == .@"++" or cur_token.tag == .@"--") and
        cur_token.line == expr_start_line)
    {
        if (!self.current_destructure_kind.is_simple_expression) {
            try self.emitDiagnosticOnToken(cur_token, "Invalid operand for update expression", .{});
            return Error.UnexpectedToken;
        }

        self.current_destructure_kind.setNoAssignOrDestruct();
        const op_token = try self.next();
        const expr_start_pos = self.nodes.items(.start)[@backingInt(expr)];
        return self.addNode(.{
            .post_unary_expr = .{
                .operand = expr,
                .operator = ast.Operator.fromTag(op_token.token.tag),
            },
        }, .{ .start = expr_start_pos, .end = op_token.endByte() });
    }

    return expr;
}

/// Parse a LeftHandSideExpression: a prefix expression followed by the
/// member/call/optional chain. Unary operators are NOT allowed at this
/// level (used for class heritage and rest elements).
inline fn parseLhsExpression(self: *Self) Error!Node.Index {
    self.current_destructure_kind.is_simple_expression = true;

    var lhs_expr = try self.parsePrefixExpression();
    lhs_expr = try self.parsePostfixChain(lhs_expr, true);

    if (self.isAtToken(.@"?.")) {
        self.current_destructure_kind.is_simple_expression = false;
        lhs_expr = try self.optionalExpression(lhs_expr);
    }

    return lhs_expr;
}

/// Parse a primary-level expression: plain identifiers first,
/// keyword-led expressions ('new', 'import.meta', 'super'), and everything
/// in `primaryExpression`.
inline fn parsePrefixExpression(self: *Self) Error!Node.Index {
    const tag = self.current.token.tag;

    if (tag == .identifier) {
        @branchHint(.likely);
        // 'x => 1' arrows are handled in `assignmentExpression`'s loop,
        // which reinterprets a lone identifier reference as the parameter.
        const name_token = try self.next();
        return self.identifierReference(name_token.span());
    }

    if (tag == .kw_new) return self.newTargetOrExpression();
    if (tag == .kw_import) return self.importMetaOrCall();
    if (tag == .kw_super) return self.superPropertyOrCall();

    return self.primaryExpression();
}

/// Parse the member accesses, calls, computed members and tagged templates
/// chained on [object].
/// When [allow_calls] is false, '(' ends the chain: the callee of a 'new'
/// expression is a MemberExpression, its arguments are parsed by the caller.
fn parsePostfixChain(self: *Self, object: Node.Index, allow_calls: bool) Error!Node.Index {
    var expr = object;
    var first_call = true;
    var destruct_kind = self.current_destructure_kind;

    while (self.current.token.tag != .eof) {
        switch (self.current.token.tag) {
            .@"(" => {
                if (!allow_calls) break;
                if (first_call) {
                    // CoverCallAndAsyncArrowHead: 'async(x)' may turn out
                    // to be the head of an async arrow function.
                    expr = try self.coverCallAndAsyncArrowHead(expr);
                    destruct_kind = self.current_destructure_kind;
                } else {
                    expr = try self.completeCallExpression(expr);
                    destruct_kind.setNoAssignOrDestruct();
                }
            },
            .@"[" => {
                expr = try self.completeComputedMemberExpression(expr);
                destruct_kind.can_destruct = false;
                destruct_kind.can_be_assigned_to = true;
                destruct_kind.is_simple_expression = true;
            },
            .@"." => {
                expr = try self.completeMemberExpression(expr);
                destruct_kind.can_destruct = false;
                destruct_kind.can_be_assigned_to = true;
                destruct_kind.is_simple_expression = true;
            },
            .template_literal_part => {
                expr = try self.completeTaggedTemplate(expr);
                destruct_kind.setNoAssignOrDestruct();
            },
            else => break,
        }
        first_call = false;
    }

    self.current_destructure_kind = destruct_kind;
    return expr;
}

/// Parse a MemberExpression: a prefix expression followed by member
/// accesses, computed members and tagged templates (no calls).
/// Used for the callee of 'new' expressions.
fn parseMemberExpression(self: *Self) Error!Node.Index {
    const member_expr = switch (self.current.token.tag) {
        .kw_new => try self.newTargetOrExpression(),
        .kw_import => try self.importMetaOrCall(),
        .kw_super => try self.superPropertyOrCall(),
        else => try self.parsePrefixExpression(),
    };
    return self.parsePostfixChain(member_expr, false);
}

/// Parse an await expression when `self.current.token`
/// is the `await` keyword.
fn awaitExpression(self: *Self) Error!Node.Index {
    const await_token = try self.next();
    assert(await_token.token.tag == .kw_await);

    if (!self.context.is_await_reserved) {
        try self.emitDiagnostic(
            await_token.token.startCoord(self.source),
            "'await' expressions are only permitted inside async functions",
            .{},
        );
        return Error.IllegalAwait;
    }

    const operand = try self.parsePostfixExpression();
    const end_pos = self.nodes.items(.end)[@backingInt(operand)];

    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{
        .await_expr = ast.UnaryPayload{
            .operator = ast.Operator.fromTag(await_token.token.tag),
            .operand = operand,
        },
    }, .{ .start = await_token.startByte(), .end = end_pos });
}

fn superExpression(self: *Self) Error!Node.Index {
    const super_token = try self.nextToken();
    // TODO: check if we're in a surrounding class.
    assert(super_token.tag == .kw_super);

    const super_args, const start, const end = try self.parseArgs();
    return self.addNode(.{ .super_call_expr = super_args }, .{ .start = start, .end = end });
}

/// Parse a 'new' expression if 'current_token' is 'new',
/// otherwise return `null`.
fn tryNewExpression(self: *Self) Error!?Node.Index {
    if (self.isAtToken(.kw_new)) {
        const new_token = try self.nextToken();
        return try self.completeNewExpression(&new_token);
    }
    return null;
}

fn completeNewExpression(self: *Self, new_start: u32) Error!Node.Index {
    const expr = try self.parseMemberExpression();
    const expr_end_pos = self.nodes.items(.end)[@backingInt(expr)];

    const arguments: ?ast.SubRange = if (self.isAtToken(.@"("))
        (try self.parseArgs())[0]
    else
        null;

    return try self.addNode(.{
        .new_expr = .{
            .callee = expr,
            .arguments = arguments,
        },
    }, .{ .start = new_start, .end = expr_end_pos });
}

/// When we're at the '(' token, parse the arguments and return a complete call expression
/// ```js
/// myFunc(...
/////    ^-- already parsed the callee
/// ```
fn completeCallExpression(self: *Self, callee: Node.Index) Error!Node.Index {
    const call_args = try self.parseArgs();
    const call_expr = ast.CallExpr{
        .arguments = call_args[0],
        .callee = callee,
    };

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(callee)];
    const end_pos = call_args[2];
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{ .call_expr = call_expr }, .{ .start = start_pos, .end = end_pos });
}

// CoverCallAndAsyncArrowHead:  MemberExpression Arguments
fn coverCallAndAsyncArrowHead(self: *Self, callee: Node.Index) Error!Node.Index {
    const call_args = try self.parseArgs();
    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(callee)];
    const end_pos = call_args[2];

    self.current_destructure_kind.setNoAssignOrDestruct();

    return self.addNode(.{
        .call_expr = .{
            .callee = callee,
            .arguments = call_args[0],
        },
    }, .{ .start = start_pos, .end = end_pos });
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
    const start_pos = self.nodes.items(.start)[@backingInt(expr)];

    var cur_token = self.peek();
    while (cur_token.tag != .eof) : (cur_token = self.peek()) {
        switch (cur_token.tag) {
            .@"[" => {
                const member_expr = try self.completeComputedMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@backingInt(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, .{ .start = start_pos, .end = end_pos });
            },
            .@"." => {
                const member_expr = try self.completeMemberExpression(expr);
                const end_pos = self.nodes.items(.end)[@backingInt(member_expr)];
                expr = try self.addNode(.{ .optional_expr = member_expr }, .{ .start = start_pos, .end = end_pos });
            },
            .@"(" => {
                const call_expr = try self.completeCallExpression(expr);
                const end_pos = self.nodes.items(.end)[@backingInt(call_expr)];
                expr = try self.addNode(.{ .optional_expr = call_expr }, .{ .start = start_pos, .end = end_pos });
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
    const start_pos = self.nodes.items(.start)[@backingInt(object)];

    const chain_op = try self.nextToken();
    assert(chain_op.tag == .@"?.");

    const cur_token = self.peek();

    switch (cur_token.tag) {
        .@"(" => {
            const call_args = try self.parseArgs();
            const end_pos = call_args[2];
            const call_expr = try self.addNode(.{
                .call_expr = .{
                    .arguments = call_args[0],
                    .callee = object,
                },
            }, .{ .start = start_pos, .end = end_pos });
            return self.addNode(.{ .optional_expr = call_expr }, .{ .start = start_pos, .end = end_pos });
        },
        .@"[" => {
            const expr = try self.completeComputedMemberExpression(object);
            const end_pos = self.nodes.items(.end)[@backingInt(expr)];
            return self.addNode(.{ .optional_expr = expr }, .{ .start = start_pos, .end = end_pos });
        },
        else => {
            if (self.current.token.tag.isIdentifier() or self.current.token.tag.isKeyword()) {
                const prop_name_token = try self.next(); // eat the property name
                const end_pos = prop_name_token.endByte();

                const expr = try self.addNode(.{ .member_expr = .{
                    .object = object,
                    .property = try self.identifier(prop_name_token.span()),
                } }, .{ .start = start_pos, .end = end_pos });
                return self.addNode(.{ .optional_expr = expr }, .{ .start = start_pos, .end = end_pos });
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

/// A `new.target` meta property, or a regular NewExpression
/// https://tc39.es/ecma262/#prod-NewTarget
/// https://tc39.es/ecma262/#prod-NewExpression
fn newTargetOrExpression(self: *Self) Error!Node.Index {
    const new_token = try self.next();
    assert(new_token.token.tag == .kw_new);
    if (self.isAtToken(.@"."))
        return self.parseMetaProperty(new_token.token, "target");
    // No '.' after 'new', so we're parsing a regular old
    // NewExpression.
    return self.completeNewExpression(new_token.startByte());
}

/// Parse an `import.meta` meta property, or an ImportCall:
/// https://tc39.es/ecma262/#prod-ImportMeta
/// https://tc39.es/ecma262/#prod-ImportCall
fn importMetaOrCall(self: *Self) Error!Node.Index {
    const import_token = try self.next();
    if (self.isAtToken(.@"."))
        return try self.parseMetaProperty(import_token.token, "meta");

    // TODO: support "ImportCall":
    // https://tc39.es/ecma262/#prod-ImportCall
    try self.emitDiagnosticOnToken(import_token.token, "Unexpected 'import'", .{});
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
    assert(super_token.token.tag == .kw_super);

    switch (self.current.token.tag) {
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
            return self.addNode(.{ .super_call_expr = super_args }, .{ .start = start, .end = end });
        },
    }
}

/// Parse a 'MetaProperty' like `new.target` or `import.meta`.
/// Assumes that `self.current.token` is the `new` or `import` keyword,
/// and that current_token is a '.'
fn parseMetaProperty(
    self: *Self,
    meta_token: Token,
    wanted_property_name: []const u8,
) Error!Node.Index {
    const meta_token_span = ast.Span{
        .start = meta_token.start,
        .end = meta_token.start + meta_token.len,
    };
    const dot = try self.nextToken();
    assert(dot.tag == .@".");

    const property_token = try self.expectIdentifier();
    const property_name_str = property_token.token.toByteSlice(self.source);
    if (!std.mem.eql(u8, property_name_str, wanted_property_name)) {
        try self.emitDiagnostic(
            property_token.token.startCoord(self.source),
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

    const meta_object = try self.identifier(meta_token_span);
    const property = try self.identifier(property_token.span());

    const end_pos = property_token.endByte();
    return self.addNode(.{
        .meta_property = .{
            .meta = meta_object,
            .property = property,
        },
    }, .{ .start = meta_token_span.start, .end = end_pos });
}

/// When `current_token` is a template literal part, and a member_expr has been parsed
/// this function parses a tagged template expression like "div`hello`"
fn completeTaggedTemplate(self: *Self, tag: Node.Index) Error!Node.Index {
    const template = try self.templateLiteral();

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(tag)];
    const end_pos = nodes.items(.end)[@backingInt(template)];

    return self.addNode(.{
        .tagged_template_expr = .{
            .tag = tag,
            .template = template,
        },
    }, .{ .start = start_pos, .end = end_pos });
}

fn completeMemberExpression(self: *Self, object: Node.Index) Error!Node.Index {
    const dot = try self.nextToken(); // eat "."
    assert(dot.tag == .@".");

    const property_id: Node.Index = blk: {
        const tok_with_id = try self.next();
        // Yes, keywords are valid property names...
        if (tok_with_id.token.tag.isIdentifier() or tok_with_id.token.tag.isKeyword()) {
            break :blk try self.identifier(tok_with_id.span());
        }

        try self.emitDiagnostic(
            tok_with_id.token.startCoord(self.source),
            "Expected to see a property name after '.', got a '{s}' instead",
            .{tok_with_id.token.toByteSlice(self.source)},
        );
        return Error.UnexpectedToken;
    };

    const property_access = ast.PropertyAccess{
        .object = object,
        .property = property_id,
    };

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(object)];
    const end_pos = nodes.items(.end)[@backingInt(property_id)];
    self.current_destructure_kind.can_destruct = false;
    self.current_destructure_kind.can_be_assigned_to = true;
    self.current_destructure_kind.is_simple_expression = true;
    return self.addNode(.{ .member_expr = property_access }, .{ .start = start_pos, .end = end_pos });
}

fn completeComputedMemberExpression(self: *Self, object: Node.Index) Error!Node.Index {
    // Apply the [+In] grammar parameter.
    // in expressions are allowed in places like: for([a in b] in c)
    const old_ctx = self.context;
    self.context.in = true;
    defer self.context = old_ctx;

    const lbrace = try self.nextToken(); // eat "["
    assert(lbrace.tag == .@"[");

    const property = try self.expression();
    const rbrace = try self.expect(.@"]");

    const property_access = ast.ComputedPropertyAccess{
        .object = object,
        .property = property,
    };

    self.current_destructure_kind.can_destruct = false;
    self.current_destructure_kind.can_be_assigned_to = true;
    self.current_destructure_kind.is_simple_expression = true;

    const start_pos = self.nodes.items(.start)[@backingInt(object)];
    return self.addNode(.{ .computed_member_expr = property_access }, .{ .start = start_pos, .end = rbrace.endByte() });
}

fn primaryExpression(self: *Self) Error!Node.Index {
    const cur = &self.current.token;
    if (cur.tag == .@"/" or cur.tag == .@"/=") {
        // If we're currently at a '/' or '/=' token,
        // we probably have mistaken a regex literal's opening '/' for an operator.
        // We'll rewind the tokenizer and try to parse a regex literal instead.
        try self.reScanRegexLiteral();
        assert(self.current.token.tag == .regex_literal);

        const regex_token = try self.next();
        return self.addNode(.{ .regex_literal = regex_token.span() }, spanBetween(regex_token, regex_token));
    } else if (cur.tag == .template_literal_part) {
        return self.templateLiteral();
    }

    switch (cur.tag) {
        .kw_class => return self.classExpression(),
        .kw_this => {
            const this = try self.next();
            self.current_destructure_kind.setNoAssignOrDestruct();
            return self.addNode(.{ .this = this.span() }, spanBetween(this, this));
        },

        .non_ascii_identifier, .identifier => {
            const name_token = try self.next();
            return self.identifierReference(name_token.span());
        },
        .decimal_literal,
        .octal_literal,
        .hex_literal,
        .binary_literal,
        .legacy_octal_literal,
        .regex_literal,
        .string_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => return self.parseLiteral(try self.next()),
        .@"[" => return self.arrayLiteral(),
        .@"{" => return self.objectLiteral(),
        .@"(" => return self.groupingExprOrArrowParameters(),
        .kw_async => return self.asyncExpression(),
        .kw_function => {
            const func_kw = try self.next();
            return self.functionExpression(func_kw.startByte(), .{});
        },
        .@"<" => {
            if (self.config.jsx) {
                const jsx_expr = self.jsxFragmentOrElement();
                self.current_destructure_kind.setNoAssignOrDestruct();
                return jsx_expr;
            } else {
                return self.errorOnToken(self.current.token, ParseError.LAngleWithoutJsx);
            }
        },
        else => {},
    }

    if (self.isKeywordIdentifier(self.current.token.tag)) {
        const name_token = try self.next();
        return self.identifierReference(name_token.span());
    }

    try self.emitDiagnostic(
        self.current.token.startCoord(self.source),
        "Unexpected '{s}'",
        .{
            if (self.current.token.tag == .eof)
                "end of input"
            else
                self.current.token.toByteSlice(self.source),
        },
    );
    return Error.UnexpectedToken;
}

/// An extension of the 'PrimaryExpression' grammar specified in the JSX spec.
/// JSXExpression:
///     JSXElement
///     JSXFragment
fn jsxFragmentOrElement(self: *Self) Error!Node.Index {
    assert(self.current.token.tag == .@"<");

    self.jsx_nesting_depth += 1;
    defer self.jsx_nesting_depth -= 1;

    const lt_token = try self.next(); // eat '<'
    return switch (self.current.token.tag) {
        .@">" => self.jsxFragment(lt_token.startByte()),
        .@">>", .@">=", .@">>=", .@">>>=", .@">>>" => blk: {
            self.reScanJsxGt();
            break :blk self.jsxFragment(lt_token.startByte());
        },
        else => self.jsxElement(lt_token.startByte()),
    };
}

/// JSXElement:
///     JSXOpeningElement JSXChildren? JSXClosingElement
///     JSXSelfClosingElement
///
/// https://facebook.github.io/jsx/#prod-JSXElement
fn jsxElement(self: *Self, lt_start: u32) Error!Node.Index {
    const opening_element_name = try self.jsxElementName();
    const opening_element_attrs = try self.jsxAttributes();
    if (self.isAtToken(.@"/")) {
        return self.jsxSelfClosingElement(
            lt_start,
            opening_element_name,
            opening_element_attrs,
        );
    }

    const opening_element = try self.jsxOpeningElement(
        lt_start,
        opening_element_name,
        opening_element_attrs,
    );

    const children: Node.Index = blk: {
        const prev_scratch_len = self.scratch.items.len;
        defer self.scratch.items.len = prev_scratch_len;

        while (try self.tryJsxChild()) |jsx_child| {
            try self.scratch.append(self.allocator, jsx_child);
        }

        const children_slice = self.scratch.items[prev_scratch_len..];
        if (children_slice.len == 0) break :blk Node.Index.empty;

        const sub_range = try self.addSubRange(children_slice);
        const nodes = self.nodes.slice();
        const children_start = nodes.items(.start)[@backingInt(children_slice[0])];
        const end_token = nodes.items(.end)[@backingInt(children_slice[children_slice.len - 1])];

        break :blk try self.addNode(.{ .jsx_children = sub_range }, .{ .start = children_start, .end = end_token });
    };

    const closing_element = try self.jsxClosingElement();
    const end_token = self.nodes.items(.end)[@backingInt(closing_element)];
    return self.addNode(.{
        .jsx_element = .{
            .opening_element = opening_element,
            .closing_element = closing_element,
            .children = children,
        },
    }, .{ .start = lt_start, .end = end_token });
}

/// JSXSelfClosingElement:
///     '<' JSXElementName JSXAttributes? '/' '>'
fn jsxSelfClosingElement(
    self: *Self,
    lt_start: u32,
    name: Node.Index,
    attributes: ast.SubRange,
) Error!Node.Index {
    const slash_token = try self.nextToken(); // eat '/'
    assert(slash_token.tag == .@"/");

    const gt_token = if (self.jsx_nesting_depth > 1)
        try self.expectJsxRAngleForOpeningTag()
    else
        try self.expect(.@">");

    return self.addNode(.{
        .jsx_self_closing_element = .{
            .name = name,
            .attributes = attributes,
        },
    }, .{ .start = lt_start, .end = gt_token.endByte() });
}

/// JSXOpeningElement:
///     '<' JSXElementName JSXAttributes? '>'
fn jsxOpeningElement(
    self: *Self,
    lt_start: u32,
    name: Node.Index,
    attrs: ast.SubRange,
) Error!Node.Index {
    // Consume the current '>' token, and lex the remaining as JSX text.
    const gt_token = try self.expectJsxRAngleForOpeningTag();
    return self.addNode(.{
        .jsx_opening_element = ast.JsxOpeningElement{
            .name = name,
            .attributes = attrs,
        },
    }, .{ .start = lt_start, .end = gt_token.endByte() });
}

/// JSXClosingElement:
///     < / JSXElementName >
fn jsxClosingElement(self: *Self) Error!Node.Index {
    const lt_token = try self.expect(.@"<");
    _ = try self.expect(.@"/");
    const name = try self.jsxElementName();
    const gt_token = if (self.jsx_nesting_depth > 1)
        try self.expectJsxRAngleForOpeningTag()
    else
        try self.expect(.@">");

    return self.addNode(.{
        .jsx_closing_element = ast.JsxClosingElement{ .name = name },
    }, spanBetween(lt_token, gt_token));
}

/// Parse a list of JSX attributes and return a `SubRange` containing them.
fn jsxAttributes(self: *Self) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (self.current.token.tag.is(TokenMask.IsJsxAttributeStart)) {
        if (self.isAtToken(.@"{")) {
            try self.scratch.append(self.allocator, try self.jsxSpreadAttribute());
        } else {
            try self.scratch.append(self.allocator, try self.jsxAttribute());
        }
    }

    const children = self.scratch.items[prev_scratch_len..];
    return self.addSubRange(children);
}

/// https://facebook.github.io/jsx/#prod-JSXAttribute
/// JSXAttribute:
///     JSXIdentifier
///     JSXNamespacedName
fn jsxAttribute(self: *Self) Error!Node.Index {
    var attr_name = try self.parseJsxIdentifier();
    if (self.isAtToken(.@":")) {
        attr_name = try self.parseJsxNamespacedName(attr_name);
    }

    var attr_value: ?Node.Index = null;
    if (self.isAtToken(.@"=")) {
        _ = try self.next(); // eat '='
        attr_value = try self.jsxAttributeValue();
    }

    const start_pos = self.nodes.items(.start)[@backingInt(attr_name)];
    const end_pos = if (attr_value) |val|
        self.nodes.items(.end)[@backingInt(val)]
    else
        self.nodes.items(.end)[@backingInt(attr_name)];

    return self.addNode(.{
        .jsx_attribute = ast.JsxAttribute{
            .name = attr_name,
            .value = attr_value,
        },
    }, .{ .start = start_pos, .end = end_pos });
}

/// JSXSpreadAttribute:
///     '{' '...' AssignmentExpression '}'
fn jsxSpreadAttribute(self: *Self) Error!Node.Index {
    const start_brace = try self.expect(.@"{");
    _ = try self.expect(.@"...");

    const expr = try self.assignExpressionNoPattern();
    const close_brace = try self.expect(.@"}");

    return self.addNode(.{
        .jsx_spread_attribute = expr,
    }, spanBetween(start_brace, close_brace));
}

/// https://facebook.github.io/jsx/#prod-JSXAttributeValue
fn jsxAttributeValue(self: *Self) Error!Node.Index {
    switch (self.current.token.tag) {
        .@"<" => return self.jsxFragmentOrElement(),
        .@"{" => {
            const open_brace = try self.next();
            const expr = try self.assignExpressionNoPattern();
            const close_brace = try self.expect(.@"}");
            return self.addNode(.{ .jsx_expression = expr }, spanBetween(open_brace, close_brace));
        },
        // TODO: JSX strings have different rules, check with the spec.
        .string_literal => {
            const string_token = try self.next();
            return self.stringLiteralFromToken(string_token.span());
        },
        else => {
            try self.emitDiagnosticOnToken(
                self.current.token,
                "Unexpected '{s}' when looking for JSX attribute value",
                .{self.current.token.toByteSlice(self.source)},
            );
            return Error.UnexpectedToken;
        },
    }
}

/// Parse the name of a JSX element.
///
/// JSXElementName:
///     JSXIdentifier
///     JSXMemberExpression
///     JSXNamespacedName
///
/// https://facebook.github.io/jsx/#prod-JSXElementName
fn jsxElementName(self: *Self) Error!Node.Index {
    var name = try self.parseJsxIdentifierReference();

    if (self.current.token.tag == .@".") {
        name = try self.parseJsxMemberExpression(name);
        while (self.current.token.tag == .@".") {
            name = try self.parseJsxMemberExpression(name);
        }
        return name;
    } else if (self.current.token.tag == .@":") {
        return try self.parseJsxNamespacedName(name);
    }

    return name;
}

fn parseJsxIdentifierReference(self: *Self) Error!Node.Index {
    const identifier_token = try self.parseJsxIdentifierToken();
    return self.addNode(.{ .jsx_identifier_reference = identifier_token.span() }, spanBetween(identifier_token, identifier_token));
}

fn parseJsxIdentifier(self: *Self) Error!Node.Index {
    const identifier_token = try self.parseJsxIdentifierToken();
    return self.addNode(.{ .jsx_identifier = identifier_token.span() }, spanBetween(identifier_token, identifier_token));
}

/// This function will MODIFY the current token in the self.tokens buffer, and turn it into
/// a '.jsx_identifier' token.
///
/// If the current token is not a valid keyword or identifier, an error will be emitted instead.
fn parseJsxIdentifierToken(self: *Self) Error!TokenWithId {
    const identifier_or_kw = self.current.token;
    if (!(identifier_or_kw.tag.isIdentifier() or identifier_or_kw.tag.isKeyword())) {
        try self.emitBadTokenDiagnostic("a JSX name", &identifier_or_kw);
        return Error.UnexpectedToken;
    }

    _ = try self.continueJsxIdentifier();
    return try self.next(); // TODO: this is weird code
}

/// JSXMemberExpression:
///     JSXIdentifier '.' JSXIdentifier
///     JSXMemberExpresion '.' JSXIdentifier
///
/// https://facebook.github.io/jsx/#prod-JSXMemberExpression
fn parseJsxMemberExpression(self: *Self, jsx_identifier: Node.Index) Error!Node.Index {
    assert(self.nodeTag(jsx_identifier) == .jsx_identifier_reference);
    assert(self.current.token.tag == .@".");

    const name_start = self.nodes.items(.start)[@backingInt(jsx_identifier)];
    var end_token = name_start;

    var jsx_member_expr = jsx_identifier;
    while (self.isAtToken(.@".")) {
        _ = try self.next();

        const property_name_token = try self.parseJsxIdentifierToken();
        const property_name = try self.addNode(.{ .jsx_identifier = property_name_token.span() }, spanBetween(property_name_token, property_name_token));

        end_token = property_name_token.endByte();
        jsx_member_expr = try self.addNode(.{
            .jsx_member_expression = .{
                .object = jsx_member_expr,
                .property = property_name,
            },
        }, .{ .start = name_start, .end = end_token });
    }

    return jsx_member_expr;
}

/// JSXNamespacedName :
///     JSXIdentifier ':' JSXIdentifier
fn parseJsxNamespacedName(self: *Self, namespace: Node.Index) Error!Node.Index {
    const namespace_tag = self.nodeTag(namespace);
    assert(namespace_tag == .jsx_identifier or namespace_tag == .jsx_identifier_reference);
    assert(self.current.token.tag == .@":");

    const name_start = self.nodes.items(.start)[@backingInt(namespace)];
    _ = try self.next(); // eat ':'

    const name_token = try self.parseJsxIdentifierToken();
    const name = try self.addNode(.{ .jsx_identifier = name_token.span() }, spanBetween(name_token, name_token));

    const end_token = name_token.endByte();
    return self.addNode(.{
        .jsx_namespaced_name = .{
            .namespace = namespace,
            .name = name,
        },
    }, .{ .start = name_start, .end = end_token });
}

/// JSXFragment:
///     <> JSXChildren* </>
/// https://facebook.github.io/jsx/#prod-JSXFragment
fn jsxFragment(self: *Self, opening_lt_start: u32) Error!Node.Index {
    // eat the '>' and consume the following characters as a JSX token.
    const opening_gt = try self.nextJsx();
    assert(opening_gt.token.tag == .@">");

    const jsx_children = try self.jsxChildren();

    // eat the closing '</>'
    const closing_lt = try self.expect(.@"<");
    _ = try self.expectToken(.@"/");
    const closing_gt = try if (self.jsx_nesting_depth > 1)
        // If nested inside another JSX element, lex the part after '>' as JSX text or '<' or '{'
        self.expectJsxRAngleForOpeningTag()
    else
        // If this is a top-level JSX tag, lex the part after '>' as a regular JS token.
        self.expect(.@">");

    const jsx_open_close_tags = try self.addExtraData(ast.ExtraData{
        .jsx_fragment_indices = .{
            // The '<' in the opening "<>"
            .opening_lt = .{ .start = opening_lt_start, .end = opening_lt_start + 1 },
            // The '<' in the closing "</>"
            .closing_lt = closing_lt.span(),
        },
    });

    return self.addNode(.{
        .jsx_fragment = .{
            .children = jsx_children,
            .open_close_tags = jsx_open_close_tags,
        },
    }, .{ .start = opening_lt_start, .end = closing_gt.endByte() });
}

/// https://facebook.github.io/jsx/#prod-JSXChildren
/// JSXChildren: JSXChild+
fn jsxChildren(self: *Self) Error!ast.SubRange {
    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    while (try self.tryJsxChild()) |jsx_child| {
        try self.scratch.append(self.allocator, jsx_child);
    }

    const children = self.scratch.items[prev_scratch_len..];
    return self.addSubRange(children);
}

/// Try to parse a JSXChild. If no JSXChild can be parsed,
/// return null.
///
/// https://facebook.github.io/jsx/#prod-JSXChild
/// JSXChild:
///     JSXText
///     JSXElement
///     JSXFragment
///     { JSXChildExpression }
fn tryJsxChild(self: *Self) Error!?Node.Index {
    switch (self.current.token.tag) {
        .@"<" => {
            // '</' means we're at a closing tag.
            const look_ahead = try self.lookAhead();
            if (look_ahead.tag == .@"/") return null;
            return try self.jsxFragmentOrElement();
        },
        .@"{" => return try self.jsxExpression(),
        .jsx_text => {
            const text_token = try self.nextJsx();
            return try self.addNode(.{ .jsx_text = text_token.span() }, spanBetween(text_token, text_token));
        },
        else => return null,
    }
}

/// { JSXChildExpression }
/// where
/// JSXChildExpression:
///     ... AssignmentExpression
///     AssignmentExpression
///
/// https://facebook.github.io/jsx/#prod-JSXChildExpression
fn jsxExpression(self: *Self) Error!Node.Index {
    assert(self.current.token.tag == .@"{");

    // Eat the '{', and consume the next token as a regular
    // JS token, not a token within a JSX text.
    const lbrace = try self.next();

    if (self.isAtToken(.@"...")) {
        _ = try self.next(); // eat "..."
        const expr = try self.assignExpressionNoPattern();
        const rbrace = try self.expectJsx(.@"}");
        return self.addNode(.{ .jsx_spread_child = expr }, spanBetween(lbrace, rbrace));
    }

    const expr = if (!self.isAtToken(.@"}"))
        try self.assignExpressionNoPattern()
    else
        Node.Index.empty;

    const rbrace = try self.expectJsx(.@"}");
    return self.addNode(.{ .jsx_expression = expr }, spanBetween(lbrace, rbrace));
}

/// Assuming the current token is a regular identifier or keyword, continue parsing it
/// as a JSXIdentifier (that allows '-' characters).
fn continueJsxIdentifier(self: *Self) Error!TokenWithId {
    assert(self.current.token.tag.isIdentifier() or self.current.token.tag.isKeyword());

    // get rid of the current identifier (or kw) token, because we want to relex and replace it
    _ = self.tokens.pop();

    const new_token = try self.tokenizer.continueJsxIdentifier(&self.current.token);
    return self.advanceToToken(new_token);
}

/// Check whether a numeric literal is valid in strict mode.
/// Decimals and legacy octal literals are not allowed (e.g: 01, 09, 023127, etc.).
fn isValidStrictModeNumber(self: *const Self, token: *const Token) bool {
    assert(token.tag.isNumericLiteral() and token.tag != .legacy_octal_literal);
    if (token.len == 1) return true; // Just a '0' is fine.
    // 0x, 0b, 0o are Ok. 09 is not.
    return !(self.source[token.start] == '0' and std.ascii.isDigit(self.source[token.start + 1]));
}

fn parseLiteral(self: *Self, token: TokenWithId) Error!Node.Index {
    const token_tag = token.token.tag;
    if (self.context.strict and token_tag.isNumericLiteral()) {
        if (token_tag == .legacy_octal_literal) {
            return self.errorOnToken(token.token, ParseError.IllegalStrictModeOctal);
        }

        if (!self.isValidStrictModeNumber(&token.token)) {
            return self.errorOnToken(token.token, ParseError.IllegalStrictModeNumber);
        }

        self.current_destructure_kind.setNoAssignOrDestruct();
        const numeric_literal = ast.NodeData{ .number_literal = try self.parseNumericToken(&token) };
        return self.addNode(numeric_literal, spanBetween(token, token));
    }

    self.current_destructure_kind.setNoAssignOrDestruct();
    const payload = switch (token.token.tag) {
        .string_literal => ast.NodeData{ .string_literal = token.span() },
        .kw_null => ast.NodeData{ .null_literal = token.span() },
        .kw_false, .kw_true => ast.NodeData{
            .boolean_literal = .{
                .token = token.span(),
                .value = token.token.tag == .kw_true,
            },
        },
        .regex_literal => ast.NodeData{ .regex_literal = token.span() },
        else => ast.NodeData{ .number_literal = try self.parseNumericToken(&token) },
    };

    return self.addNode(payload, spanBetween(token, token));
}

fn parseNumericToken(self: *Self, token: *const TokenWithId) Error!ast.Number {
    const str = token.token.toByteSlice(self.source);
    const value: f64 = switch (token.token.tag) {
        .legacy_octal_literal,
        .octal_literal,
        => parseOctal(str),
        // TODO(perf): non-floating point numbers can be parsed with `parseInt`, actually.
        .decimal_literal => parseDecimal(str),
        .binary_literal => parseBinary(str),
        .hex_literal => parseHex(str),
        else => unreachable,
    };

    return ast.Number{
        .token = token.span(),
        .value_id = try self.addExtraData(
            ast.ExtraData{ .number_value = value },
        ),
    };
}

fn parseDecimal(str: []const u8) f64 {
    return std.fmt.parseFloat(f64, str) catch
        std.debug.panic("Invalid number: {s}\n", .{str});
}

fn parseOctal(str: []const u8) f64 {
    assert(str[0] == '0');

    const numeric = if (str[1] == 'O' or str[1] == 'o')
        str[2..]
    else
        str[1..];

    var val: i64 = 0;
    for (numeric) |ch| {
        if (ch == '_') continue;
        val = val * 8 + @as(i64, @intCast(ch - '0'));
    }

    return @floatFromInt(val);
}

fn parseBinary(str: []const u8) f64 {
    assert(str[0] == '0');
    assert(str[1] == 'b' or str[1] == 'B');

    var val: i64 = 0;
    for (str[2..]) |ch| {
        if (ch == '_') continue;
        val = val * 2 + @as(i64, @intCast(ch - '0'));
    }
    return @floatFromInt(val);
}

fn parseHex(str: []const u8) f64 {
    assert(str[0] == '0');
    assert(str[1] == 'x' or str[1] == 'X');

    var val: i64 = 0;
    for (str[2..]) |ch| {
        val <<= 4;
        switch (ch) {
            '0'...'9' => val |= @intCast(ch - '0'),
            'a'...'f' => val |= @intCast(ch - 'a' + 10),
            'A'...'F' => val |= @intCast(ch - 'A' + 10),
            '_' => {},
            else => unreachable,
        }
    }

    return @floatFromInt(val);
}

fn stringLiteral(self: *Self) Error!Node.Index {
    const token = try self.expect(.string_literal);
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{ .string_literal = token.span() }, spanBetween(token, token));
}

fn stringLiteralFromToken(
    self: *Self,
    token_span: ast.Span,
) Error!Node.Index {
    self.current_destructure_kind.setNoAssignOrDestruct();
    return self.addNode(.{ .string_literal = token_span }, token_span);
}

/// Parse an arrow function that starts with an identifier token.
/// E.g: `x => 1` or `yield => 2`
/// Parse a template literal expression.
fn templateLiteral(self: *Self) Error!Node.Index {
    // Handle the [+In] grammar parameter
    // to allow `for(`${foo in bar}` in baz)`;
    const ctx = self.context;
    self.context.in = true;
    defer self.context = ctx;

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    var template_token = try self.next();
    assert(template_token.token.tag == .template_literal_part);

    const start_pos = template_token.startByte();
    var end_pos = template_token.endByte();

    try self.scratch.append(self.allocator, try self.addNode(.{ .template_element = template_token.span() }, spanBetween(template_token, template_token)));

    while (!self.isTemplateEndToken(&template_token.token)) {
        // parse an interpolation expression.
        try self.scratch.append(self.allocator, try self.expression());

        // After parsing the interpolated expression,
        // the current token should be a '}'. Now, we re-scan starting
        // from '}' to the next '${', or the end of the template literal.
        if (self.current.token.tag != .@"}") {
            try self.emitBadTokenDiagnostic("'}}' after template expression", &self.current.token);
            return Error.UnexpectedToken;
        }

        try self.reScanTemplatePart();
        template_token = try self.next();

        // Now, parse the template part that follows
        try self.scratch.append(self.allocator, try self.addNode(.{ .template_element = template_token.span() }, spanBetween(template_token, template_token)));
        end_pos = template_token.endByte();
    }

    const template_parts = self.scratch.items[prev_scratch_len..];
    const elements = try self.addSubRange(template_parts);
    return try self.addNode(.{ .template_literal = elements }, .{ .start = start_pos, .end = end_pos });
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
fn asyncExpression(self: *Self) Error!Node.Index {
    const async_kw = try self.next();
    assert(async_kw.token.tag == .kw_async);

    const async_line = async_kw.token.line;
    if (self.current.token.tag == .kw_function and async_line == self.current.token.line) {
        _ = try self.nextToken(); // eat 'function keyword'
        return self.functionExpression(async_kw.startByte(), .{ .is_async = true });
    }

    if (self.current.token.tag == .@"(") {
        const argsOrArrowParams = try self.callArgsOrAsyncArrowParams(
            ast.FunctionFlags{
                .is_async = true,
                .is_arrow = true,
            },
        );

        switch (argsOrArrowParams) {
            .arrow_params => |params| return params,
            .call_args => |args| {
                const async_identifier = try self.bindingIdentifier(async_kw.span());
                return self.addNode(.{
                    .call_expr = .{ .callee = async_identifier, .arguments = args.range },
                }, .{ .start = async_kw.startByte(), .end = args.end });
            },
        }
    }

    if (self.isIdentifier(self.current.token.tag) and
        self.current.token.line == async_kw.token.line)
    {
        // async x => ...
        const id_token = try self.next();
        const param = try self.bindingIdentifier(id_token.span());
        const params_range = try self.addSubRange(&[_]Node.Index{param});
        const params = try self.addNode(.{ .parameters = params_range }, spanBetween(async_kw, id_token));

        try self.ensureFatArrow(&id_token.token, &id_token.token);
        self.is_current_arrow_func_async = true;
        return params;
    }

    // Just a reference to some variable/function called "async"
    return self.identifierReference(async_kw.span());
}

const AsyncCallOrArrowParams = union(enum) {
    arrow_params: Node.Index,
    call_args: struct { range: ?ast.SubRange, end: u32 },
};

fn callArgsOrAsyncArrowParams(self: *Self, _: ast.FunctionFlags) Error!AsyncCallOrArrowParams {
    const lparen = try self.next();
    assert(lparen.token.tag == .@"(");

    const scratch_start = self.scratch.items.len;
    defer self.scratch.items.len = scratch_start;

    var destructure_kind = DestructureKind{
        .can_destruct = true,
        .must_destruct = false,
        .can_be_assigned_to = true,
        .is_simple_expression = false,
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
            try self.scratch.append(self.allocator, spread_elem);

            if (self.isAtToken(.@")")) break;
            // spread element must be the last item in a parameter list.
            if (destructure_kind.must_destruct) {
                return self.errorOnNode(spread_elem, ParseError.RestElementNotLast);
            }
            // If 'must_destruct' is not set, but we parsed a spread element
            // without seeing a ')' right after, then we must be inside a call-expression's
            // arguments.
            destructure_kind.setNoAssignOrDestruct();
            spread_elem_in_middle = spread_elem;
        } else {
            const expr = try self.assignmentExpression();
            destructure_kind.update(self.current_destructure_kind);
            try self.scratch.append(self.allocator, expr);
        }

        if (destructure_kind.isMalformed()) {
            // TODO: this error message can be improved based
            // on whether the most recently parsed node was a
            // pattern.
            return self.errorOnNode(
                self.scratch.items[self.scratch.items.len - 1],
                ParseError.InvalidExpressionInsideParens,
            );
        }

        if (self.isAtToken(.@","))
            _ = try self.nextToken()
        else
            break;
    }

    const rparen = try self.expect(.@")");
    const sub_exprs = self.scratch.items[scratch_start..];

    if (self.isAtToken(.@"=>")) {
        if (!destructure_kind.can_destruct) {
            if (spread_elem_in_middle) |node| {
                return self.errorOnNode(
                    node,
                    ParseError.RestElementNotLastInParams,
                );
            }

            // TODO: improve the location of the diagnostic.
            // Which part exactly is invalid?
            try self.emitDiagnostic(
                lparen.token.startCoord(self.source),
                "Invalid arrow function parameters",
                .{},
            );
            return Error.InvalidArrowParameters;
        }

        // mutate the expressions so far to be interpreted as patterns.
        for (sub_exprs) |node| {
            self.reinterpretAsBindingPattern(node);
        }

        const params_range = if (sub_exprs.len > 0)
            try self.addSubRange(sub_exprs)
        else
            null;
        const parameters = try self.addNode(.{ .parameters = params_range }, spanBetween(lparen, rparen));

        try self.ensureFatArrow(&lparen.token, &rparen.token);
        self.is_current_arrow_func_async = true;
        return .{ .arrow_params = parameters };
    }

    if (destructure_kind.must_destruct) {
        // TODO: improve the location of the diagnostic.
        // Which part exaclty forces a destructuring pattern?
        try self.emitDiagnostic(
            lparen.token.startCoord(self.source),
            "function call arguments contain a destructuring pattern",
            .{},
        );
        return Error.UnexpectedPattern;
    }

    const call_args = if (sub_exprs.len > 0)
        try self.addSubRange(sub_exprs)
    else
        null;

    return .{ .call_args = .{ .range = call_args, .end = rparen.endByte() } };
}

/// Save `token` as an identifier reference node.
fn identifierReference(self: *Self, name: ast.Span) Error!Node.Index {
    return self.addNode(.{ .identifier_reference = name }, name);
}

/// Save `token` as an identifier node.
fn identifier(self: *Self, name: ast.Span) Error!Node.Index {
    return self.addNode(.{ .identifier = name }, name);
}

/// Save `token` as an binding identifier on the LHS of a variable declaration
fn bindingIdentifier(self: *Self, name: ast.Span) Error!Node.Index {
    return try self.addNode(.{ .binding_identifier = name }, name);
}

fn makeSuper(
    self: *Self,
    super_token: *const TokenWithId,
) Error!Node.Index {
    assert(super_token.token.tag == .kw_super);
    return self.addNode(.{ .super = super_token.span() }, super_token.span());
}

/// Ensures:
/// 1. That the current token is a '=>'
/// 2. That '=>' is on the same line as the last token in the parameter list.
fn ensureFatArrow(
    self: *Self,
    params_start_token: *const Token,
    params_end_token: *const Token,
) Error!void {
    if (!self.isAtToken(.@"=>")) {
        return self.errorOnToken(params_start_token.*, ParseError.MissingArrow);
    }

    const fat_arrow = self.current.token;
    if (params_end_token.line != fat_arrow.line) {
        return self.errorOnToken(fat_arrow, ParseError.IllegalFatArrow);
    }
}

/// Assuming the parameter list has been consumed, parse the body of
/// an arrow function and return the complete arrow function AST node id.
fn completeArrowFunction(self: *Self, params: Node.Index) Error!Node.Index {
    assert(self.current.token.tag == .@"=>");

    if (self.nodeTag(params) != .parameters) {
        try self.emitDiagnosticOnToken(self.current.token, "Unexpected '=>'", .{});
        return Error.UnexpectedToken;
    }

    _ = try self.nextToken(); // eat '=>'

    const is_async = self.is_current_arrow_func_async;
    self.is_current_arrow_func_async = false;

    const is_strict, const body = blk: {
        const context = self.context;

        defer self.context = context;
        const body_start_token = self.current.token;

        self.context.is_yield_reserved = false; // arrow functions cannot be generators.
        self.context.is_await_reserved = is_async;

        // If an arrow function body starts with a '{',
        // we will attempt to parse it as a block statement, and not an object literal.
        if (body_start_token.tag == .@"{") {
            self.context.@"return" = true;
            break :blk try self.functionBody();
        }

        const assignment = try self.assignmentExpression();
        if (self.current_destructure_kind.must_destruct) {
            return self.errorOnToken(body_start_token, ParseError.MalformedArrowFnBody);
        }

        break :blk .{ false, assignment };
    };

    const nodes = self.nodes.slice();
    const start = nodes.items(.start)[@backingInt(params)];
    const end = nodes.items(.end)[@backingInt(body)];

    // cannot be destructured or assigned to.
    self.current_destructure_kind.reset();

    const fn_meta = ast.FunctionMeta{
        .name = null,
        .flags = .{
            .is_arrow = true,
            .is_generator = false,
            .is_async = is_async,
            .has_strict_directive = is_strict,
        },
    };

    return self.addNode(ast.NodeData{
        .function_expr = ast.Function{
            .parameters = params,
            .body = body,
            .meta = try self.addNode(.{ .function_meta = fn_meta }, .{ .start = start, .end = start }),
        },
    }, .{ .start = start, .end = end });
}

/// Parse a spread element when current_token is '...'
fn spreadElement(self: *Self) Error!Node.Index {
    const dotdotdot = try self.next();
    assert(dotdotdot.token.tag == .@"...");
    const rest_arg = try self.assignExpressionNoPattern();
    const end_pos = self.nodes.items(.end)[@backingInt(rest_arg)];
    return self.addNode(.{ .spread_element = rest_arg }, .{ .start = dotdotdot.startByte(), .end = end_pos });
}

/// Parse a RestElement, assuming we're at the `...` token
fn restElement(self: *Self) Error!Node.Index {
    const dotdotdot = try self.next();
    assert(dotdotdot.token.tag == .@"...");
    const rest_arg = try self.parseLhsExpression();

    if (!self.current_destructure_kind.can_be_assigned_to) {
        return self.errorOnNode(rest_arg, ParseError.InvalidRestOperand);
    }

    const end_pos = self.nodes.items(.end)[@backingInt(rest_arg)];
    return self.addNode(.{ .rest_element = rest_arg }, .{ .start = dotdotdot.startByte(), .end = end_pos });
}

/// Parse a 'BindingRestElement', assuming we're at "...".
/// A "BindingRestElement" is a rest element in a destructuring pattern,
/// like the LHS of a declaration or the parameter of a function or catch block.
///
/// BindingRestElement:
///     ... BindingIdentifier
///     ... BindingPattern
///
/// https://tc39.es/ecma262/#prod-BindingRestElement
fn bindingRestElement(self: *Self) Error!Node.Index {
    const dotdotdot = try self.next();
    assert(dotdotdot.token.tag == .@"...");
    const rest_arg = blk: {
        if (self.isIdentifier(self.current.token.tag))
            break :blk try self.bindingIdentifier((try self.next()).span());
        break :blk try self.bindingPattern();
    };
    const end_pos = self.nodes.items(.end)[@backingInt(rest_arg)];
    return self.addNode(.{ .rest_element = rest_arg }, .{ .start = dotdotdot.startByte(), .end = end_pos });
}

/// Once a '(' token has been eaten, parse the either an arrow function or a parenthesized expression.
fn completeArrowParamsOrGroupingExpr(
    self: *Self,
    lparen: *const Token,
    lparen_start: u32,
) Error!Node.Index {
    const first_expr = try self.assignmentExpression();
    if (!self.current_destructure_kind.can_destruct) {
        const expr = try self.completeSequenceExpr(first_expr);
        const rparen = try self.expect(.@")");
        if (!self.config.preserve_parens) return expr;
        return self.addNode(.{ .parenthesized_expr = expr }, .{ .start = lparen_start, .end = rparen.endByte() });
    }

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    try self.scratch.append(self.allocator, first_expr);

    var has_trailing_comma = false;

    var destructure_kind = self.current_destructure_kind;
    while (self.isAtToken(.@",")) {
        const comma_token = try self.nextToken(); // eat ','
        // A ')' after comma is allowed in arrow function parameters,
        // but not in regular comma-separated expressions.
        if (self.isAtToken(.@")") and destructure_kind.can_destruct) {
            has_trailing_comma = true;
            break;
        }

        // A '...' at this point is either a rest parameter or a syntax error.
        if (self.isAtToken(.@"...")) {
            if (destructure_kind.can_destruct) {
                const rest_elem = try self.bindingRestElement();
                try self.scratch.append(self.allocator, rest_elem);

                if (!self.isAtToken(.@")"))
                    try self.restParamNotLastError(&self.current.token);

                destructure_kind.must_destruct = true;
                break;
            }

            try self.emitBadTokenDiagnostic("expression", &self.current.token);
            return Error.UnexpectedToken;
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
        try self.scratch.append(self.allocator, rhs);
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

        const params = self.scratch.items[prev_scratch_len..];
        for (params) |node| {
            self.reinterpretAsBindingPattern(node);
        }

        const params_range = try self.addSubRange(params);
        const parameters = try self.addNode(.{ .parameters = params_range }, .{ .start = lparen_start, .end = rparen.endByte() });
        try self.ensureFatArrow(lparen, &rparen.token);
        return parameters;
    }

    // Not at a '=>', so we are parsing a grouping expression.
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

    // (Pattern, Pattern) cannot be assigned to or destructured,
    // However, (Identifier) can be assigned to (but not destructured)
    // So, `(a) = 1` is valid, `(a = 1) = 2` is not and neither are `((a)) => 1` and `var (a) = 1`
    const items = self.scratch.items[prev_scratch_len..];
    if (items.len > 1 or self.isValidAssignTargetInParens(items[0])) {
        self.current_destructure_kind.setNoAssignOrDestruct();
    } else {
        self.current_destructure_kind.can_destruct = false;
    }

    if (has_trailing_comma) {
        try self.emitDiagnostic(
            rparen.token.startCoord(self.source),
            "Trailing comma is not permitted in parenthesied expressions",
            .{},
        );
        return Error.UnexpectedToken;
    }

    // A single expression inside parentheses.
    // E.g: (5 * a)
    if (items.len == 1) {
        const expr = items[0];
        if (!self.config.preserve_parens) return expr;
        return self.addNode(.{ .parenthesized_expr = expr }, .{ .start = lparen_start, .end = rparen.endByte() });
    }

    // We are parsing a list of comma separated items inside parenthese.
    // E.g: (a, b, c + 1)
    // range of a sequence_expr = [start_pos(first_expr), end_pos(last_expr)]
    const slice = self.nodes.slice();
    const last_expr = items[items.len - 1];
    const start_pos = slice.items(.start)[@backingInt(first_expr)];
    const end_pos = slice.items(.end)[@backingInt(last_expr)];

    const sub_exprs = try self.addSubRange(items);
    const sequence_expr = try self.addNode(.{ .sequence_expr = sub_exprs }, .{ .start = start_pos, .end = end_pos });
    if (!self.config.preserve_parens) return sequence_expr;
    return self.addNode(.{ .parenthesized_expr = sequence_expr }, .{ .start = lparen_start, .end = rparen.endByte() });
}

/// Parses either an arrow function or a parenthesized expression.
fn groupingExprOrArrowParameters(self: *Self) Error!Node.Index {
    const lparen = try self.next();
    assert(lparen.token.tag == .@"(");

    if (self.isAtToken(.@")")) {
        const rparen = try self.next();
        const params = try self.addNode(.{ .parameters = null }, spanBetween(lparen, rparen));
        try self.ensureFatArrow(&lparen.token, &rparen.token);
        return params;
    }

    if (self.isAtToken(.@"...")) {
        const rest_elem = try self.bindingRestElement();
        const rparen = try self.expect(.@")");
        const params = try self.addSubRange(&[_]Node.Index{rest_elem});
        try self.ensureFatArrow(&lparen.token, &rparen.token);

        const parameters = try self.addNode(.{ .parameters = params }, spanBetween(lparen, rparen));
        return parameters;
    }

    return self.completeArrowParamsOrGroupingExpr(&lparen.token, lparen.startByte());
}

/// Parse an object literal, assuming the `{` has already been consumed.
/// https://262.ecma-international.org/15.0/index.html#prod-ObjectLiteral
fn objectLiteral(self: *Self) Error!Node.Index {
    const lbrace = try self.next();
    assert(lbrace.token.tag == .@"{");

    const properties = try self.propertyDefinitionList();
    const rbrace = try self.expect(.@"}");
    return self.addNode(.{ .object_literal = properties }, spanBetween(lbrace, rbrace));
}

/// https://tc39.es/ecma262/#prod-PropertyDefinitionList
/// Parse a comma-separated list of properties.
/// Returns `null` if there's 0 properties in the object.
fn propertyDefinitionList(self: *Self) Error!?ast.SubRange {
    const scratch_start = self.scratch.items.len;
    defer self.scratch.items.len = scratch_start;

    var destructure_kind = self.current_destructure_kind;
    destructure_kind.is_simple_expression = false;

    while (true) {
        switch (self.current.token.tag) {
            .non_ascii_identifier, .identifier => {
                try self.scratch.append(self.allocator, try self.identifierProperty());
                destructure_kind.update(self.current_destructure_kind);
            },

            .@"[" => {
                _ = try self.nextToken();
                // The destructuring kind of the expression inside "[]" doesn't
                // matter. Only "value" (after ':') determines whether
                // this pattern is a valid object pattern for destructuring/assignment.
                const prev_destruct_kind = self.current_destructure_kind;
                const key = try self.assignExpressionNoPattern();
                self.current_destructure_kind = prev_destruct_kind;

                _ = try self.expectToken(.@"]");

                const property = try self.completePropertyDef(
                    key,
                    .{ .is_computed = true },
                );
                destructure_kind.update(self.current_destructure_kind);
                try self.scratch.append(self.allocator, property);
            },

            .decimal_literal,
            .octal_literal,
            .hex_literal,
            .binary_literal,
            .legacy_octal_literal,
            .string_literal,
            => {
                const key_token = try self.next();

                // Even though a literal like 5 or "foo" is not a valid destructuring
                // target (e.g: 5=1 is invalid), they can still be used as keys in
                // object patterns (e.g: `({5: x} = 1)` is valid).
                // The call to `parseLiteral()` for parsing the key will update
                // the parser's "current destructure kind" to `.assign = false`
                // and `can_destruct = false`.
                // But we undo that effect by restoring the previous destructure kind
                // after parsing the key.
                const key = try self.parseLiteral(key_token);

                const property_expr = try self.completePropertyDef(key, .{});
                destructure_kind.update(self.current_destructure_kind);
                try self.scratch.append(self.allocator, property_expr);
            },

            // generator method
            .@"*" => {
                _ = try self.nextToken(); // eat '*'
                const key = try self.classElementName();
                const generator_method = try self.parseMethodBody(
                    key,
                    .{ .is_method = true },
                    .{ .is_generator = true },
                );
                destructure_kind.can_destruct = false;
                try self.scratch.append(self.allocator, generator_method);
            },

            .@"..." => {
                const ellipsis_tok = try self.next();
                const expr = try self.assignmentExpression();

                destructure_kind.update(self.current_destructure_kind);

                const start = ellipsis_tok.startByte();
                const end = self.nodes.items(.end)[@backingInt(expr)];
                try self.scratch.append(self.allocator, try self.addNode(.{ .spread_element = expr }, .{ .start = start, .end = end }));

                if (self.isAtToken(.@",")) {
                    // comma is not allowed after rest element in object patterns
                    destructure_kind.setNoAssignOrDestruct();
                }
            },
            else => {
                if (self.current.token.tag.isKeyword()) {
                    const id_property = try self.identifierProperty();
                    try self.scratch.append(self.allocator, id_property);
                    destructure_kind.update(self.current_destructure_kind);
                } else {
                    break;
                }
            },
        }

        const maybe_comma = self.peek();
        if (maybe_comma.tag == .@",") {
            _ = try self.nextToken();
        } else {
            break;
        }
    }

    if (destructure_kind.isMalformed()) {
        // TODO: emit a diagnostic.
        return Error.InvalidObject;
    }

    self.current_destructure_kind = destructure_kind;

    const property_defs = self.scratch.items[scratch_start..];
    if (property_defs.len == 0) return null;
    return try self.addSubRange(property_defs);
}

/// Parse an the property of an object literal or object pattern that starts with an identifier.
fn identifierProperty(self: *Self) Error!Node.Index {
    const key_token = try self.next();
    assert(key_token.token.tag.isIdentifier() or
        key_token.token.tag.isKeyword());

    const cur_token = self.current.token;
    if (cur_token.tag != .@":" and cur_token.tag != .@"(" and
        cur_token.tag != .@"," and cur_token.tag != .@"}" and
        cur_token.tag != .@"=")
    {
        if (key_token.token.tag == .kw_async) {
            // handle `async f() { ... }` and `async *f() { ... }`

            const is_generator = self.isAtToken(.@"*");
            if (is_generator) _ = try self.nextToken(); // eat '*'

            const property_key_token = try self.expectIdOrKeyword();
            const property_key = try self.identifier(property_key_token.span());
            const property_val = try self.parseMethodBody(
                property_key,
                .{ .is_method = true },
                .{ .is_async = true, .is_generator = is_generator },
            );

            const end_pos = self.nodes.items(.end)[@backingInt(property_val)];

            // object literals with async methods are not valid object patterns.
            self.current_destructure_kind.setNoAssignOrDestruct();
            return self.addNode(.{
                .object_property = .{
                    .key = property_key,
                    .value = property_val,
                },
            }, .{ .start = key_token.startByte(), .end = end_pos });
        }

        const maybe_getter_or_setter = try self.getterOrSetter(key_token.token);
        if (maybe_getter_or_setter) |getter_or_setter| {
            // object literals with getters or setters are not valid object patterns.
            self.current_destructure_kind.setNoAssignOrDestruct();
            return getter_or_setter;
        }

        try self.emitDiagnostic(
            self.current.token.startCoord(self.source),
            "Unexpected '{s}' in property definition",
            .{self.current.token.toByteSlice(self.source)},
        );
        return Error.UnexpectedToken;
    }

    const cur_token_tag = self.current.token.tag;
    switch (cur_token_tag) {
        .@":", .@"(" => {
            const key = try self.identifier(key_token.span());
            return self.completePropertyDef(key, .{
                .is_method = cur_token_tag == .@"(",
            });
        },

        .@"=" => {
            const key = try self.identifier(key_token.span());
            const op_token = try self.next(); // eat '='
            if (!key_token.token.tag.isValidPropertyName()) {
                try self.emitBadTokenDiagnostic("property name", &key_token.token);
                return Error.UnexpectedToken;
            }

            const value = try self.assignmentExpression();
            const nodes = self.nodes.slice();
            const start_pos = nodes.items(.start)[@backingInt(key)];
            const end_pos = nodes.items(.end)[@backingInt(value)];

            const assignment_pattern = ast.NodeData{
                .assignment_pattern = .{
                    .lhs = key,
                    .rhs = value,
                    .operator = ast.Operator.fromTag(op_token.token.tag),
                },
            };

            const property = ast.PropertyDefinition{
                .value = try self.addNode(assignment_pattern, .{ .start = start_pos, .end = end_pos }),
                .key = key,
                .flags = .{ .kind = .init },
            };

            // 'Identifier = AssignmentExpression' is allowed in object patterns but not in object literals
            // TODO: use a helper method here instead of overriding the current destructure kind.
            self.current_destructure_kind = DestructureKind.MustDestruct;
            return self.addNode(.{ .object_property = property }, .{ .start = start_pos, .end = end_pos });
        },

        else => {
            if (!self.isIdentifier(key_token.token.tag)) {
                return self.errorOnToken(key_token.token, ParseError.InvalidKeywordShorthandProperty);
            }

            // { a } <- 'a' is a *reference* to some variable.
            const key = try self.identifierReference(key_token.span());
            const kv_node = ast.ShorthandProperty{ .name = key };

            // { k }
            //   ^-- Is a valid property in a destructuring pattern
            self.current_destructure_kind.can_destruct = true;
            self.current_destructure_kind.can_be_assigned_to = true;
            return self.addNode(.{ .shorthand_property = kv_node }, spanBetween(key_token, key_token));
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
/// ```js
/// class Foo {
///     foo = 1; // .identifier
///     if = 2; // .kw_if
///     [x] = 3; // .@"["
/// }
/// ```
fn canStartClassElementName(token: *const Token) bool {
    return token.tag.isValidPropertyName() or
        token.tag == .@"[" or
        token.tag == .private_identifier;
}

/// https://tc39.es/ecma262/#prod-ClassElementName
fn classElementName(self: *Self) Error!Node.Index {
    const token = try self.next();
    switch (token.token.tag) {
        .non_ascii_identifier,
        .identifier,
        .private_identifier,
        => return self.identifier(token.span()),
        .@"[" => {
            const expr = try self.assignmentExpression();
            _ = try self.expectToken(.@"]");
            return expr;
        },
        // TODO: use the literal mask
        .string_literal,
        .decimal_literal,
        .octal_literal,
        .hex_literal,
        .binary_literal,
        .legacy_octal_literal,
        => return try self.parseLiteral(token),
        else => {
            if (token.token.tag.isKeyword())
                return self.identifier(token.span());

            try self.emitDiagnostic(
                token.token.startCoord(self.source),
                "Expected property name, got '{s}'",
                .{token.token.toByteSlice(self.source)},
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
    const func_expr = try self.parseFunctionBody(
        self.current.startByte(),
        null,
        fn_flags,
        false,
    );

    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = func_expr,
        .flags = flags,
    };

    if (flags.kind == .get or flags.kind == .set) {
        // verify the number of parameters for getters and setters.
        try self.checkGetterOrSetterParams(func_expr, flags.kind);
    }

    const nodes = self.nodes.slice();
    const key_start = nodes.items(.start)[@backingInt(key)];
    const end_pos = nodes.items(.end)[@backingInt(func_expr)];
    return self.addNode(.{ .object_property = kv_node }, .{ .start = key_start, .end = end_pos });
}

/// Verify the number of parameters in a getter or setter.
/// Emit a diagnostic, then return an error if the number is invalid.
/// A Getter must have exaclty 0 parameters, and a setter only one.
/// Additionally, a setter cannot have rest parameters.
fn checkGetterOrSetterParams(
    self: *Self,
    func_expr: Node.Index,
    kind: ast.PropertyDefinitionKind,
) Error!void {
    const func = &self.getNode(func_expr).data.function_expr;
    // SAFETY: self.tree is always valid here. It can only be null once `parse` (called before this)
    // has already returned.
    const n_params = func.getParameterCount(self.tree orelse unreachable);

    if (kind == .get and n_params != 0) {
        // TODO: emit error and keep parsing
        return self.errorOnNode(func_expr, ParseError.InvalidGetter);
    }

    if (kind == .set) {
        if (n_params != 1) {
            return self.errorOnNode(func_expr, ParseError.InvalidSetter);
        }

        // SAFETY: `self.tree` cannot be null here.
        const param = func.getParameterSlice(self.tree orelse unreachable)[0];
        if (self.nodeTag(param) == .rest_element) {
            return self.errorOnNode(param, ParseError.InvalidSetterRestParam);
        }
    }
}

/// Assuming that the key has been parsed, complete the property definition.
fn completePropertyDef(
    self: *Self,
    key: Node.Index,
    flags: ast.PropertyDefinitionFlags,
) Error!Node.Index {
    if (self.current.token.tag == .@"(") {
        self.current_destructure_kind.setNoAssignOrDestruct();
        return self.parseMethodBody(key, .{
            .is_method = true,
            .is_computed = flags.is_computed,
            .kind = flags.kind,
        }, .{});
    }

    _ = try self.expectToken(.@":");

    const value = try self.assignmentExpression();
    // the assignmentExpression() will have updated self.current_destructure_kind

    const nodes = self.nodes.slice();
    const start_pos = nodes.items(.start)[@backingInt(key)];
    const end_pos = nodes.items(.end)[@backingInt(value)];
    const kv_node = ast.PropertyDefinition{
        .key = key,
        .value = value,
        .flags = flags,
    };
    return self.addNode(.{ .object_property = kv_node }, .{ .start = start_pos, .end = end_pos });
}

/// Parse an ArrayLiteral:
/// https://262.ecma-international.org/15.0/index.html#prod-ArrayLiteral
fn arrayLiteral(self: *Self) Error!Node.Index {
    const open_brac = try self.next();
    assert(open_brac.token.tag == .@"[");

    const scratch_start = self.scratch.items.len;
    defer self.scratch.items.len = scratch_start;

    var destructure_kind = self.current_destructure_kind;
    destructure_kind.is_simple_expression = false;

    const start_pos = open_brac.startByte();
    var end_pos = start_pos;
    while (true) {
        while (self.isAtToken(.@",")) {
            // elision: https://262.ecma-international.org/15.0/index.html#prod-Elision
            const comma = try self.next();
            try self.scratch.append(self.allocator, try self.addNode(.{ .empty_array_item = {} }, spanBetween(comma, comma)));
        }

        if (self.isAtToken(.@"]")) {
            end_pos = (try self.next()).endByte();
            break;
        }

        switch (self.peek().tag) {
            // Spread element
            .@"..." => {
                const ellipsis = try self.next();
                const expr = try self.assignmentExpression();
                const start = ellipsis.startByte();
                const end = self.nodes.items(.end)[@backingInt(expr)];

                destructure_kind.update(self.current_destructure_kind);
                if (self.isAtToken(.@",")) {
                    // "," is not allowed after rest element in array patterns
                    destructure_kind.setNoAssignOrDestruct();
                }

                const spread_element = try self.addNode(.{ .spread_element = expr }, .{ .start = start, .end = end });
                try self.scratch.append(self.allocator, spread_element);
            },

            else => {
                const item = try self.assignmentExpression();
                destructure_kind.update(self.current_destructure_kind);
                try self.scratch.append(self.allocator, item);
            },
        }

        const next_token = try self.expect2(.@",", .@"]");
        if (next_token.token.tag == .@"]") {
            end_pos = next_token.endByte();
            break;
        }
    }

    const elements = self.scratch.items[scratch_start..];
    const nodes = try self.addSubRange(elements);
    self.current_destructure_kind = destructure_kind;
    return self.addNode(.{ .array_literal = nodes }, .{ .start = start_pos, .end = end_pos });
}

/// Parse a function expression assuming the 'function' keyword
/// has been consumed before this is called.
///
/// [start_token_id] Must be the ID of an 'async' or 'function' keyword
fn functionExpression(
    self: *Self,
    start_byte: u32,
    flags: ast.FunctionFlags,
) Error!Node.Index {
    var fn_flags = flags;
    if (self.isAtToken(.@"*")) {
        _ = try self.nextToken(); // eat '*'
        fn_flags.is_generator = true;
    }

    const saved_ctx = self.context;
    // 'await' and 'yield' are always allowed as
    // function expression names, except if the function is a generator
    self.context.is_await_reserved = false;
    self.context.is_yield_reserved = fn_flags.is_generator;

    const name_token: ?Node.Index =
        if (self.current.token.tag.isIdentifier() or
        self.isKeywordIdentifier(self.current.token.tag))
            try self.functionName()
        else
            null;

    self.context = saved_ctx;

    defer self.current_destructure_kind = DestructureKind.CannotDestruct;
    return self.parseFunctionBody(start_byte, name_token, fn_flags, false);
}

/// parses the arguments and body of a function expression (or declaration),
/// assuming the `function` keyword (and/or the function/method name) has been consumed.
fn parseFunctionBody(
    self: *Self,
    start_pos: u32,
    func_name: ?Node.Index,
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

    const parameters = try self.parseFormalParameters();

    // Allow return statements inside function
    const ctx = self.context;
    defer self.context = ctx;
    self.context.@"return" = true;

    const is_strict, const body = try self.functionBody();
    const end_pos = self.nodeSpan(body).end;

    var fn_flags = flags;
    fn_flags.has_strict_directive = is_strict;

    const meta_span = if (func_name) |name|
        self.nodeData(name).binding_identifier
    else
        ast.Span{ .start = start_pos, .end = start_pos };
    const function_meta = try self.addNode(.{
        .function_meta = ast.FunctionMeta{
            .name = func_name,
            .flags = fn_flags,
        },
    }, meta_span);

    const function = ast.Function{
        .parameters = parameters,
        .body = body,
        .meta = function_meta,
    };

    const node_data: ast.NodeData = if (is_decl)
        .{ .function_declaration = function }
    else
        .{ .function_expr = function };
    return self.addNode(node_data, .{ .start = start_pos, .end = end_pos });
}

/// Parse a list of statements surrounded by '{}'.
/// Does not introduce a new scope.
/// Returns a (bool, Node.Index) pair:
///     1. A boolean indicating whether the body has a 'use strict' directive
///     2. The index of the block statement node.
fn functionBody(self: *Self) Error!struct { bool, Node.Index } {
    const start_pos = (try self.expect(.@"{")).startByte();

    const prev_scratch_len = self.scratch.items.len;
    defer self.scratch.items.len = prev_scratch_len;

    // check all directives and see if we enter strict mode somewhere
    const is_strict = blk: {
        while (!self.isAtToken(.@"}")) {
            const stmt = try self.statementOrDeclaration();
            try self.scratch.append(self.allocator, stmt);

            switch (self.checkDirective(stmt)) {
                .use_strict => break :blk true,
                .directive => {},
                .not_directive => break :blk false,
            }
        }

        break :blk false;
    };

    const ctx = self.context;
    defer self.context = ctx;
    if (is_strict) {
        self.context.strict = true;
    }

    while (self.current.token.tag != .@"}") {
        const stmt = try self.statementOrDeclaration();
        try self.scratch.append(self.allocator, stmt);
    }

    const rbrace = try self.next();
    assert(rbrace.token.tag == .@"}");
    const end_pos = rbrace.endByte();

    const statements = self.scratch.items[prev_scratch_len..];
    const stmt_list_node = try self.addSubRange(statements);
    const block_node = ast.NodeData{ .statement_list = stmt_list_node };
    return .{ is_strict, try self.addNode(block_node, .{ .start = start_pos, .end = end_pos }) };
}

/// Encodes the kind of directive present at the beginning of a function
const DirectiveKind = enum {
    /// A "use strict" directive
    use_strict,
    /// Some unrecognized directive (or a string literal)
    directive,
    /// Some other statement
    not_directive,
};

/// Check if a given statement is a directive.
/// If so, return whether its a "use strict" directive or not.
fn checkDirective(self: *const Self, stmt: Node.Index) DirectiveKind {
    const nodes = self.nodes.slice();
    const data: []ast.NodeData = nodes.items(.data);

    if (meta.activeTag(data[@backingInt(stmt)]) != .expression_statement)
        return .not_directive;

    const expr_id: usize = @backingInt(
        data[@backingInt(stmt)].expression_statement,
    );

    const expr = data[expr_id];
    if (meta.activeTag(expr) != .string_literal)
        return .not_directive;

    const literal_span = expr.string_literal;
    const string_val = literal_span.slice(self.source);
    if (std.mem.eql(u8, string_val[1 .. string_val.len - 1], "use strict"))
        return .use_strict;
    return .directive;
}

fn parseParameter(self: *Self) Error!Node.Index {
    const param = try self.bindingElement();
    if (!self.isAtToken(.@"=")) return param;

    const eq_token = try self.next(); // eat '='

    const defaultValue = try self.assignExpressionNoPattern();
    const node_starts: []u32 = self.nodes.items(.start);

    const assignment_pattern: ast.BinaryPayload = .{
        .lhs = param,
        .rhs = defaultValue,
        .operator = ast.Operator.fromTag(eq_token.token.tag),
    };

    return self.addNode(.{ .assignment_pattern = assignment_pattern }, .{ .start = node_starts[@backingInt(param)], .end = node_starts[@backingInt(defaultValue)] });
}

/// Starting with the '(' token , parse formal parameters of a function.
fn parseFormalParameters(self: *Self) Error!Node.Index {
    const context = self.context;
    defer self.context = context;

    const start_pos = (try self.expect(.@"(")).startByte();

    const scratch_start = self.scratch.items.len;
    defer self.scratch.items.len = scratch_start;

    const rparen_end = blk: {
        if (self.isAtToken(.@")"))
            break :blk (try self.next()).endByte();

        while (true) {
            if (self.isAtToken(.@"...")) {
                const rest_elem = try self.bindingRestElement();
                try self.scratch.append(self.allocator, rest_elem);

                if (!self.isAtToken(.@")"))
                    try self.restParamNotLastError(&self.current.token);

                break :blk (try self.next()).endByte();
            }

            const param = try self.parseParameter();
            try self.scratch.append(self.allocator, param);

            const comma_or_rpar = try self.expect2(.@",", .@")");
            // After a ",", we expect either a ")" or another parameter
            if (comma_or_rpar.token.tag == .@")") {
                break :blk comma_or_rpar.endByte();
            } else if (self.isAtToken(.@")")) {
                const rparen = try self.next();
                break :blk rparen.endByte();
            }
        }
    };

    const end_pos = rparen_end;
    const params = self.scratch.items[scratch_start..];
    const param_list = if (params.len > 0)
        try self.addSubRange(params)
    else
        null;

    self.current_destructure_kind.reset();
    return self.addNode(.{ .parameters = param_list }, .{ .start = start_pos, .end = end_pos });
}

/// Get a pointer to a node by its index.
/// The returned value can be invalidated by any call to `addNode`, `restoreState`, `addSubRange`.
pub fn getNode(self: *const Self, index: Node.Index) ast.Node {
    return self.nodes.get(@backingInt(index));
}

pub fn getExtraData(
    self: *const Self,
    index: ast.ExtraData.Index,
) *const ast.ExtraData {
    return &self.extra_data.items[@backingInt(index)];
}

/// From a `SubRange` object, get a slice that contains the ID for each node
/// in that list.
pub fn getSubRange(
    self: *const Self,
    from: ast.SubRange.Index,
    to: ast.SubRange.Index,
) []const Node.Index {
    const from_: usize = @backingInt(from);
    const to_: usize = @backingInt(to);
    return self.node_lists.items[from_..to_];
}

pub fn getToken(self: *const Self, index: Token.Index) Token {
    return self.tokens.items[@backingInt(index)];
}

/// Get the start and end byte offset of a node in the source file.
fn nodeSpan(self: *const Self, index: Node.Index) ast.Span {
    const nodes = self.nodes.slice();
    const start = nodes.items(.start)[@backingInt(index)];
    const end = nodes.items(.end)[@backingInt(index)];
    return .{ .start = start, .end = end };
}

/// Get the start byte offset of a node in the source file.
fn nodeStartCoord(self: *const Self, index: Node.Index) types.Coordinate {
    const start = self.nodes.items(.start)[@backingInt(index)];
    return util.offsets.byteIndexToCoordinate(self.source, self.getToken(start).start);
}

/// Return the tag that identifies the type of a node.
fn nodeTag(self: *const Self, index: Node.Index) std.meta.Tag(NodeData) {
    return meta.activeTag(self.nodes.items(.data)[@backingInt(index)]);
}

fn nodeData(self: *const Self, index: Node.Index) *const NodeData {
    return &self.nodes.items(.data)[@backingInt(index)];
}

/// Parses arguments for a function call, assuming the current_token is '('
/// Parse arguments for a function call, then return it alongside the start and end locations.
fn parseArgs(self: *Self) Error!struct { ast.SubRange, u32, u32 } {

    // The "+In" grammar attribute.
    // This is to allow things like `for (f(a in b) in c)`
    const old_ctx = self.context;
    self.context.in = true;
    defer self.context = old_ctx;

    const start_pos = (try self.expect(.@"(")).startByte();

    const scratch_start = self.scratch.items.len;
    defer self.scratch.items.len = scratch_start;

    while (!self.isAtToken(.@")")) {
        if (self.isAtToken(.@"...")) {
            const spread_elem = try self.spreadElement();
            try self.scratch.append(self.allocator, spread_elem);
        } else {
            const expr = try self.assignExpressionNoPattern();
            try self.scratch.append(self.allocator, expr);
        }
        if (!self.isAtToken(.@","))
            break;
        _ = try self.nextToken(); // eat ','
    }

    const end_pos = (try self.expect(.@")")).endByte(); // eat closing ')'
    const arg_list = self.scratch.items[scratch_start..];
    return .{ try self.addSubRange(arg_list), start_pos, end_pos };
}

// -----
// Tests
// -----

const t = std.testing;

const estree = @import("./estree.zig");

fn runTestOnFile(tests_dir: std.Io.Dir, file_path: []const u8) !void {
    const source_code = try tests_dir.readFileAlloc(
        std.testing.io_instance.io(),
        file_path,
        t.allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    );
    defer t.allocator.free(source_code);

    var parser = try Self.init(t.allocator, source_code, .{ .source_type = .script });
    defer parser.deinit();

    var result = parser.parse() catch |err| {
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

    defer result.deinit();

    // 1. prettify the AST as a JSON string
    const ast_json = try estree.toJsonString(t.allocator, result.tree, .{});
    defer t.allocator.free(ast_json);

    // 2. For every `<filename>.js`, read the corresponding `<filename>.json` file
    const json_file_path = try std.mem.concat(
        t.allocator,
        u8,
        &.{ file_path[0 .. file_path.len - 3], ".json" },
    );
    defer t.allocator.free(json_file_path);

    const expected_ast_json = tests_dir.readFileAlloc(
        std.testing.io_instance.io(),
        json_file_path,
        t.allocator,
        std.Io.Limit.limited(std.math.maxInt(u32)),
    ) catch |err| {
        std.debug.print("could not read file: {s}\n", .{json_file_path});
        return err;
    };
    defer t.allocator.free(expected_ast_json);

    const trimmed_ast_json = std.mem.trim(u8, expected_ast_json, "\n\t ");

    // 3. ensure the AST JSON is equal to the expected JSON
    try t.expectEqualStrings(trimmed_ast_json, ast_json);
}

test parse {
    var root_dir = std.Io.Dir.cwd();

    const tests_dir_path = try std.fs.path.join(t.allocator, &.{ "src", "js", "test-files", "parser" });
    defer t.allocator.free(tests_dir_path);

    var tests_dir = try root_dir.openDir(std.testing.io_instance.io(), tests_dir_path, .{ .iterate = true });
    defer tests_dir.close(std.testing.io_instance.io());

    var iter = tests_dir.iterate();
    while (try iter.next(std.testing.io_instance.io())) |entry| {
        if (entry.kind != .directory) {
            continue;
        }

        var dir = try tests_dir.openDir(std.testing.io_instance.io(), entry.name, .{ .iterate = true });
        defer dir.close(std.testing.io_instance.io());

        var d_iter = dir.iterate();
        while (try d_iter.next(std.testing.io_instance.io())) |test_case_entry| {
            assert(test_case_entry.kind == .file);
            if (!std.mem.endsWith(u8, test_case_entry.name, ".js")) continue;
            runTestOnFile(dir, test_case_entry.name) catch |err| {
                std.log.err("Error comparing ASTs for file: {s}\n", .{test_case_entry.name});
                return err;
            };
        }
    }
}
