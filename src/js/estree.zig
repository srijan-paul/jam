// Contains helpers that convert a Jam AST to
// ESTree compatible JSON.

const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");
const util = @import("util");

const meta = std.meta;
const assert = std.debug.assert;

const Node = ast.Node;
const Tree = ast.Tree;

const JValue = std.json.Value;
const JNull = JValue{ .null = {} };
const JTrue = JValue{ .bool = true };
const JFalse = JValue{ .bool = false };

// TODO: add `loc` with source locations as well.
pub const EstreeJsonOpts = struct {
    start_end_locs: bool = true,
    source_type: bool = false,
    /// Always set to `null`. Only used for babel compatibility
    interpreter: bool = false,
    /// emits an "optional": false field to Call/Member expressions
    /// that are not optional (i.e no .? or .?()).
    optional_chain_flag: bool = false,
    babel_estree_compat: bool = false,
};

/// Options to use for ESTree conversion when the output has to be
/// compatible with `@babel/parser`.
pub const BabelEstreeOptions = EstreeJsonOpts{
    .interpreter = true,
    .source_type = true,
    .optional_chain_flag = true,
    .babel_estree_compat = true,
};

fn subRangeToJsonArray(
    al: std.mem.Allocator,
    t: *const Tree,
    maybe_args: ?ast.SubRange,
    opts: EstreeJsonOpts,
) !JValue {
    var arr = std.json.Array.init(al);
    if (maybe_args) |arguments| {
        const from: usize = @backingInt(arguments.from);
        const to: usize = @backingInt(arguments.to);
        for (from..to) |i| {
            const arg_node = t.node_indices.items[i];
            const new_arg = try nodeToEsTree(t, al, arg_node, opts);
            try arr.append(new_arg);
        }
    }
    return JValue{ .array = arr };
}

fn processEscapes(allocator: std.mem.Allocator, str: []const u8) error{
    OutOfMemory,
    InvalidCodePoint,
    BadEscapeSequence,
}![]const u8 {
    var iter = std.unicode.Utf8Iterator{ .bytes = str, .i = 0 };
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    while (iter.i < str.len) {
        if (str[iter.i] == '\\') {
            if (iter.i + 1 >= str.len) return error.BadEscapeSequence;

            const maybe_parsed_cp = if (str[iter.i + 1] == 'u')
                util.utf8.parseUnicodeEscape(str[iter.i..])
            else
                util.utf8.parseOctalEscape(str[iter.i..]);

            const parsed_cp = maybe_parsed_cp orelse
                return error.InvalidCodePoint;
            iter.i += parsed_cp.len;

            var cp_slice: [4]u8 = undefined;
            const cp_len = std.unicode.utf8Encode(parsed_cp.codepoint, &cp_slice) catch
                return error.InvalidCodePoint;
            try buf.appendSlice(allocator, cp_slice[0..cp_len]);

            continue;
        }

        const cp_slice = iter.nextCodepointSlice() orelse
            unreachable; // already validated UTF-8 during tokenization
        try buf.appendSlice(allocator, cp_slice);
    }

    return buf.toOwnedSlice(allocator);
}

pub fn jamToEstreeTag(node: ast.NodeData) []const u8 {
    return switch (node) {
        .program => "Program",
        .binary_expr => "BinaryExpression",
        .assignment_expr => "AssignmentExpression",
        .member_expr => "MemberExpression",
        .computed_member_expr => "MemberExpression",
        .tagged_template_expr => "TaggedTemplateExpression",
        .meta_property => "MetaProperty",
        .new_expr => "NewExpression",
        .call_expr => "CallExpression",
        .super_call_expr => "CallExpression",
        .super => "Super",
        .optional_expr => "OptionalExpression",
        .function_expr => "FunctionExpression",
        .post_unary_expr => "UpdateExpression",
        .unary_expr => "UnaryExpression",
        .await_expr => "AwaitExpression",
        .yield_expr => "YieldExpression",
        .update_expr => "UpdateExpression",

        .binding_identifier,
        .identifier,
        .identifier_reference,
        => "Identifier",

        .jsx_identifier_reference,
        .jsx_identifier,
        => "JSXIdentifier",

        .string_literal,
        .number_literal,
        .null_literal,
        .boolean_literal,
        .regex_literal,
        => "Literal",

        .this => "ThisExpression",
        .empty_array_item => "EmptyArrayItem",
        .array_literal => "ArrayExpression",
        .array_pattern => "ArrayPattern",
        .spread_element => "SpreadElement",
        .rest_element => "RestElement",
        .object_literal => "ObjectExpression",
        .object_property => "Property",
        .shorthand_property => "Property",
        .class_expression => "ClassExpression",
        .class_field => "ClassProperty",
        .class_method => "MethodDefinition",
        .sequence_expr => "SequenceExpression",
        .conditional_expr => "ConditionalExpression",
        .template_literal => "TemplateLiteral",
        .template_element => "TemplateElement",
        .assignment_pattern => "AssignmentPattern",
        .object_pattern => "ObjectPattern",
        .empty_statement => "EmptyStatement",
        .labeled_statement => "LabeledStatement",
        .try_statement => "TryStatement",
        .catch_clause => "CatchClause",
        .block_statement,
        .statement_list,
        => "BlockStatement",
        .expression_statement => "ExpressionStatement",
        .variable_declaration => "VariableDeclaration",
        .variable_declarator => "VariableDeclarator",
        .function_declaration => "FunctionDeclaration",
        .class_declaration => "ClassDeclaration",
        .debugger_statement => "DebuggerStatement",
        .if_statement => "IfStatement",
        .do_while_statement => "DoWhileStatement",
        .while_statement => "WhileStatement",
        .with_statement => "WithStatement",
        .throw_statement => "ThrowStatement",
        .for_statement => "ForStatement",
        .for_of_statement => "ForOfStatement",
        .for_in_statement => "ForInStatement",
        .switch_statement => "SwitchStatement",
        .switch_case => "SwitchCase",
        .default_case => "SwitchCase",
        .break_statement => "BreakStatement",
        .continue_statement => "ContinueStatement",
        .for_in_of_iterator, .for_iterator, .parameters => unreachable,
        .return_statement => "ReturnStatement",
        .import_declaration => "ImportDeclaration",
        .import_default_specifier => "ImportDefaultSpecifier",
        .import_specifier => "ImportSpecifier",
        .import_namespace_specifier => "ImportNamespaceSpecifier",
        .export_declaration => "ExportNamedDeclaration",
        .export_specifier => "ExportSpecifier",
        .export_list_declaration => "ExportNamedDeclaration",
        .export_from_declaration => "ExportFromDeclaration",
        .export_all_declaration => "ExportAllDeclaration",
        .parenthesized_expr => "GroupingExpression",

        .jsx_text => "JSXText",
        .jsx_spread_child => "JSXSpreadChild",
        .jsx_expression => "JSXExpressionContainer",
        .jsx_fragment => "JSXFragment",
        .jsx_namespaced_name => "JSXNamespacedName",
        .jsx_attribute => "JSXAttribute",
        .jsx_spread_attribute => "JSXSpreadAttribute",
        .jsx_opening_element => "JSXOpeningElement",
        .jsx_self_closing_element => "JSXSelfClosingElement",
        .jsx_closing_element => "JSXClosingElement",
        .jsx_element => "JSXElement",
        .jsx_member_expression => "JSXMemberExpression",
        .function_meta, .class_meta, .jsx_children, .none => unreachable,
    };
}

fn nodeToEsTree(
    t: *const Tree,
    al: std.mem.Allocator,
    node_id: Node.Index,
    opts: EstreeJsonOpts,
) error{
    InvalidCodePoint,
    OutOfMemory,
    BadEscapeSequence,
}!JValue {
    const node = t.nodes.get(@backingInt(node_id));
    switch (node.data) {
        .none, .empty_array_item => return JValue{ .null = {} },
        .parenthesized_expr => |e| return nodeToEsTree(t, al, e, opts),
        else => {},
    }

    var o = std.json.ObjectMap.empty;
    const tag = jamToEstreeTag(node.data);
    try o.put(al, "type", JValue{ .string = tag });
    if (opts.start_end_locs) {
        if (node.data == .program) {
            // The Program node spans the entire source, including leading and
            // trailing whitespace and comments (matches @babel/parser).
            try o.put(al, "start", JValue{ .integer = 0 });
            try o.put(al, "end", JValue{ .integer = @intCast(t.source.len) });
        } else {
            try o.put(al, "start", JValue{ .integer = node.start });
            try o.put(al, "end", JValue{ .integer = node.end });
        }
    }

    switch (node.data) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try nodeToEsTree(t, al, payload.lhs, opts);
            const rhs = try nodeToEsTree(t, al, payload.rhs, opts);
            const operator = payload.operator.lexeme();

            try o.put(al, "operator", JValue{ .string = operator });
            try o.put(al, "left", lhs);
            try o.put(al, "right", rhs);
        },

        .binding_identifier,
        .identifier,
        .identifier_reference,
        .jsx_identifier,
        .jsx_identifier_reference,
        => |tok_id| {
            const name = tok_id.slice(t.source);
            const escapedName = try processEscapes(al, name);
            try o.put(al, "name", JValue{ .string = escapedName });
        },

        .program => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements, opts);
            if (opts.source_type) try o.put(al, "sourceType", JValue{ .string = @tagName(t.source_type) });
            if (opts.interpreter) try o.put(al, "interpreter", JValue{ .null = {} });
            try o.put(al, "body", body);
        },

        .expression_statement => |e| {
            const expression = try nodeToEsTree(t, al, e, opts);
            try o.put(al, "expression", expression);
        },

        .empty_array_item, .this, .super => {},
        .array_literal,
        .array_pattern,
        => |maybe_elements| {
            const elements = try subRangeToJsonArray(al, t, maybe_elements, opts);
            try o.put(al, "elements", elements);
        },

        .string_literal => |token_span| {
            const raw = token_span.slice(t.source);
            try o.put(al, "value", JValue{
                .string = try processEscapes(al, raw[1 .. raw.len - 1]),
            });
            try o.put(al, "raw", JValue{ .string = raw });
        },

        .null_literal => |token_span| {
            const raw = token_span.slice(t.source);
            try o.put(al, "value", JNull);
            try o.put(al, "raw", JValue{ .string = raw });
        },

        .boolean_literal => |b| {
            const raw = b.token.slice(t.source);
            try o.put(al, "value", JValue{ .bool = b.value });
            try o.put(al, "raw", JValue{ .string = raw });
        },

        .number_literal => |num| {
            const raw = num.token.slice(t.source);
            const value = num.value(t);
            if (value == @trunc(value))
                try o.put(al, "value", JValue{ .number_string = raw })
            else
                try o.put(al, "value", JValue{ .float = num.value(t) });

            try o.put(al, "raw", JValue{ .string = raw });
        },

        .regex_literal => |token_span| {
            const raw = token_span.slice(t.source);
            try o.put(al, "raw", JValue{ .string = raw });
            // In babel's ESTree output, a regex literal's value is an empty object
            // when JSON serialized
            try o.put(al, "value", JValue{ .object = std.json.ObjectMap.empty });
        },

        .export_from_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            assert(meta.activeTag(source) == .object);

            try o.put(al, "source", source);
            if (payload.specifiers.asSlice(t).len > 0) {
                const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);
                try o.put(al, "specifiers", specifiers);
            }
        },

        .export_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.local, opts);

            try o.put(al, "local", local);
            if (payload.exported) |e| {
                const exported = try nodeToEsTree(t, al, e, opts);
                try o.put(al, "exported", exported);
            } else {
                try o.put(al, "exported", local);
            }
        },

        .export_all_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            try o.put(al, "source", source);
        },

        .export_declaration => |payload| {
            if (payload.default)
                try o.put(al, "type", JValue{ .string = "ExportDefaultDeclaration" });

            const declaration = try nodeToEsTree(t, al, payload.declaration, opts);
            try o.put(al, "declaration", declaration);
        },

        .export_list_declaration => |payload| {
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);
            try o.put(al, "specifiers", specifiers);
            // to be compatible with babel...
            try o.put(al, "source", JNull);
            try o.put(al, "declaration", JNull);
        },

        .call_expr => |payload| {
            const callee = try nodeToEsTree(t, al, payload.callee, opts);
            const args = if (payload.arguments) |range|
                try subRangeToJsonArray(al, t, range, opts)
            else
                JValue{ .array = std.json.Array.init(al) };
            try o.put(al, "callee", callee);
            try o.put(al, "arguments", args);

            if (meta.activeTag(node.data) == .call_expr and opts.optional_chain_flag)
                try o.put(al, "optional", JValue{ .bool = false });
        },

        .new_expr => |payload| {
            const callee = try nodeToEsTree(t, al, payload.callee, opts);
            if (payload.arguments) |range| {
                const args_val = try subRangeToJsonArray(al, t, range, opts);
                try o.put(al, "arguments", args_val);
            } else {
                try o.put(al, "arguments", JValue{ .array = std.json.Array.init(al) });
            }

            try o.put(al, "callee", callee);
        },

        .super_call_expr => |maybe_args| {
            try o.put(al, "arguments", try subRangeToJsonArray(al, t, maybe_args, opts));
            // TODO: store super identifier as well.
        },

        .spread_element, .rest_element => |arg| {
            const argument = try nodeToEsTree(t, al, arg, opts);
            try o.put(al, "argument", argument);
        },

        .unary_expr,
        .post_unary_expr,
        .update_expr,
        => |payload| {
            const arg = try nodeToEsTree(t, al, payload.operand, opts);
            const operator_str = payload.operator.lexeme();
            try o.put(al, "operator", JValue{ .string = operator_str });
            const is_prefix = std.meta.activeTag(node.data) != .post_unary_expr;
            try o.put(al, "prefix", JValue{ .bool = is_prefix });
            try o.put(al, "argument", arg);
        },

        .yield_expr => |payload| {
            if (payload.value) |arg| {
                const argument = try nodeToEsTree(t, al, arg, opts);
                try o.put(al, "argument", argument);
            }

            try o.put(al, "delegate", JValue{ .bool = payload.is_delegated });
        },

        .await_expr => |payload| {
            const arg = try nodeToEsTree(t, al, payload.operand, opts);
            try o.put(al, "argument", arg);
        },

        .if_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const consequent = try nodeToEsTree(t, al, payload.consequent, opts);
            const alternate = try nodeToEsTree(t, al, payload.alternate, opts);

            try o.put(al, "test", cond);
            try o.put(al, "consequent", consequent);
            try o.put(al, "alternate", alternate);
        },

        .optional_expr => |operand| {
            const arg = try nodeToEsTree(t, al, operand, opts);
            try o.put(al, "argument", arg);
        },

        .object_literal, .object_pattern => |maybe_properties| {
            const properties = try subRangeToJsonArray(al, t, maybe_properties, opts);
            try o.put(al, "properties", properties);
        },

        .object_property => |payload| {
            const key = try nodeToEsTree(t, al, payload.key, opts);
            const value = try nodeToEsTree(t, al, payload.value, opts);
            const kind = @tagName(payload.flags.kind);
            const computed = payload.flags.is_computed;
            const method = payload.flags.is_method;

            try o.put(al, "method", JValue{ .bool = method });
            try o.put(al, "key", key);

            try o.put(al, "computed", JValue{ .bool = computed });
            if (std.meta.activeTag(payload.value.get(t).*) == .assignment_pattern)
                try o.put(al, "shorthand", JTrue)
            else
                try o.put(al, "shorthand", JFalse);
            try o.put(al, "value", value);
            try o.put(al, "kind", JValue{ .string = kind });
        },

        .shorthand_property => |payload| {
            const key = try nodeToEsTree(t, al, payload.name, opts);

            try o.put(al, "method", JFalse);
            try o.put(al, "key", key);
            try o.put(al, "computed", JFalse);
            try o.put(al, "shorthand", JTrue);
            try o.put(al, "value", key);
            try o.put(al, "kind", JValue{ .string = "init" });
        },

        .try_statement => |payload| {
            const block = try nodeToEsTree(t, al, payload.body, opts);
            const handler = try nodeToEsTree(t, al, payload.catch_clause, opts);
            const finalizer = try nodeToEsTree(t, al, payload.finalizer, opts);

            try o.put(al, "block", block);
            try o.put(al, "handler", handler);
            try o.put(al, "finalizer", finalizer);
        },

        .catch_clause => |payload| {
            if (payload.param) |param| {
                const param_ = try nodeToEsTree(t, al, param, opts);
                try o.put(al, "param", param_);
            }

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put(al, "body", body);
        },

        .block_statement => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements, opts);
            try o.put(al, "body", body);
        },

        .statement_list => |stats| {
            const body = try subRangeToJsonArray(al, t, stats, opts);
            try o.put(al, "body", body);
        },

        .with_statement => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put(al, "object", object);
            try o.put(al, "body", body);
        },

        .switch_statement => |payload| {
            const discriminant = try nodeToEsTree(t, al, payload.discriminant, opts);
            const cases = try subRangeToJsonArray(al, t, payload.cases, opts);

            try o.put(al, "discriminant", discriminant);
            try o.put(al, "cases", cases);
        },

        .switch_case => |payload| {
            const cond = try nodeToEsTree(t, al, payload.expression, opts);
            const consequent = try subRangeToJsonArray(al, t, payload.consequent, opts);

            try o.put(al, "test", cond);
            try o.put(al, "consequent", consequent);
        },

        .default_case => |payload| {
            const consequent = try subRangeToJsonArray(al, t, payload.consequent, opts);
            try o.put(al, "consequent", consequent);
        },

        .import_namespace_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.name, opts);
            try o.put(al, "local", local);
        },

        .import_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.local, opts);
            if (payload.imported) |imported| {
                const imported_ = try nodeToEsTree(t, al, imported, opts);
                try o.put(al, "imported", imported_);
            } else {
                try o.put(al, "imported", local);
            }
            try o.put(al, "local", local);
        },

        .import_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);

            try o.put(al, "source", source);
            try o.put(al, "specifiers", specifiers);
        },

        .import_default_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.name, opts);
            try o.put(al, "local", local);
        },

        .variable_declaration => |payload| {
            const declarations = try subRangeToJsonArray(al, t, payload.declarators, opts);
            const kind = @tagName(payload.kind);

            try o.put(al, "declarations", declarations);
            try o.put(al, "kind", JValue{ .string = kind });
        },

        .variable_declarator => |payload| {
            const id = try nodeToEsTree(t, al, payload.lhs, opts);

            try o.put(al, "id", id);
            if (payload.init) |init| {
                const init_ = try nodeToEsTree(t, al, init, opts);
                try o.put(al, "init", init_);
            } else {
                try o.put(al, "init", JNull);
            }
        },

        .member_expr => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put(al, "object", object);
            try o.put(al, "computed", JValue{ .bool = false });
            try o.put(al, "property", property);

            if (opts.optional_chain_flag)
                try o.put(al, "optional", JValue{ .bool = false });
        },

        .computed_member_expr => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put(al, "object", object);
            try o.put(al, "property", property);
            try o.put(al, "computed", JValue{ .bool = true });

            if (opts.optional_chain_flag)
                try o.put(al, "optional", JValue{ .bool = false });
        },

        .function_expr, .function_declaration => |func| {
            const params_range = t.nodes.get(@backingInt(func.parameters)).data.parameters;
            const params = try subRangeToJsonArray(al, t, params_range, opts);
            const body = try nodeToEsTree(t, al, func.body, opts);

            const fn_meta = func.meta.get(t).function_meta;
            if (func.getName(t)) |name| {
                if (name != Node.Index.empty) {
                    const id = try nodeToEsTree(t, al, name, opts);
                    try o.put(al, "id", id);
                }
            }

            try o.put(al, "generator", JValue{ .bool = fn_meta.flags.is_generator });
            try o.put(al, "async", JValue{ .bool = fn_meta.flags.is_async });

            // function_declaration can never be `arrow`, so only have the "arrow"
            // field for function_exprs.
            if (meta.activeTag(node.data) == .function_expr)
                try o.put(al, "arrow", JValue{ .bool = fn_meta.flags.is_arrow });

            try o.put(al, "params", params);
            try o.put(al, "body", body);
        },

        .debugger_statement => {},
        .while_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put(al, "test", cond);
            try o.put(al, "body", body);
        },

        .do_while_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put(al, "test", cond);
            try o.put(al, "body", body);
        },

        .sequence_expr => |payload| {
            const expressions = try subRangeToJsonArray(al, t, payload, opts);
            try o.put(al, "expressions", expressions);
        },

        .assignment_pattern => |payload| {
            const left = try nodeToEsTree(t, al, payload.lhs, opts);
            const right = try nodeToEsTree(t, al, payload.rhs, opts);

            try o.put(al, "left", left);
            try o.put(al, "right", right);
        },

        .empty_statement => {},
        .labeled_statement => |payload| {
            const label = try nodeToEsTree(t, al, payload.label, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put(al, "body", body);
            try o.put(al, "label", label);
        },

        .class_expression, .class_declaration => |class| {
            const class_meta = class.meta.get(t).class_meta;
            if (class_meta.super_class != .empty) {
                const superClass = try nodeToEsTree(t, al, class_meta.super_class, opts);
                try o.put(al, "superClass", superClass);
            }

            if (class_meta.name != .empty) {
                const id_ = try nodeToEsTree(t, al, class_meta.name, opts);
                try o.put(al, "id", id_);
            }

            const body_defs = try subRangeToJsonArray(al, t, class.body, opts);
            var body = std.json.ObjectMap.empty;
            try body.put(al, "type", JValue{ .string = "ClassBody" });
            try body.put(al, "body", body_defs);

            try o.put(al, "body", JValue{ .object = body });
        },

        .class_field, .class_method => |payload| {
            const key = try nodeToEsTree(t, al, payload.key, opts);
            const value = try nodeToEsTree(t, al, payload.value, opts);
            const computed = payload.flags.is_computed;
            const static = payload.flags.is_static;

            try o.put(al, "key", key);
            try o.put(al, "value", value);
            try o.put(al, "computed", JValue{ .bool = computed });
            try o.put(al, "static", JValue{ .bool = static });

            if (payload.flags.kind != .init) {
                const kind = @tagName(payload.flags.kind);
                try o.put(al, "kind", JValue{ .string = kind });
            }
        },

        .template_literal => |payload| {
            const items = payload.asSlice(t);

            var exprs = std.json.Array.init(al);
            var quasis = std.json.Array.init(al);

            for (0.., items) |i, item| {
                if (i % 2 == 0) {
                    try quasis.append(try nodeToEsTree(t, al, item, opts));
                } else {
                    try exprs.append(try nodeToEsTree(t, al, item, opts));
                }
            }

            try o.put(al, "quasis", JValue{ .array = quasis });
            try o.put(al, "expressions", JValue{ .array = exprs });
        },

        .template_element => |token_id| {
            const value = token_id.slice(t.source);
            try o.put(al, "value", JValue{ .string = value });
            const is_tail = value[0] == '{';
            try o.put(al, "tail", JValue{ .bool = is_tail });
        },

        .throw_statement => |payload| {
            const argument = try nodeToEsTree(t, al, payload, opts);
            try o.put(al, "argument", argument);
        },

        .break_statement, .continue_statement => |payload| {
            if (payload.label) |label| {
                const label_ = try nodeToEsTree(t, al, label, opts);
                try o.put(al, "label", label_);
            }
        },

        .for_statement => |payload| {
            const iter = payload.iterator.get(t).for_iterator;
            try o.put(al, "init", try nodeToEsTree(t, al, iter.init, opts));
            try o.put(al, "test", try nodeToEsTree(t, al, iter.condition, opts));
            try o.put(al, "update", try nodeToEsTree(t, al, iter.update, opts));

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put(al, "body", body);
        },

        // covered in .for_statement
        .for_in_of_iterator, .for_iterator => unreachable,

        .for_in_statement,
        .for_of_statement,
        => |payload| {
            const iter = payload.iterator.get(t).for_in_of_iterator;
            const left = try nodeToEsTree(t, al, iter.left, opts);
            const right = try nodeToEsTree(t, al, iter.right, opts);

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put(al, "left", left);
            try o.put(al, "right", right);
            try o.put(al, "body", body);
        },

        .return_statement => |payload| {
            if (payload) |arg| {
                const argument = try nodeToEsTree(t, al, arg, opts);
                try o.put(al, "argument", argument);
            }
        },

        .conditional_expr => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const consequent = try nodeToEsTree(t, al, payload.consequent, opts);
            const alternate = try nodeToEsTree(t, al, payload.alternate, opts);

            try o.put(al, "test", cond);
            try o.put(al, "consequent", consequent);
            try o.put(al, "alternate", alternate);
        },

        .meta_property => |payload| {
            const obj = try nodeToEsTree(t, al, payload.meta, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put(al, "meta", obj);
            try o.put(al, "property", property);
        },

        .tagged_template_expr => |payload| {
            const pl_tag = try nodeToEsTree(t, al, payload.tag, opts);
            const quasi = try nodeToEsTree(t, al, payload.template, opts);

            try o.put(al, "tag", pl_tag);
            try o.put(al, "quasi", quasi);
        },

        .jsx_fragment => |fragment| {
            const tags = fragment.getOpenAndCloseTags(t);

            const open_fragment = blk: {
                var obj = std.json.ObjectMap.empty;
                try obj.put(al, "type", JValue{ .string = "JSXOpeningFragment" });

                const open = tags.getOpening(t);
                if (opts.start_end_locs) {
                    try obj.put(al, "start", JValue{ .integer = open.lt.start });
                    try obj.put(al, "end", JValue{ .integer = open.gt.end });
                }
                break :blk JValue{ .object = obj };
            };

            const close_fragment = blk: {
                var obj = std.json.ObjectMap.empty;
                try obj.put(al, "type", JValue{ .string = "JSXClosingFragment" });

                const close = tags.getClosing(t);
                if (opts.start_end_locs) {
                    try obj.put(al, "start", JValue{ .integer = close.lt.start });
                    try obj.put(al, "end", JValue{ .integer = close.gt.end });
                }
                break :blk JValue{ .object = obj };
            };

            try o.put(al, "openingFragment", open_fragment);
            try o.put(al, "closingFragment", close_fragment);

            const children_json = try subRangeToJsonArray(al, t, fragment.children, opts);
            try o.put(al, "children", children_json);
        },

        .jsx_namespaced_name => |payload| {
            const namespace = try nodeToEsTree(t, al, payload.namespace, opts);
            const name = try nodeToEsTree(t, al, payload.name, opts);

            try o.put(al, "namespace", namespace);
            try o.put(al, "name", name);
        },

        .jsx_member_expression => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put(al, "object", object);
            try o.put(al, "property", property);
        },

        .jsx_attribute => |payload| {
            const name = try nodeToEsTree(t, al, payload.name, opts);

            try o.put(al, "name", name);
            if (payload.value) |v| {
                try o.put(al, "value", try nodeToEsTree(t, al, v, opts));
            }
        },

        .jsx_spread_attribute => |arg| {
            const argument = try nodeToEsTree(t, al, arg, opts);
            try o.put(al, "argument", argument);
        },

        .jsx_element => |payload| {
            const opening = try nodeToEsTree(t, al, payload.opening_element, opts);
            const closing = try nodeToEsTree(t, al, payload.closing_element, opts);
            const children = if (payload.children != Node.Index.empty)
                try subRangeToJsonArray(al, t, payload.children.get(t).jsx_children, opts)
            else
                JValue{ .array = std.json.Array.init(al) };

            try o.put(al, "openingElement", opening);
            try o.put(al, "closingElement", closing);
            try o.put(al, "children", children);
        },

        .jsx_opening_element => try jsxOpeningOrSelfClosingElement(al, t, &o, node.data, opts),
        .jsx_self_closing_element => {
            // In Babel's ESTree output, we get a JSXElement node that has an opening element
            // but no closing element. Whereas most other tools just give you a "JSXSelfClosingElement"
            // object.
            if (opts.babel_estree_compat) {
                try o.put(al, "type", JValue{ .string = "JSXElement" });

                var opening_el_json = std.json.ObjectMap.empty;
                try opening_el_json.put(al, "type", JValue{ .string = "JSXOpeningElement" });
                try opening_el_json.put(al, "start", o.get("start").?);
                try opening_el_json.put(al, "end", o.get("end").?);

                try jsxOpeningOrSelfClosingElement(al, t, &opening_el_json, node.data, opts);
                try o.put(al, "openingElement", JValue{ .object = opening_el_json });
                try o.put(al, "closingElement", JNull);
                try o.put(al, "children", JValue{ .array = std.json.Array.init(al) });
            } else {
                try jsxOpeningOrSelfClosingElement(al, t, &o, node.data, opts);
            }
        },

        .jsx_closing_element => |payload| {
            const name = try nodeToEsTree(t, al, payload.name, opts);
            try o.put(al, "name", name);
        },

        .jsx_expression => |e| {
            const expression = try nodeToEsTree(t, al, e, opts);
            try o.put(al, "expression", expression);
        },

        .jsx_spread_child => |e| {
            const expression = try nodeToEsTree(t, al, e, opts);
            try o.put(al, "expression", expression);
        },

        .jsx_text => |token_id| {
            const raw = token_id.slice(t.source);
            try o.put(al, "value", JValue{
                .string = try processEscapes(al, raw),
            });
            try o.put(al, "raw", JValue{ .string = raw });
        },

        // this should be unreachable
        .parameters,
        .none,
        .parenthesized_expr,
        .class_meta,
        .function_meta,
        .jsx_children,
        => {
            std.debug.panic("Bad call to nodeToEstree: parameters", .{});
        },
    }

    return JValue{ .object = o };
}

/// Convert a JSX Opening or Self closing element into ESTree JSON.
/// Populates the [json_object] passed as an argument.
fn jsxOpeningOrSelfClosingElement(
    al: std.mem.Allocator,
    t: *const ast.Tree,
    /// The JSON object to write to
    json_object: *std.json.ObjectMap,
    node: ast.NodeData,
    opts: EstreeJsonOpts,
) !void {
    assert(meta.activeTag(node) == .jsx_opening_element or
        meta.activeTag(node) == .jsx_self_closing_element);

    const is_self_closing = meta.activeTag(node) == .jsx_self_closing_element;
    const pl = if (is_self_closing)
        node.jsx_self_closing_element
    else
        node.jsx_opening_element;

    const name = try nodeToEsTree(t, al, pl.name, opts);
    const attributes = try subRangeToJsonArray(al, t, pl.attributes, opts);

    try json_object.put(al, "name", name);
    try json_object.put(al, "attributes", attributes);
    try json_object.put(al, "selfClosing", JValue{ .bool = is_self_closing });
}

const EstreeJson = struct {
    arena: *std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    tree: std.json.Value,

    pub fn deinit(self: EstreeJson) void {
        self.arena.deinit();
        self.allocator.destroy(self.arena);
    }
};

pub fn toJsonObject(
    allocator: std.mem.Allocator,
    t: *const Tree,
    options: EstreeJsonOpts,
) !EstreeJson {
    var arena = try allocator.create(std.heap.ArenaAllocator);

    arena.* = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();

    const estree = try nodeToEsTree(t, al, t.root, options);
    return EstreeJson{
        .arena = arena,
        .allocator = allocator,
        .tree = estree,
    };
}

pub fn toJsonString(
    allocator: std.mem.Allocator,
    t: *const Tree,
    options: EstreeJsonOpts,
) ![]u8 {
    const estree_json = try toJsonObject(allocator, t, options);
    defer estree_json.deinit();

    return try std.json.Stringify.valueAlloc(allocator, estree_json.tree, .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });
}
