// Contains helpers that convert a Jam AST to
// ESTree compatible JSON.

const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");
const util = @import("util");

const meta = std.meta;

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
};

/// Options to use for ESTree conversion when the output has to be
/// compatible with `@babel/parser`.
pub const BabelEstreeOptions = EstreeJsonOpts{
    .interpreter = true,
    .source_type = true,
    .optional_chain_flag = true,
};

fn subRangeToJsonArray(
    al: std.mem.Allocator,
    t: *const Tree,
    maybe_args: ?ast.SubRange,
    opts: EstreeJsonOpts,
) !JValue {
    var arr = std.json.Array.init(al);
    if (maybe_args) |arguments| {
        const from: usize = @intFromEnum(arguments.from);
        const to: usize = @intFromEnum(arguments.to);
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
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

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
            try buf.appendSlice(cp_slice[0..cp_len]);

            continue;
        }

        const cp_slice = iter.nextCodepointSlice() orelse
            unreachable; // already validated UTF-8 during tokenization
        try buf.appendSlice(cp_slice);
    }

    return buf.toOwnedSlice();
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
        .arguments => "Arguments",
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
        .block_statement => "BlockStatement",
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
        .parameters => unreachable,
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
        .none => unreachable,
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
    const node = t.nodes.get(@intFromEnum(node_id));
    switch (node.data) {
        .none, .empty_array_item => return JValue{ .null = {} },
        .parenthesized_expr => |e| return nodeToEsTree(t, al, e, opts),
        else => {},
    }

    var o = std.json.ObjectMap.init(al);
    const tag = jamToEstreeTag(node.data);
    try o.put("type", JValue{ .string = tag });
    if (opts.start_end_locs) {
        try o.put("start", JValue{ .integer = @intCast(t.getToken(node.start).start) });
        const end_token = t.getToken(node.end);
        try o.put("end", JValue{ .integer = @intCast(end_token.start + end_token.len) });
    }

    switch (node.data) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try nodeToEsTree(t, al, payload.lhs, opts);
            const rhs = try nodeToEsTree(t, al, payload.rhs, opts);
            const token = t.tokens.items[(@intFromEnum(payload.operator))];
            const operator = token.toByteSlice(t.source);

            try o.put("operator", JValue{ .string = operator });
            try o.put("left", lhs);
            try o.put("right", rhs);
        },

        .binding_identifier, .identifier, .identifier_reference => |tok_id| {
            const name = t.getToken(tok_id).toByteSlice(t.source);
            const escapedName = try processEscapes(al, name);
            try o.put("name", JValue{ .string = escapedName });
        },

        .program => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements, opts);
            if (opts.source_type) try o.put("sourceType", JValue{ .string = @tagName(t.source_type) });
            if (opts.interpreter) try o.put("interpreter", JValue{ .null = {} });
            try o.put("body", body);
        },

        .expression_statement => |e| {
            const expression = try nodeToEsTree(t, al, e, opts);
            try o.put("expression", expression);
        },

        .empty_array_item, .this, .super => {},
        .array_literal,
        .array_pattern,
        => |maybe_elements| {
            const elements = try subRangeToJsonArray(al, t, maybe_elements, opts);
            try o.put("elements", elements);
        },

        .string_literal => |token_id| {
            const token = t.getToken(token_id);
            const raw = token.toByteSlice(t.source);
            try o.put("value", JValue{
                .string = try processEscapes(al, raw[1 .. raw.len - 1]),
            });
            try o.put("raw", JValue{ .string = raw });
        },

        .null_literal => |token_id| {
            const token = t.getToken(token_id);
            const raw = token.toByteSlice(t.source);
            try o.put("value", JNull);
            try o.put("raw", JValue{ .string = raw });
        },

        .boolean_literal => |b| {
            const token = t.getToken(b.token);
            const raw = token.toByteSlice(t.source);
            try o.put("value", JValue{ .bool = b.value });
            try o.put("raw", JValue{ .string = raw });
        },

        .number_literal => |num| {
            const token = t.getToken(num.token);
            const raw = token.toByteSlice(t.source);
            const value = num.value(t);
            if (value == @trunc(value))
                try o.put("value", JValue{ .number_string = raw })
            else
                try o.put("value", JValue{ .float = num.value(t) });

            try o.put("raw", JValue{ .string = raw });
        },

        .regex_literal => |token_id| {
            const token = t.getToken(token_id);
            const raw = token.toByteSlice(t.source);
            try o.put("raw", JValue{ .string = raw });
            // In babel's ESTree output, a regex literal's value is an empty object
            // when JSON serialized
            try o.put("value", JValue{ .object = std.json.ObjectMap.init(al) });
        },

        .export_from_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            std.debug.assert(meta.activeTag(source) == .object);

            try o.put("source", source);
            if (payload.specifiers.asSlice(t).len > 0) {
                const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);
                try o.put("specifiers", specifiers);
            }
        },

        .export_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.local, opts);

            try o.put("local", local);
            if (payload.exported) |e| {
                const exported = try nodeToEsTree(t, al, e, opts);
                try o.put("exported", exported);
            } else {
                try o.put("exported", local);
            }
        },

        .export_all_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            try o.put("source", source);
        },

        .export_declaration => |payload| {
            if (payload.default)
                try o.put("type", JValue{ .string = "ExportDefaultDeclaration" });

            const declaration = try nodeToEsTree(t, al, payload.declaration, opts);
            try o.put("declaration", declaration);
        },

        .export_list_declaration => |payload| {
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);
            try o.put("specifiers", specifiers);
            // to be compatible with babel...
            try o.put("source", JNull);
            try o.put("declaration", JNull);
        },

        .call_expr => |payload| {
            const callee = try nodeToEsTree(t, al, payload.callee, opts);
            const arguments = payload.arguments.get(t).arguments;
            const args = try subRangeToJsonArray(al, t, arguments, opts);
            try o.put("callee", callee);
            try o.put("arguments", args);

            if (meta.activeTag(node.data) == .call_expr and opts.optional_chain_flag)
                try o.put("optional", JValue{ .bool = false });
        },

        .new_expr => |payload| {
            const callee = try nodeToEsTree(t, al, payload.callee, opts);
            if (payload.arguments) |args| {
                const args_range = t.getNode(args).data.arguments;
                const args_val = try subRangeToJsonArray(al, t, args_range, opts);
                try o.put("arguments", args_val);
            } else {
                try o.put("arguments", JValue{ .array = std.json.Array.init(al) });
            }

            try o.put("callee", callee);
        },

        .super_call_expr => |maybe_args| {
            try o.put("arguments", try subRangeToJsonArray(al, t, maybe_args, opts));
            // TODO: store super identifier as well.
        },

        .spread_element, .rest_element => |arg| {
            const argument = try nodeToEsTree(t, al, arg, opts);
            try o.put("argument", argument);
        },

        .unary_expr,
        .post_unary_expr,
        .update_expr,
        => |payload| {
            const operator = t.tokens.items[(@intFromEnum(payload.operator))];
            const arg = try nodeToEsTree(t, al, payload.operand, opts);
            const operator_str = operator.toByteSlice(t.source);
            try o.put("operator", JValue{ .string = operator_str });
            const is_prefix = std.meta.activeTag(node.data) != .post_unary_expr;
            try o.put("prefix", JValue{ .bool = is_prefix });
            try o.put("argument", arg);
        },

        .yield_expr => |payload| {
            if (payload.value) |arg| {
                const argument = try nodeToEsTree(t, al, arg, opts);
                try o.put("argument", argument);
            }

            try o.put("delegate", JValue{ .bool = payload.is_delegated });
        },

        .await_expr => |payload| {
            const arg = try nodeToEsTree(t, al, payload.operand, opts);
            try o.put("argument", arg);
        },

        .if_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const consequent = try nodeToEsTree(t, al, payload.consequent, opts);
            const alternate = try nodeToEsTree(t, al, payload.alternate, opts);

            try o.put("test", cond);
            try o.put("consequent", consequent);
            try o.put("alternate", alternate);
        },

        .optional_expr => |operand| {
            const arg = try nodeToEsTree(t, al, operand, opts);
            try o.put("argument", arg);
        },

        .object_literal, .object_pattern => |maybe_properties| {
            const properties = try subRangeToJsonArray(al, t, maybe_properties, opts);
            try o.put("properties", properties);
        },

        .object_property => |payload| {
            const key = try nodeToEsTree(t, al, payload.key, opts);
            const value = try nodeToEsTree(t, al, payload.value, opts);
            const kind = @tagName(payload.flags.kind);
            const computed = payload.flags.is_computed;
            const method = payload.flags.is_method;

            try o.put("method", JValue{ .bool = method });
            try o.put("key", key);
            // TODO: if the value is of type assignment_pattern,
            // the flag shorthand is set in ESTree for some reason?
            try o.put("computed", JValue{ .bool = computed });
            try o.put("shorthand", JFalse);
            try o.put("value", value);
            try o.put("kind", JValue{ .string = kind });
        },

        .shorthand_property => |payload| {
            const key = try nodeToEsTree(t, al, payload.name, opts);

            try o.put("method", JFalse);
            try o.put("key", key);
            try o.put("computed", JFalse);
            try o.put("shorthand", JTrue);
            try o.put("value", key);
            try o.put("kind", JValue{ .string = "init" });
        },

        .try_statement => |payload| {
            const block = try nodeToEsTree(t, al, payload.body, opts);
            const handler = try nodeToEsTree(t, al, payload.catch_clause, opts);
            const finalizer = try nodeToEsTree(t, al, payload.finalizer, opts);

            try o.put("block", block);
            try o.put("handler", handler);
            try o.put("finalizer", finalizer);
        },

        .catch_clause => |payload| {
            if (payload.param) |param| {
                const param_ = try nodeToEsTree(t, al, param, opts);
                try o.put("param", param_);
            }

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put("body", body);
        },

        .block_statement => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements, opts);
            try o.put("body", body);
        },

        .with_statement => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put("object", object);
            try o.put("body", body);
        },

        .switch_statement => |payload| {
            const discriminant = try nodeToEsTree(t, al, payload.discriminant, opts);
            const cases = try subRangeToJsonArray(al, t, payload.cases, opts);

            try o.put("discriminant", discriminant);
            try o.put("cases", cases);
        },

        .switch_case => |payload| {
            const cond = try nodeToEsTree(t, al, payload.expression, opts);
            const consequent = try subRangeToJsonArray(al, t, payload.consequent, opts);

            try o.put("test", cond);
            try o.put("consequent", consequent);
        },

        .default_case => |payload| {
            const consequent = try subRangeToJsonArray(al, t, payload.consequent, opts);
            try o.put("consequent", consequent);
        },

        .import_namespace_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.name, opts);
            try o.put("local", local);
        },

        .import_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.local, opts);
            if (payload.imported) |imported| {
                const imported_ = try nodeToEsTree(t, al, imported, opts);
                try o.put("imported", imported_);
            } else {
                try o.put("imported", local);
            }
            try o.put("local", local);
        },

        .import_declaration => |payload| {
            const source = try nodeToEsTree(t, al, payload.source, opts);
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers, opts);

            try o.put("source", source);
            try o.put("specifiers", specifiers);
        },

        .import_default_specifier => |payload| {
            const local = try nodeToEsTree(t, al, payload.name, opts);
            try o.put("local", local);
        },

        .variable_declaration => |payload| {
            const declarations = try subRangeToJsonArray(al, t, payload.declarators, opts);
            const kind = @tagName(payload.kind);

            try o.put("declarations", declarations);
            try o.put("kind", JValue{ .string = kind });
        },

        .variable_declarator => |payload| {
            const id = try nodeToEsTree(t, al, payload.lhs, opts);

            try o.put("id", id);
            if (payload.init) |init| {
                const init_ = try nodeToEsTree(t, al, init, opts);
                try o.put("init", init_);
            } else {
                try o.put("init", JNull);
            }
        },

        .member_expr => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put("object", object);
            try o.put("computed", JValue{ .bool = false });
            try o.put("property", property);

            if (opts.optional_chain_flag)
                try o.put("optional", JValue{ .bool = false });
        },

        .computed_member_expr => |payload| {
            const object = try nodeToEsTree(t, al, payload.object, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put("object", object);
            try o.put("property", property);
            try o.put("computed", JValue{ .bool = true });

            if (opts.optional_chain_flag)
                try o.put("optional", JValue{ .bool = false });
        },

        .function_expr, .function_declaration => |payload| {
            const params_range = t.nodes.get(@intFromEnum(payload.parameters)).data.parameters;
            const params = try subRangeToJsonArray(al, t, params_range, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            const extra: ast.ExtraData = t.getExtraData(payload.info);
            const fn_info = extra.function;
            if (fn_info.name) |name| {
                if (name != Node.Index.empty) {
                    const id = try nodeToEsTree(t, al, name, opts);
                    try o.put("id", id);
                }
            }

            try o.put("generator", JValue{ .bool = fn_info.flags.is_generator });
            try o.put("async", JValue{ .bool = fn_info.flags.is_async });

            // function_declaration can never be `arrow`, so only have the "arrow"
            // field for function_exprs.
            if (meta.activeTag(node.data) == .function_expr)
                try o.put("arrow", JValue{ .bool = fn_info.flags.is_arrow });

            try o.put("params", params);
            try o.put("body", body);
        },

        .debugger_statement => {},
        .while_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put("test", cond);
            try o.put("body", body);
        },

        .do_while_statement => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put("test", cond);
            try o.put("body", body);
        },

        .sequence_expr => |payload| {
            const expressions = try subRangeToJsonArray(al, t, payload, opts);
            try o.put("expressions", expressions);
        },

        .assignment_pattern => |payload| {
            const left = try nodeToEsTree(t, al, payload.lhs, opts);
            const right = try nodeToEsTree(t, al, payload.rhs, opts);

            try o.put("left", left);
            try o.put("right", right);
        },

        .empty_statement => {},
        .labeled_statement => |payload| {
            const label = try nodeToEsTree(t, al, payload.label, opts);
            const body = try nodeToEsTree(t, al, payload.body, opts);

            try o.put("body", body);
            try o.put("label", label);
        },

        .class_expression, .class_declaration => |payload| {
            const info = t.getExtraData(payload.class_information).class;
            if (info.super_class != .empty) {
                const superClass = try nodeToEsTree(t, al, info.super_class, opts);
                try o.put("superClass", superClass);
            }

            if (info.name) |id| {
                const id_ = try nodeToEsTree(t, al, id, opts);
                try o.put("id", id_);
            }

            const body_defs = try subRangeToJsonArray(al, t, payload.body, opts);
            var body = std.json.ObjectMap.init(al);
            try body.put("type", JValue{ .string = "ClassBody" });
            try body.put("body", body_defs);

            try o.put("body", JValue{ .object = body });
        },

        .class_field, .class_method => |payload| {
            const key = try nodeToEsTree(t, al, payload.key, opts);
            const value = try nodeToEsTree(t, al, payload.value, opts);
            const computed = payload.flags.is_computed;
            const static = payload.flags.is_static;

            try o.put("key", key);
            try o.put("value", value);
            try o.put("computed", JValue{ .bool = computed });
            try o.put("static", JValue{ .bool = static });

            if (payload.flags.kind != .init) {
                const kind = @tagName(payload.flags.kind);
                try o.put("kind", JValue{ .string = kind });
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

            try o.put("quasis", JValue{ .array = quasis });
            try o.put("expressions", JValue{ .array = exprs });
        },

        .template_element => |token_id| {
            const value = t.getToken(token_id).toByteSlice(t.source);
            try o.put("value", JValue{ .string = value });
            const is_tail = value[0] == '{';
            try o.put("tail", JValue{ .bool = is_tail });
        },

        .throw_statement => |payload| {
            const argument = try nodeToEsTree(t, al, payload, opts);
            try o.put("argument", argument);
        },

        .break_statement, .continue_statement => |payload| {
            if (payload.label) |label| {
                const label_ = try nodeToEsTree(t, al, label, opts);
                try o.put("label", label_);
            }
        },

        .for_statement => |payload| {
            const iter = t.getExtraData(payload.iterator).for_iterator;
            try o.put("init", try nodeToEsTree(t, al, iter.init, opts));
            try o.put("test", try nodeToEsTree(t, al, iter.condition, opts));
            try o.put("update", try nodeToEsTree(t, al, iter.update, opts));

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put("body", body);
        },

        .for_in_statement,
        .for_of_statement,
        => |payload| {
            const iter = t.getExtraData(payload.iterator).for_in_of_iterator;
            const left = try nodeToEsTree(t, al, iter.left, opts);
            const right = try nodeToEsTree(t, al, iter.right, opts);

            const body = try nodeToEsTree(t, al, payload.body, opts);
            try o.put("left", left);
            try o.put("right", right);
            try o.put("body", body);
        },

        .return_statement => |payload| {
            if (payload) |arg| {
                const argument = try nodeToEsTree(t, al, arg, opts);
                try o.put("argument", argument);
            }
        },

        .conditional_expr => |payload| {
            const cond = try nodeToEsTree(t, al, payload.condition, opts);
            const consequent = try nodeToEsTree(t, al, payload.consequent, opts);
            const alternate = try nodeToEsTree(t, al, payload.alternate, opts);

            try o.put("test", cond);
            try o.put("consequent", consequent);
            try o.put("alternate", alternate);
        },

        .meta_property => |payload| {
            const obj = try nodeToEsTree(t, al, payload.meta, opts);
            const property = try nodeToEsTree(t, al, payload.property, opts);

            try o.put("meta", obj);
            try o.put("property", property);
        },

        .tagged_template_expr => |payload| {
            const pl_tag = try nodeToEsTree(t, al, payload.tag, opts);
            const quasi = try nodeToEsTree(t, al, payload.template, opts);

            try o.put("tag", pl_tag);
            try o.put("quasi", quasi);
        },

        // this should be unreachable
        .parameters,
        .none,
        .arguments,
        .parenthesized_expr,
        => {
            std.debug.panic("Bad call to nodeToEstree: parameters", .{});
        },
    }

    return JValue{ .object = o };
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

    return try std.json.stringifyAlloc(allocator, estree_json.tree, .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });
}
