// Contains helpers that convert a Jam AST to
// ESTree compatible JSON.

const std = @import("std");

const Parser = @import("parser.zig");
const ast = @import("ast.zig");
const util = @import("util");

const Node = ast.Node;
const Tree = ast.Tree;

const JsonValue = std.json.Value;

fn copy(al: std.mem.Allocator, value: anytype) !*@TypeOf(value) {
    const ptr = try al.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

fn checkField(v: anytype, want: std.meta.Tag(@TypeOf(v))) bool {
    return std.meta.activeTag(v) == want;
}

fn subRangeToJsonArray(
    al: std.mem.Allocator,
    t: *const Tree,
    maybe_args: ?ast.SubRange,
) !JsonValue {
    var arr = std.json.Array.init(al);
    if (maybe_args) |arguments| {
        const from: usize = @intFromEnum(arguments.from);
        const to: usize = @intFromEnum(arguments.to);
        for (from..to) |i| {
            const arg_node = t.node_indices.items[i];
            const new_arg = try nodeToEstree(t, al, arg_node);
            try arr.append(new_arg);
        }
    }
    return JsonValue{ .array = arr };
}

fn escapeUtf8(allocator: std.mem.Allocator, str: []const u8) error{
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
        .identifier => "Identifier",
        .literal => "Literal",
        .this => "ThisExpression",
        .empty_array_item => "EmptyArrayItem",
        .array_literal => "ArrayExpression",
        .array_pattern => "ArrayPattern",
        .spread_element => "SpreadElement",
        .rest_element => "RestElement",
        .object_literal => "ObjectExpression",
        .object_property => "Property",
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
        .none => unreachable,
    };
}

fn nodeToEstree(
    t: *const Tree,
    al: std.mem.Allocator,
    node_id: Node.Index,
) error{
    InvalidCodePoint,
    OutOfMemory,
    BadEscapeSequence,
}!JsonValue {
    const node = t.nodes.get(@intFromEnum(node_id));
    switch (node.data) {
        .none, .empty_array_item => return JsonValue{ .null = {} },
        else => {},
    }

    var o = std.json.ObjectMap.init(al);
    const tag = jamToEstreeTag(node.data);
    try o.put("type", JsonValue{ .string = tag });
    try o.put("start", JsonValue{ .integer = @intCast(t.getToken(node.start).start) });
    const end_token = t.getToken(node.end);
    try o.put("end", JsonValue{ .integer = @intCast(end_token.start + end_token.len) });

    switch (node.data) {
        .binary_expr,
        .assignment_expr,
        => |payload| {
            const lhs = try nodeToEstree(t, al, payload.lhs);
            const rhs = try nodeToEstree(t, al, payload.rhs);
            const token = t.tokens.items[(@intFromEnum(payload.operator))];
            const operator = token.toByteSlice(t.source);

            try o.put("operator", JsonValue{ .string = operator });
            try o.put("left", lhs);
            try o.put("right", rhs);
        },

        .identifier => |s| {
            const name = t.string_pool.toByteSlice(s);
            try o.put("name", JsonValue{ .string = name });
        },

        .program => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements);
            // try o.put("interpreter", JsonValue{ .null = {} });
            // try o.put("sourceType", JsonValue{ .string = @tagName(t.source_type) });
            try o.put("body", body);
        },

        .expression_statement => |e| {
            const expression = try nodeToEstree(t, al, e);
            try o.put("expression", expression);
        },

        .empty_array_item, .this, .super => {},
        .array_literal,
        .array_pattern,
        => |maybe_elements| {
            const elements = try subRangeToJsonArray(al, t, maybe_elements);
            try o.put("elements", elements);
        },
        .literal => |token_id| {
            const token = t.getToken(token_id);
            const value = token.toByteSlice(t.source);
            if (token.tag == .string_literal) {
                try o.put("value", JsonValue{ .string = try escapeUtf8(al, value) });
            } else {
                try o.put("value", JsonValue{ .string = value });
            }
        },

        .export_from_declaration => |payload| {
            const source = try nodeToEstree(t, al, payload.source);
            try o.put("source", source);
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers);
            try o.put("specifiers", specifiers);
        },

        .export_specifier => |payload| {
            const local = try nodeToEstree(t, al, payload.local);

            try o.put("local", local);
            if (payload.exported) |e| {
                const exported = try nodeToEstree(t, al, e);
                try o.put("exported", exported);
            } else {
                try o.put("exported", local);
            }
        },

        .export_all_declaration => |payload| {
            const source = try nodeToEstree(t, al, payload.source);
            try o.put("source", source);
        },

        .export_declaration => |payload| {
            const declaration = try nodeToEstree(t, al, payload.declaration);
            try o.put("declaration", declaration);
        },

        .export_list_declaration => |payload| {
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers);
            try o.put("specifiers", specifiers);
        },

        .call_expr, .new_expr => |payload| {
            const callee = try nodeToEstree(t, al, payload.callee);
            const arguments = t.getNode(payload.arguments).data.arguments;
            const args = try subRangeToJsonArray(al, t, arguments);
            try o.put("callee", callee);
            try o.put("arguments", args);
        },

        .super_call_expr => |maybe_args| {
            try o.put("arguments", try subRangeToJsonArray(al, t, maybe_args));
            // TODO: store super identifier as well.
        },

        .spread_element, .rest_element => |arg| {
            const argument = try nodeToEstree(t, al, arg);
            try o.put("argument", argument);
        },

        .unary_expr,
        .post_unary_expr,
        .update_expr,
        => |payload| {
            const operator = t.tokens.items[(@intFromEnum(payload.operator))];
            const arg = try nodeToEstree(t, al, payload.operand);
            const operator_str = operator.toByteSlice(t.source);
            try o.put("operator", JsonValue{ .string = operator_str });
            try o.put("argument", arg);
        },

        .yield_expr => |payload| {
            if (payload.value) |arg| {
                const argument = try nodeToEstree(t, al, arg);
                try o.put("argument", argument);
            }

            try o.put("delegate", JsonValue{ .bool = payload.is_delegated });
        },

        .await_expr => |payload| {
            const arg = try nodeToEstree(t, al, payload.operand);
            try o.put("argument", arg);
        },

        .if_statement => |payload| {
            const cond = try nodeToEstree(t, al, payload.condition);
            const consequent = try nodeToEstree(t, al, payload.consequent);
            const alternate = try nodeToEstree(t, al, payload.alternate);

            try o.put("test", cond);
            try o.put("consequent", consequent);
            try o.put("alternate", alternate);
        },

        .optional_expr => |operand| {
            const arg = try nodeToEstree(t, al, operand);
            try o.put("argument", arg);
        },

        .object_literal, .object_pattern => |maybe_properties| {
            const properties = try subRangeToJsonArray(al, t, maybe_properties);
            try o.put("properties", properties);
        },

        .object_property => |payload| {
            const key = try nodeToEstree(t, al, payload.key);
            const value = try nodeToEstree(t, al, payload.value);
            const kind = @tagName(payload.flags.kind);
            const computed = payload.flags.is_computed;
            const shorthand = payload.flags.is_shorthand;
            const method = payload.flags.is_method;

            try o.put("key", key);
            try o.put("value", value);
            try o.put("kind", JsonValue{ .string = kind });
            try o.put("computed", JsonValue{ .bool = computed });
            try o.put("shorthand", JsonValue{ .bool = shorthand });
            try o.put("method", JsonValue{ .bool = method });
        },

        .try_statement => |payload| {
            const block = try nodeToEstree(t, al, payload.body);
            const handler = try nodeToEstree(t, al, payload.catch_clause);
            const finalizer = try nodeToEstree(t, al, payload.finalizer);

            try o.put("block", block);
            try o.put("handler", handler);
            try o.put("finalizer", finalizer);
        },

        .catch_clause => |payload| {
            if (payload.param) |param| {
                const param_ = try nodeToEstree(t, al, param);
                try o.put("param", param_);
            }

            const body = try nodeToEstree(t, al, payload.body);
            try o.put("body", body);
        },

        .block_statement => |maybe_statements| {
            const body = try subRangeToJsonArray(al, t, maybe_statements);
            try o.put("body", body);
        },

        .with_statement => |payload| {
            const object = try nodeToEstree(t, al, payload.object);
            const body = try nodeToEstree(t, al, payload.body);

            try o.put("object", object);
            try o.put("body", body);
        },

        .switch_statement => |payload| {
            const discriminant = try nodeToEstree(t, al, payload.discriminant);
            const cases = try subRangeToJsonArray(al, t, payload.cases);

            try o.put("discriminant", discriminant);
            try o.put("cases", cases);
        },

        .switch_case => |payload| {
            const cond = try nodeToEstree(t, al, payload.expression);
            const consequent = try subRangeToJsonArray(al, t, payload.consequent);

            try o.put("test", cond);
            try o.put("consequent", consequent);
        },

        .default_case => |payload| {
            const consequent = try subRangeToJsonArray(al, t, payload.consequent);
            try o.put("consequent", consequent);
        },

        .import_namespace_specifier => |payload| {
            const local = try nodeToEstree(t, al, payload.name);
            try o.put("local", local);
        },

        .import_specifier => |payload| {
            const local = try nodeToEstree(t, al, payload.local);
            if (payload.imported) |imported| {
                const imported_ = try nodeToEstree(t, al, imported);
                try o.put("imported", imported_);
            } else {
                try o.put("imported", local);
            }
            try o.put("local", local);
        },

        .import_declaration => |payload| {
            const source = try nodeToEstree(t, al, payload.source);
            const specifiers = try subRangeToJsonArray(al, t, payload.specifiers);

            try o.put("source", source);
            try o.put("specifiers", specifiers);
        },

        .import_default_specifier => |payload| {
            const local = try nodeToEstree(t, al, payload.name);
            try o.put("local", local);
        },

        .variable_declaration => |payload| {
            const declarations = try subRangeToJsonArray(al, t, payload.declarators);
            const kind = @tagName(payload.kind);

            try o.put("declarations", declarations);
            try o.put("kind", JsonValue{ .string = kind });
        },

        .variable_declarator => |payload| {
            const id = try nodeToEstree(t, al, payload.lhs);

            try o.put("id", id);
            if (payload.init) |init| {
                const init_ = try nodeToEstree(t, al, init);
                try o.put("init", init_);
            }
        },

        .member_expr => |payload| {
            const object = try nodeToEstree(t, al, payload.object);
            const property = try nodeToEstree(t, al, payload.property);

            try o.put("object", object);
            try o.put("property", property);
            try o.put("computed", JsonValue{ .bool = false });
        },

        .computed_member_expr => |payload| {
            const object = try nodeToEstree(t, al, payload.object);
            const property = try nodeToEstree(t, al, payload.property);

            try o.put("object", object);
            try o.put("property", property);
            try o.put("computed", JsonValue{ .bool = true });
        },

        .function_expr, .function_declaration => |payload| {
            const params_range = t.getNode(payload.parameters).data.parameters;
            const params = try subRangeToJsonArray(al, t, params_range);
            const body = try nodeToEstree(t, al, payload.body);

            const extra = t.getExtraData(payload.info).function;
            if (extra.name) |name| {
                const id = try nodeToEstree(t, al, name);
                try o.put("id", id);
            }

            try o.put("async", JsonValue{ .bool = extra.flags.is_async });
            try o.put("generator", JsonValue{ .bool = extra.flags.is_generator });
            try o.put("arrow", JsonValue{ .bool = extra.flags.is_arrow });

            try o.put("params", params);
            try o.put("body", body);
        },

        .debugger_statement => {},
        .while_statement => |payload| {
            const cond = try nodeToEstree(t, al, payload.condition);
            const body = try nodeToEstree(t, al, payload.body);

            try o.put("test", cond);
            try o.put("body", body);
        },

        .do_while_statement => |payload| {
            const cond = try nodeToEstree(t, al, payload.condition);
            const body = try nodeToEstree(t, al, payload.body);

            try o.put("test", cond);
            try o.put("body", body);
        },

        .sequence_expr => |payload| {
            const expressions = try subRangeToJsonArray(al, t, payload);
            try o.put("expressions", expressions);
        },

        .assignment_pattern => |payload| {
            const left = try nodeToEstree(t, al, payload.lhs);
            const right = try nodeToEstree(t, al, payload.rhs);

            try o.put("left", left);
            try o.put("right", right);
        },

        .empty_statement => {},
        .labeled_statement => |payload| {
            const label = try nodeToEstree(t, al, payload.label);
            const body = try nodeToEstree(t, al, payload.body);

            try o.put("label", label);
            try o.put("body", body);
        },

        .class_expression, .class_declaration => |payload| {
            const info = t.getExtraData(payload.class_information).class;
            if (info.super_class != .empty) {
                const superClass = try nodeToEstree(t, al, info.super_class);
                try o.put("superClass", superClass);
            }

            if (info.name) |id| {
                const id_ = try nodeToEstree(t, al, id);
                try o.put("id", id_);
            }

            const body_defs = try subRangeToJsonArray(al, t, payload.body);
            var body = std.json.ObjectMap.init(al);
            try body.put("type", JsonValue{ .string = "ClassBody" });
            try body.put("body", body_defs);

            try o.put("body", JsonValue{ .object = body });
        },

        .class_field, .class_method => |payload| {
            const key = try nodeToEstree(t, al, payload.key);
            const value = try nodeToEstree(t, al, payload.value);
            const computed = payload.flags.is_computed;
            const static = payload.flags.is_static;

            try o.put("key", key);
            try o.put("value", value);
            try o.put("computed", JsonValue{ .bool = computed });
            try o.put("static", JsonValue{ .bool = static });

            if (payload.flags.kind != .init) {
                const kind = @tagName(payload.flags.kind);
                try o.put("kind", JsonValue{ .string = kind });
            }
        },

        .template_literal => |payload| {
            const items = payload.asSlice(t);

            var exprs = std.json.Array.init(al);
            var quasis = std.json.Array.init(al);

            for (0.., items) |i, item| {
                if (i % 2 == 0) {
                    try quasis.append(try nodeToEstree(t, al, item));
                } else {
                    try exprs.append(try nodeToEstree(t, al, item));
                }
            }

            try o.put("quasis", JsonValue{ .array = quasis });
            try o.put("expressions", JsonValue{ .array = exprs });
        },

        .template_element => |token_id| {
            const value = t.getToken(token_id).toByteSlice(t.source);
            try o.put("value", JsonValue{ .string = value });
            const is_tail = value[0] == '{';
            try o.put("tail", JsonValue{ .bool = is_tail });
        },

        .throw_statement => |payload| {
            const argument = try nodeToEstree(t, al, payload);
            try o.put("argument", argument);
        },

        .break_statement, .continue_statement => |payload| {
            if (payload.label) |label| {
                const label_ = try nodeToEstree(t, al, label);
                try o.put("label", label_);
            }
        },

        .for_statement => |payload| {
            const iter = t.getExtraData(payload.iterator).for_iterator;
            if (iter.init != .empty)
                try o.put("init", try nodeToEstree(t, al, iter.init));
            if (iter.condition != .empty)
                try o.put("test", try nodeToEstree(t, al, iter.condition));
            if (iter.update != .empty)
                try o.put("update", try nodeToEstree(t, al, iter.update));

            const body = try nodeToEstree(t, al, payload.body);
            try o.put("body", body);
        },

        .for_in_statement,
        .for_of_statement,
        => |payload| {
            const iter = t.getExtraData(payload.iterator).for_in_of_iterator;
            const left = try nodeToEstree(t, al, iter.left);
            const right = try nodeToEstree(t, al, iter.right);

            const body = try nodeToEstree(t, al, payload.body);
            try o.put("left", left);
            try o.put("right", right);
            try o.put("body", body);
        },

        .return_statement => |payload| {
            if (payload) |arg| {
                const argument = try nodeToEstree(t, al, arg);
                try o.put("argument", argument);
            }
        },

        .conditional_expr => |payload| {
            const cond = try nodeToEstree(t, al, payload.condition);
            const consequent = try nodeToEstree(t, al, payload.consequent);
            const alternate = try nodeToEstree(t, al, payload.alternate);

            try o.put("test", cond);
            try o.put("consequent", consequent);
            try o.put("alternate", alternate);
        },

        .meta_property => |payload| {
            const meta = try nodeToEstree(t, al, payload.meta);
            const property = try nodeToEstree(t, al, payload.property);

            try o.put("meta", meta);
            try o.put("property", property);
        },

        .tagged_template_expr => |payload| {
            const pl_tag = try nodeToEstree(t, al, payload.tag);
            const quasi = try nodeToEstree(t, al, payload.template);

            try o.put("tag", pl_tag);
            try o.put("quasi", quasi);
        },

        // this should be unreachable
        .parameters, .none, .arguments => {
            std.debug.panic("Bad call to nodeToEstree: parameters", .{});
        },
    }

    return JsonValue{ .object = o };
}

pub fn toJsonString(
    allocator: std.mem.Allocator,
    t: *const Tree,
    node: ast.Node.Index,
) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const al = arena.allocator();
    defer arena.deinit();

    const pretty_node = try nodeToEstree(t, al, node);
    return try std.json.stringifyAlloc(allocator, pretty_node, .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });
}
