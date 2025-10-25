// Generates Jam fmt IR from JS/JSX/TS source code.
const std = @import("std");
const ir = @import("./ir.zig");
const js = @import("js");
const ast = js.Ast;

const Doc = ir.Doc;
const Allocator = std.mem.Allocator;

const Self = @This();

allocator: Allocator,
docs: std.ArrayList(Doc),
tree: *const ast.Tree,

pub fn init(allocator: Allocator, tree: *const ast.Tree) Allocator.Error!Self {
    var docs: std.ArrayList(Doc) = .{};
    try docs.ensureTotalCapacity(allocator, 256);
    return Self{
        .allocator = allocator,
        .docs = docs,
        .tree = tree,
    };
}

pub fn deinit(self: *Self) void {
    self.docs.deinit(self.allocator);
}

// At comptime, initialize a table that maps a node tag to
// the function that should be used to visit that node.
const NodeKind = std.meta.Tag(ast.NodeData);
const node_kinds = std.meta.tags(NodeKind);

// NOTE: This works based on the assumption that `@intFromEnum(tag)`
// doesn't change for two subsequent calls to `std.meta.FieldEnum`.
// From what I can tell, @typeInfo is guaranteed to memoize `type -> std.meta.Type`
// after 0.13.0, so this should be fine.
const VisitorFn = fn (*Self, *const ast.NodeData) Doc.Id;
const visitor_of_node: [node_kinds.len]*const VisitorFn = blk: {
    @setEvalBranchQuota(10_000);
    var visitors: [node_kinds.len]*const VisitorFn = undefined; // intialized below

    for (node_kinds) |tag| {
        const tag_name = @tagName(tag);

        // convert snake_case to camelCase
        const camel_case_name = blk_name: {
            var camel_case: [tag_name.len]u8 = undefined; // intialized below

            var len = 0;
            var capitalize_next: bool = false;
            for (tag_name) |ch| {
                if (ch == '_') {
                    capitalize_next = true;
                    continue;
                }

                if (capitalize_next) {
                    capitalize_next = false;
                    camel_case[len] = std.ascii.toUpper(ch);
                } else {
                    camel_case[len] = ch;
                }

                len += 1;
            }

            break :blk_name camel_case[0..len];
        };

        if (!@hasDecl(Self, camel_case_name)) {
            @compileError("Missing implementation for " ++ tag_name ++ " in formatter");
        }

        const func = @field(Self, camel_case_name);
        visitors[@intFromEnum(tag)] = &func;
    }

    break :blk visitors;
};

fn visit(self: *Self, node_id: ast.Node.Index) Doc.Id {
    const data = self.tree.nodeData(node_id);
    const visitFn = visitor_of_node[@intFromEnum(std.meta.activeTag(data.*))];
    return visitFn(self, data);
}

fn format(self: *Self) void {
    _ = self.visit(self.tree.root);
    // std.debug.print("{d}\n", .{x});
}

pub fn program(_: *Self, _: *const ast.NodeData) Doc.Id {
    return @enumFromInt(42);
}
pub fn assignmentExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn binaryExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn memberExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn computedMemberExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn taggedTemplateExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn metaProperty(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn arguments(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn newExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn callExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn superCallExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn super(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn optionalExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn functionExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn postUnaryExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn unaryExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn awaitExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn yieldExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn updateExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn identifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn identifierReference(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn bindingIdentifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn numberLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn stringLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn regexLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn booleanLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn nullLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn this(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn emptyArrayItem(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn arrayLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn arrayPattern(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn spreadElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn restElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn objectLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn objectProperty(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn shorthandProperty(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn classExpression(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn classField(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn classMethod(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn sequenceExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn conditionalExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn templateLiteral(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn templateElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn assignmentPattern(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn objectPattern(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn emptyStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn labeledStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn tryStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn catchClause(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn blockStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn statementList(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn expressionStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn variableDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn variableDeclarator(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn functionDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn classDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn debuggerStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn ifStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn doWhileStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn whileStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn withStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn throwStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn forStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn forInOfIterator(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn forIterator(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn forOfStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn forInStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn switchStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn switchCase(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn defaultCase(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn breakStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn continueStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn parameters(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn returnStatement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn importDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn importDefaultSpecifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn importSpecifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn importNamespaceSpecifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn exportDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn exportSpecifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn exportListDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn exportFromDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn exportAllDeclaration(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn parenthesizedExpr(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn functionMeta(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn classMeta(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn none(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxSpreadChild(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxExpression(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxFragment(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxText(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxOpeningElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxClosingElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxSelfClosingElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxElement(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxMemberExpression(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxNamespacedName(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxIdentifier(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}
pub fn jsxIdentifierReference(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxChildren(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxAttribute(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

pub fn jsxSpreadAttribute(_: *Self, _: *const ast.NodeData) Doc.Id {
    unreachable;
}

const t = std.testing;

fn parse(s: []const u8) !js.Parser.Result {
    var parser = try js.Parser.init(t.allocator, s, .{});
    defer parser.deinit();

    return try parser.parse();
}

test Self {
    var p = try parse("1 + 1;");
    defer p.deinit();

    var fmt = try Self.init(t.allocator, p.tree);
    fmt.format();
    defer fmt.deinit();
}
