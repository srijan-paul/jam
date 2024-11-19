# Jam JavaScript

JavaScript parsing, analysis, and manipulation utilities for Jam.
The AST is described in `ast.zig`, and a recursive descent parser is implemented in `parser.zig`.

## Parser architecture 

Jam uses a recursive descent parser that tries to keep allocations to a minimum.
All AST nodes are stored in a flat array (`std.MultiArrayList(Node)`), and reference each other using indices.

Apart from a few growable buffers—an array of all nodes, an array of all metadata, and some scratch space—
no allocations are made in the parser.

The parser never backtracks, even for [cover productions](https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList) like arrow functions.
For example, consider that a `(a, b)` could either be a parenthesized expression, or a parameter list for an arrow function.
One way to parse this is to consume the `(`, assume a ParenthesizedExpression, then go back in the input string if we see a `=>`
(or if we see an obvious assignment pattern that cannot appear inside `()`, like `{a=1}` or `...foo`).

Jam takes an alternative approach.
The parser maintains some internal state to track whether each individual item inside `()` is also a valid parameter,
and modifies the existing parse-tree if a `=>` is seen after the `)` token.

This approach was inspired by [the Meriyah parser](https://github.com/meriyah/meriyah).

```js
(a, b, c=1, {e}) => a + b + c
//               ^-- only now we know its an arrow function
```

## Lossless syntax trees 

The syntax tree is designed in a way that preserves all information from the source code,
meaning that the source code can be completely re-constructed from the AST, without any loss of information.
Owing to this decision, building source-transformation tools like minifiers and formatters becomes easier.

This design was inspired by [Oilshell](https://www.oilshell.org/).
See: [From AST to Lossless syntax tree](https://www.oilshell.org/blog/2017/02/11.html).

## Storing AST nodes

As mentioned above, all AST nodes are stored in a multi-array to optimize for cache hits:

```zig
const Tree = struct {
    nodes: std.MultiArrayList(Node),
    tokens: std.ArrayList(Token),
    extras: std.ArrayList(ExtraData),
    // ... other properties
}
```

The `Node` struct is defined like so:

```zig
const Node = struct {
    const Index = enum { _ };
    // ...properties
}
```

The `Node.Index` enum is used to reference other nodes in the tree.
For example, a `BinaryExpression` node might look like this:

```zig
const BinaryExpression = struct {
    left: Node.Index,
    right: Node.Index,
    operator: Token.Index,
}
```

The `Token.Index` enum is used to reference tokens in the `Tree.tokens` array,
and `Node.Index` for the `Tree.nodes` array.

If the payload for a node cannot fit in 128 bytes, the remaining data for that node-type
is stored in the `Tree.extras` array, and the `Node` struct only contains a reference to that data
using `ExtraData.Index`.

