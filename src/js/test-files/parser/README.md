## Handwritten expression parsing tests

> [!IMPORTANT]
> For more complete tests of the grammar, we use the ECMAScript tests262.
> This test suite is for some rough manual testing of edge cases.

The tests in this directory are handwritten to verify the shape of a parse tree.
The ASTs have been manually verified by comparing the output with parsers like ESpree and Meriyah.

For every `<file>.js`, there is a corresponding `<file>.json` that contains a stringified version of the expected AST.
The test block in `src/js/parser.zig` will parse these files, and compare the parse tree with the expected output.

Files that do not have a corresponding JSON file start with `// ERROR`, and are expected to fail parsing.

