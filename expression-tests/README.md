## Handwritten expression parsing tests

For a more complete testing, we use the ECMAScript tests262.

The files in this directory are handwritten tests for *expressions only*.
The ASTs have been manually verified by comparing the output with parsers like ESPree and Meriyah.

Every file has a comment in the first line that describes the expected output.
The second line is an expression that is parsed by the Jam parser.
Once stringified, the output should match the AST in the comment.

See: the test block in src/syntax/parser.zig.
