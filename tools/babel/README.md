# Babel parser test cases

These tests are taken from the `@babel/parser` test suite.
The `index.mjs` script is used to re-run the parser and convert every test case into an
ESTree compatible JSON object.

These JSON serialized ASTs are then compared against the ASTs generated by Jam's parser
to ensure that our parser works as expected.

