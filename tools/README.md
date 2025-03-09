# EC262 and babel tests runner

The `ec262-tests.zig` script runs the jam parser against `tc39/test262-parser-tests` test suite,
and reports the results in a JSON format.
These results are stored in `results.json`, and the CI script ensures that we do not regress on these tests.
We track three metrics:
  1. Percentage of files that parse correctly.
  2. Percentage of files that fail to parse with an error, or incorrect parse tree. 
  3. Number of files that parse without an error, but have an incorrect parse tree.

For more information about how the tests262 test suite works, see [its README](https://github.com/tc39/test262-parser-tests).

Similarly, the `babel-tests.zig` script runs (relevant parts of) babel's tests and
compares results with Jam's parser, writing results to the `babel-results.json` file.

## Running the EC262 tests

1. Clone the [tc39/test262-parser-tests](https://github.com/tc39/test262-parser-tests) repository.
2. Set the `JAM_TESTS_262_DIR` environment variable to the cloned directory's path.
3. Run `zig build test262` to see the results on your terminal.
4. Run `zig build test262 > ./tools/results.json` to update the results file. 

## Running the babel tests

1. Run `zig build test-babel`


