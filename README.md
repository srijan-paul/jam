# Jam

A high-performance JavaScript toolchain built from the ground up.
**Work in progress!**

## Goals

- 100% Spec compliant (currently: 70% of ECMAScript-262)
- Faster than (or close to) existing tools. 
- Low memory footprint.
- Support JS, TS, JSX, TSX, out of the box.
- Support data flow analysis and call-graphs with an accessible API. 
- API for writing linting rules in Zig.
- Expose a capable parsing and scoping library, such that a bundler, minifier, etc., can be built on top of it.
- Custom JS plugins.
    (I plan to embed the [Kiesel](https://kiesel.dev) JS engine).

## Local development

I've tried to keep the development process hassle-free.
You need only a Zig compiler (and optionally an environment variable) to get going.

If you still face any issues, feel free to open an issue
or reach out to me on discord (`@injuly.`), twitter (`@ptrCast`), or e-mail (`srijan@injuly.in`).
I usually respond within a day.

> **NOTE:** If you're willing to contribute, It's a good idea to copy the contents of ./pre-commit to 
your `./.git/hooks/pre-commit`.
> This will ensure you didn't break any existing functionality before letting you commit changes.

### Basic setup

1. Ensure you have a `master` build of the zig compiler –
to seamlessly switch between multiple zig versions, I recommend using [zvm](https://github.com/tristanisham/zvm).
2. Clone this project into your development machine.
3. Run `zig build run -- <path-to-file.js>` to see a parsed AST for the given file.
4. Run `zig build test` to run the unit tests.

### Checking ECMAScript conformance

To avoid regressions and keep track of spec compliance, we use a `results.json` file,
the format for which is further explained in the [tests262 runner's README](./tools/README.md).

You'll need to set the `JAM_TESTS_262_DIR` environment variable to the path of a cloned [tc39/test262-parser-tests](https://github.com/tc39/test262-parser-tests) repository:

```sh
git clone --depth 1 https://github.com/tc39/test262-parser-tests.git /tmp/test262-parser-tests
# `tools/ec262-tests` can find the tests if you set the environment variable.
export JAM_TESTS_262_DIR=/tmp/test262-parser-tests
```

To compare your changes with the existing test results, run `zig build test262 -- compare ./tools/results.json`.
If it exits with a zero status code, you didn't break anything!

To update the test results, run `zig build test262 > ./tools/results.json`.


Currently, the format of the `results.json` file is roughly as follows:

```js
{
  // % of test files that either: a) parsed incorrectly, or b) failed to parse.
  "fail_percent": 35.703479576399396,
  // % of test files that were parsed correctly.
  "pass_percent": 64.2965204236006,
  // number of test files that parsed but had an incorrect syntax tree.
  "unmatching_ast_count": 17,
  // results for individual test cases:
  "test_cases": {
    "2db5219f0ac5dd71.js": "parse_error",
    "c532e126a986c1d4.js": "pass",
    "d532e126a986c1d4.js": "ast_no_match",
    // ...goes on for a few thousand lines...
```
 
