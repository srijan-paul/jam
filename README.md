# Jam

A JavaScript toolchain built from the ground up.
**Work in progress!**

To see how we test with the [tc39/test262-parser-tests](https://github.com/tc39/test262-parser-tests),
see the [tests262 runner](./tools/README.md).

## Goals

- 100% Spec compliant
- Faster than (or close to) existing tools. 
- Low memory footprint.
- Support JS, TS, JSX, TSX out of the box.
- Support data flow analysis and call-graphs with an accessible API. 
- API for writing linting rules in Zig.
- Custom JS plugins.
    (I plan to embed the [Kiesel](https://kiesel.dev) JS engine).

