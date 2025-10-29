# Code Coverage Criteria

When we write tests for a codebase, it is common to wonder how good is the test-suite. One way to measure it is through *code coverage*. That is: how much how the code base is covered by the tests? Most of the time, this notion is not precise. Most tools measuring code coverage can give you a percentage of the coverage, without a clear definition.

Yet there exists some precise definitions for various ways of measuring code coverage. They are called *code coverage criteria*.

## Function Coverage

The easiest one is called *function coverage* (FC). It measure the percentage of the functions of your programs that are called by the test-suite. For instance, if your whole code base has only two functions, and in your test-suite, only one of them gets execution, then it means you have a function coverage of 50%.

## Statement Coverage

This criteria (SC) measure the percentage of instructions of your program that are executed.

## Decision Coverage

This criteria (DC) measure the percentage of decisions covered for you test-suite. For instance, if you have some code looking like the following:

```c
void f(int x) {
  if (x) {
    // A
  } else {
    // B
  }
}
```

When the conditional is reached, there are two possible decisions. If `x` is `true`, then, we execute `A`. If `x` is false, then, we execute `B`.

## Condition Coverage

This criteria (CC) ... TODO

## TODO

All the others (MC/DC)

## TODO: explain that there is a correspondence with the CFG (covering nodes versus covering edges and such)

## References

TODO
