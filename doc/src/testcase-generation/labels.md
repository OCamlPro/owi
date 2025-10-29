# Labels

There exists many code coverage criteria. Having to implement a different mechanism for all of them is tedious. This is a problem solved by `labels`.

Labels are annotations added to a program (often via an instrumentation pass). Each label correspond to a point that must be reached by the test-suite. The percentage of labels reached by the test-suite is the percentage of code coverage for the criteria that was chosen when adding labels.

## Example

For instance, if we are interested in the following program:

```c
void f(void) {
  // A
}

void g(void) {
  // B
}
```

We could instrument the program to add labels for the FC criteria this way:

```c
void f(void) {
  label_reached(0);
  // A
}

void g(void) {
  label_reached(1);
  // B
}
```

Then, by providing an appropriate definition of `label_reached` and knowing how many of them are in the program, we can count how many of them are reached by the test-suite by running it on the instrumented program. Then, we can compute the percentage of code coverage for the FC criteria.

## Labels in Owi

Owi has the ability to :

1. Annotate a Wasm program with labels for a few criteria (see the `owi instrument label` sub-command).
2. Generate tests for annotated programs in order to automatically get a high code coverage percentage (run your program with `owi sym instrumented.wasm`.

TODO: complete example with more details on test-case generation
