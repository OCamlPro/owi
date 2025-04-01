# Testing

## Unit tests

Tests are mostly written using [Cram Tests].
The ones that are integrated into documentation are using [MDX].
You can run them as follow:

```shell-session
$ dune runtest
```

If you made some changes and the output of some tests is changing, the diff will be displayed.
If you want to automatically accept the diff as being the new expected output, you can run:

```shell-session
$ dune promote
```

## Code coverage

You can generate the code coverage report with:

```shell-session
BISECT_FILE=$(pwd)/bisect odune runtest --force --instrument-with bisect_ppx
bisect-ppx-report html -o _coverage
xdg-open _coverage/index.html
```

## Fuzzing

See [test/fuzz].

[Cram Tests]: https://dune.readthedocs.io/en/latest/reference/cram.html
[MDX]: https://github.com/realworldocaml/mdx
[test/fuzz]: https://github.com/OCamlPro/owi/tree/main/test/fuzz
