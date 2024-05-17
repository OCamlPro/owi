# Benchmarks for Owi

## Testcomp

### Usage:

```shell-session
$ git submodule update --init --depth 1
$ dune exec -- testcomp/testcomp.exe
```

You can additionally specify a timeout:

```shell-session
$ dune exec -- testcomp/testcomp.exe 5 # timeout of 5 seconds
```

### Generate the report:

```shell-session
$ dune exec -- testcomp/report.exe testcomp/stdout # this is an example of an old run, use the correct file instead
```
