# Benchmarks for Owi

## Installing dependencies

```shell-session
$ sudo apt install libcairo2-dev
$ git clone git@github.com:thierry-f-78/pie.git
$ cd pie
$ sudo make install DESTDIR=/usr/local/
```

## Running testcomp

### Usage:

```shell-session
$ git submodule update --init --depth 1
$ dune exec -- testcomp/testcomp.exe
```

You can additionally specify a timeout:

```shell-session
$ dune exec -- testcomp/testcomp.exe 5 # timeout of 5 seconds
```

A folder `testcomp-results-YYYY-MM-DD_HHhMMhSSs` has been created with a lot of output. It contains `results-report/index.html` which is the recommended way to visualize the results.

## Generate the report by hand

### Running on some benchmark output

```shell-session
$ dune exec -- report/bin/report.exe testcomp/results # this is an example of an old run, use the correct file instead
```

A folder `results-report` should be available in the working directory with the `index.html` file that contains the results.
