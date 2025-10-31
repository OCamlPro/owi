# `owi c`

```sh
$ owi c --help=plain
NAME
       owi-c - Compile a C file to Wasm and run the symbolic interpreter on
       it

SYNOPSIS
       owi c [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --concolic
           concolic mode

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --e-acsl
           e-acsl mode, refer to
           https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for
           Frama-C's current language feature implementations

       --entry-point=FUNCTION (absent=main)
           entry point of the executable

       --exploration=VAL (absent=lifo)
           exploration strategy to use ("fifo", "lifo" or "random")

       --fail-on-assertion-only
           ignore traps and only report assertion violations

       --fail-on-trap-only
           ignore assertion violations and only report traps

       -I VAL
           headers path

       --invoke-with-symbols
           Invoke the entry point of the program with symbolic values instead
           of dummy constants.

       -m VAL, --arch=VAL (absent=32)
           data model

       --model-format=VAL (absent=scfg)
            The format of the model ("json" or "scfg")

       --model-out-file=FILE
           Output the generated model to FILE. if --no-stop-at-failure is
           given this is used as a prefix and the ouputed files would have
           PREFIX_%d.

       --no-assert-failure-expression-printing
           do not display the expression in the assert failure

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --no-value
           do not display a value for each symbol

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

       -O VAL (absent=3)
           specify which optimization level to use

       --property=FILE
           property file

       --rac
           runtime assertion checking mode

       -s VALUE, --solver=VALUE (absent=Z3)
           SMT solver to use. VALUE must be one of the 5 available solvers:
           Z3, Bitwuzla, Colibri2, cvc5, Alt-Ergo

       --srac
           symbolic runtime assertion checking mode

       --testcomp
           test-comp mode

       -u, --unsafe
           skip typechecking pass

       -w VAL, --workers=VAL (absent=n)
           number of workers for symbolic execution. Defaults to the number
           of physical cores.

       --with-breadcrumbs
           add breadcrumbs to the generated model

       --workspace=DIR
           write results and intermediate compilation artifacts to dir

COMMON OPTIONS
       --bench
           enable benchmarks

       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning or OWI_VERBOSITY env)
           Be more or less verbose. LEVEL must be one of quiet, error,
           warning, info or debug. Takes over -v.

       --version
           Show version information.

EXIT STATUS
       owi c exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi c:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```

