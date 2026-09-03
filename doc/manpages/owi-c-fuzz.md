[owi](owi.md) › [owi c](owi-c.md) › **owi c fuzz**

# owi c fuzz

## Help

```text
NAME
       owi-c-fuzz - Run the fuzzer.

SYNOPSIS
       owi c fuzz [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --e-acsl
           e-acsl mode, refer to
           https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for
           Frama-C's current language feature implementations

       --entry-point=FUNCTION (absent=main)
           entry point of the executable

       -I VALUE
           headers path

       -m INT, --arch=INT (absent=32)
           data model

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

       -O VAL (absent=3)
           specify which optimization level to use

       --property=FILE
           property file

       --rounds=I
           Stop after a number of fuzzing rounds.

       --seed=I
           Initial seed for the PRNG state

       --testcomp
           test-comp mode

       --timeout=S
           Stop execution after S seconds.

       --timeout-instr=I
           Stop execution after running I instructions.

       -u, --unsafe
           skip typechecking pass

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
       owi c fuzz exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi c fuzz:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
