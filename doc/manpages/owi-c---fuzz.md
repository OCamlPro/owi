[owi](owi.md) › [owi c++](owi-c--.md) › **owi c++ fuzz**

# owi c++ fuzz

## Help

```text
NAME
       owi-c++-fuzz - Run the fuzzer.

SYNOPSIS
       owi c++ fuzz [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --entry-point=FUNCTION (absent=main)
           entry point of the executable

       -I VALUE
           headers path

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

       -O VAL (absent=3)
           specify which optimization level to use

       --rounds=I
           Stop after a number of fuzzing rounds.

       --seed=I
           Initial seed for the PRNG state

       --timeout=S
           Stop execution after S seconds.

       --timeout-instr=I
           Stop execution after running I instructions.

       -u, --unsafe
           skip typechecking pass

       --workspace=DIR
           write results and intermediate compilation artifacts to dir

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi c++ fuzz exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
