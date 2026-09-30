[owi](owi.md) › [owi zig](owi-zig.md) › **owi zig run**

# owi zig run

## Help

```text
NAME
       owi-zig-run - Run the concrete interpreter.

SYNOPSIS
       owi zig run [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --entry-point=FUNCTION (absent=_start)
           entry point of the executable

       -I VALUE
           headers path

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

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
       owi zig run exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
