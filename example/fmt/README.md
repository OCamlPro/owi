# Formatter

## Quickstart

Given a file `horrible.wat`:


<!-- $MDX file=horrible.wat -->
```wat
(module (memory
10) (func
$f (param
     $n i32) (result
i32) (if
             (                       i32.lt_s
        (
local.get $n)
        (
                          i32.const
                          
                          
                          0))
    (  then
(
 
 unreachable)))

    (
     if
( 
i32.lt_s
        (local.get                            $n)
        (i32.const                             2))
    (then          (return (local.get $n)))) (if   
      (i32.eqz   
(i32.load (i32.mul (i32.const 4) (local.get $n)))) (then local.get $n i32.const 4 i32.mul
      (call $f (i32.sub (local.get $n) (i32.const 1)))
      (call $f (i32.sub (local.get $n)
(i32.const 2))) i32.add   i32.store )) local.get $n       i32.const 4 i32.mul i32.load return))
```

Owi will format it like this:

```sh
$ dune exec -- owi fmt horrible.wat
(module
  (memory 10)
  (func $f (param $n i32) (result i32)
    local.get $n
    i32.const 0
    i32.lt_s
    (if
      (then
        unreachable
      )
    )
    local.get $n
    i32.const 2
    i32.lt_s
    (if
      (then
        local.get $n
        return
      )
    )
    i32.const 4
    local.get $n
    i32.mul
    i32.load align=1
    i32.eqz
    (if
      (then
        local.get $n
        i32.const 4
        i32.mul
        local.get $n
        i32.const 1
        i32.sub
        call $f
        local.get $n
        i32.const 2
        i32.sub
        call $f
        i32.add
        i32.store align=1
      )
    )
    local.get $n
    i32.const 4
    i32.mul
    i32.load align=1
    return
  )
)
```

Are you able to recognize the program now?

## Man page

```sh
$ dune exec owi -- fmt --help=plain
OWI-FMT(1)                        Owi Manual                        OWI-FMT(1)

NAME
       owi-fmt - Format a .wat or .wast file

SYNOPSIS
       owi fmt [--inplace] [OPTION]… ARG

OPTIONS
       -i, --inplace
           Format in-place, overwriting input file

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show  this  help  in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the  format  is  pager  or  plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi fmt exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                     OWI-FMT(1)
```
