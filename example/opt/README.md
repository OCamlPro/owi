# Optimizer

## Usage

Given the useless program `useless.wat`:

<!-- $MDX file=useless.wat -->
```wat
(module
  (func $i32binop
    i32.const 21
    i32.const 21
    i32.add
    i32.const 0
    drop
    drop

    i32.const 63
    i32.const 21
    i32.sub
    i32.const 0
    drop
    drop

    i32.const 21
    i32.const 2
    i32.mul
    i32.const 0
    drop
    drop

    i32.const 84
    i32.const 2
    i32.div_s
    i32.const 0
    drop
    drop

    i32.const 84
    i32.const 2
    i32.div_u
    i32.const 0
    drop
    drop

    i32.const -42
    i32.const 4
    i32.rem_s     ;; 4*10 + 2 > =-2
    i32.const 0
    drop
    drop

    i32.const -42
    i32.const 4
    i32.rem_u     ;; 4*10 + 2 > =2
    i32.const 0
    drop
    drop

    i32.const 1
    i32.const 3
    i32.and          ;; bitwise ope > =1
    i32.const 0
    drop
    drop

    i32.const 1
    i32.const 3
    i32.or           ;; bitwise ope > =3
    i32.const 0
    drop
    drop

    i32.const 1
    i32.const 3
    i32.xor          ;; bitwise ope > =2
    i32.const 0
    drop
    drop

    i32.const 1
    i32.const 3
    i32.shl          ;; shift left ope > =8
    i32.const 0
    drop
    drop

    i32.const 8
    i32.const 3
    i32.shr_u        ;; shift right ope > =1
    i32.const 0
    drop
    drop

    i32.const 8
    i32.const 3
    i32.shr_s        ;; shift right ope > =1
    i32.const 0
    drop
    drop

    i32.const 1
    i32.const 3
    i32.rotl         ;; rotation left ope > =8
    i32.const 0
    drop
    drop

    i32.const 8
    i32.const 3
    i32.rotr         ;; rotation left ope > =1
    i32.const 0
    drop
    drop
  )

  (func $i64binop
    i64.const 21
    i64.const 21
    i64.add
    i32.const 0
    drop
    drop

    i64.const 63
    i64.const 21
    i64.sub
    i32.const 0
    drop
    drop

    i64.const 21
    i64.const 2
    i64.mul
    i32.const 0
    drop
    drop

    i64.const 84
    i64.const 2
    i64.div_s
    i32.const 0
    drop
    drop

    i64.const 84
    i64.const 2
    i64.div_u
    i32.const 0
    drop
    drop

    i64.const -42
    i64.const 4
    i64.rem_s     ;; 4*10 + 2 > =-2
    i32.const 0
    drop
    drop

    i64.const -42
    i64.const 4
    i64.rem_u     ;; 4*10 + 2 > =2
    i32.const 0
    drop
    drop

    i64.const 1
    i64.const 3
    i64.and          ;; bitwise ope > =1
    i32.const 0
    drop
    drop

    i64.const 1
    i64.const 3
    i64.or           ;; bitwise ope > =3
    i32.const 0
    drop
    drop

    i64.const 1
    i64.const 3
    i64.xor          ;; bitwise ope > =2
    i32.const 0
    drop
    drop

    i64.const 1
    i64.const 3
    i64.shl          ;; shift left ope > =8
    i32.const 0
    drop
    drop

    i64.const 8
    i64.const 3
    i64.shr_u        ;; shift right ope > =1
    i32.const 0
    drop
    drop

    i64.const 8
    i64.const 3
    i64.shr_s        ;; shift right ope > =1
    i32.const 0
    drop
    drop

    i64.const 1
    i64.const 3
    i64.rotl         ;; rotation left ope > =8
    i32.const 0
    drop
    drop

    i64.const 8
    i64.const 3
    i64.rotr         ;; rotation left ope > =1
    i32.const 0
    drop
    drop
  )

  (func $f32binop
    f32.const 21
    f32.const 21
    f32.add
    i32.const 0
    drop
    drop

    f32.const 63
    f32.const 21
    f32.sub
    i32.const 0
    drop
    drop

    f32.const 21
    f32.const 2
    f32.mul
    i32.const 0
    drop
    drop

    f32.const 84
    f32.const 2
    f32.div
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 21
    f32.max
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 84
    f32.min
    i32.const 0
    drop
    drop

    f32.const -42
    f32.const 21
    f32.copysign
    i32.const 0
    drop
    drop
  )

  (func $f64binop
    f64.const 21
    f64.const 21
    f64.add
    i32.const 0
    drop
    drop

    f64.const 63
    f64.const 21
    f64.sub
    i32.const 0
    drop
    drop

    f64.const 21
    f64.const 2
    f64.mul
    i32.const 0
    drop
    drop

    f64.const 84
    f64.const 2
    f64.div
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 21
    f64.max
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 84
    f64.min
    i32.const 0
    drop
    drop

    f64.const -42
    f64.const 21
    f64.copysign
    i32.const 0
    drop
    drop
  )

  (func $start
    call $i32binop
    call $i64binop
    call $f32binop
    call $f64binop
  )

  (start $start)
)
```

Owi is able to get rid of most of the code:

```sh
$ owi opt useless.wat
(module

  (type (sub final  (func)))
  (func $i32binop

  )
  (func $i64binop

  )
  (func $f32binop

  )
  (func $f64binop

  )
  (func $start
    call 0
    call 1
    call 2
    call 3
  )
  (start 4)
)
```

## Man page

```sh
$ owi opt --help=plain
NAME
       owi-opt - Optimize a module

SYNOPSIS
       owi opt [--debug] [--unsafe] [OPTION]… [ARG]…

OPTIONS
       -d, --debug
           debug mode

       -u, --unsafe
           skip typechecking pass

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi opt exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
