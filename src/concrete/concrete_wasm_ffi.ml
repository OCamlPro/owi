(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Choice = Concrete_choice
module Memory = Concrete_memory
module Value = Concrete_value

type extern_func = Concrete.Extern_func.extern_func

open Concrete_value

let syscall_faccessat (_ : int32) (_ : int32) (_ : int32) (_ : int32) :
  int32 Choice.t =
  assert false

let emscripten_date_now () : float64 Choice.t = assert false

let emscripten_get_now_is_monotonic () : int32 Choice.t = assert false

let emscripten_get_now () : float64 Choice.t = assert false

let syscall_fchmod (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_chmod (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_fchown32 (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let syscall_fcntl64 (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let syscall_openat (_ : int32) (_ : int32) (_ : int32) (_ : int32) :
  int32 Choice.t =
  assert false

let syscall_ioctl (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let syscall_fstat64 (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_stat64 (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_newfstatat (_ : int32) (_ : int32) (_ : int32) (_ : int32) :
  int32 Choice.t =
  assert false

let syscall_lstat64 (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_ftruncate64 (_ : int32) (_ : int64) : int32 Choice.t = assert false

let syscall_getcwd (_ : int32) (_ : int32) : int32 Choice.t = assert false

let syscall_mkdirat (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let tzset_js (_ : int32) (_ : int32) (_ : int32) (_ : int32) : unit Choice.t =
  assert false

let localtime_js (_ : int64) (_ : int32) : unit Choice.t = assert false

let munmap_js (_ : int32) (_ : int32) (_ : int32) (_ : int32) (_ : int32)
  (_ : int64) : int32 Choice.t =
  assert false

let mmap_js (_ : int32) (_ : int32) (_ : int32) (_ : int32) (_ : int64)
  (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let syscall_readlinkat (_ : int32) (_ : int32) (_ : int32) (_ : int32) :
  int32 Choice.t =
  assert false

let syscall_rmdir (_ : int32) : int32 Choice.t = assert false

let syscall_unlinkat (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let syscall_utimensat (_ : int32) (_ : int32) (_ : int32) (_ : int32) :
  int32 Choice.t =
  assert false

let emscripten_resize_heap (_ : int32) : int32 Choice.t = assert false

let env_extern_module =
  let functions =
    [ ( "__syscall_faccessat"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , syscall_faccessat ) )
    ; ( "emscripten_date_now"
      , Concrete.Extern_func.Extern_func
          (Func (UArg Res, R1 F64), emscripten_date_now) )
    ; ( "_emscripten_get_now_is_monotonic"
      , Concrete.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), emscripten_get_now_is_monotonic) )
    ; ( "emscripten_get_now"
      , Concrete.Extern_func.Extern_func
          (Func (UArg Res, R1 F64), emscripten_get_now) )
    ; ( "__syscall_fchmod"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_fchmod) )
    ; ( "__syscall_chmod"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_chmod) )
    ; ( "__syscall_fchown32"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Res))), R1 I32), syscall_fchown32)
      )
    ; ( "__syscall_fcntl64"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Res))), R1 I32), syscall_fcntl64)
      )
    ; ( "__syscall_openat"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , syscall_openat ) )
    ; ( "__syscall_ioctl"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Res))), R1 I32), syscall_ioctl)
      )
    ; ( "__syscall_fstat64"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_fstat64) )
    ; ( "__syscall_stat64"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_stat64) )
    ; ( "__syscall_newfstatat"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , syscall_newfstatat ) )
    ; ( "__syscall_lstat64"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_lstat64) )
    ; ( "__syscall_ftruncate64"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I64, Res)), R1 I32), syscall_ftruncate64) )
    ; ( "__syscall_getcwd"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), syscall_getcwd) )
    ; ( "__syscall_mkdirat"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Res))), R1 I32), syscall_mkdirat)
      )
    ; ( "_tzset_js"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R0), tzset_js)
      )
    ; ( "_localtime_js"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I64, Arg (I32, Res)), R0), localtime_js) )
    ; ( "_munmap_js"
      , Concrete.Extern_func.Extern_func
          ( Func
              ( Arg
                  ( I32
                  , Arg (I32, Arg (I32, Arg (I32, Arg (I32, Arg (I64, Res)))))
                  )
              , R1 I32 )
          , munmap_js ) )
    ; ( "_mmap_js"
      , Concrete.Extern_func.Extern_func
          ( Func
              ( Arg
                  ( I32
                  , Arg
                      ( I32
                      , Arg
                          (I32, Arg (I32, Arg (I64, Arg (I32, Arg (I32, Res)))))
                      ) )
              , R1 I32 )
          , mmap_js ) )
    ; ( "__syscall_readlinkat"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , syscall_readlinkat ) )
    ; ( "__syscall_rmdir"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), syscall_rmdir) )
    ; ( "__syscall_unlinkat"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Arg (I32, Res))), R1 I32), syscall_unlinkat)
      )
    ; ( "__syscall_utimensat"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , syscall_utimensat ) )
    ; ( "emscripten_resize_heap"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), emscripten_resize_heap) )
    ]
  in
  { Link.functions }

let fd_close (_ : int32) : int32 Choice.t = assert false

let fd_write (_ : int32) (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let fd_read (_ : int32) (_ : int32) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let fd_sync (_ : int32) : int32 Choice.t = assert false

let environ_sizes_get (_ : int32) (_ : int32) : int32 Choice.t = assert false

let environ_get (_ : int32) (_ : int32) : int32 Choice.t = assert false

let fd_seek (_ : int32) (_ : int64) (_ : int32) (_ : int32) : int32 Choice.t =
  assert false

let fd_fdstat_get (_ : int32) (_ : int32) : int32 Choice.t = assert false

let wasi_snapshot_preview1_extern_module =
  let functions =
    [ ( "fd_close"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), fd_close) )
    ; ( "fd_write"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , fd_write ) )
    ; ( "fd_read"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I32, Arg (I32, Arg (I32, Res)))), R1 I32)
          , fd_read ) )
    ; ( "fd_sync"
      , Concrete.Extern_func.Extern_func (Func (Arg (I32, Res), R1 I32), fd_sync)
      )
    ; ( "environ_sizes_get"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), environ_sizes_get) )
    ; ( "environ_get"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), environ_get) )
    ; ( "fd_seek"
      , Concrete.Extern_func.Extern_func
          ( Func (Arg (I32, Arg (I64, Arg (I32, Arg (I32, Res)))), R1 I32)
          , fd_seek ) )
    ; ( "fd_fdstat_get"
      , Concrete.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), fd_fdstat_get) )
    ]
  in
  { Link.functions }
