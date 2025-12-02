(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Choice = Concrete_choice
module Memory = Concrete_memory
module Value = Concrete_value

type extern_func = Concrete_extern_func.extern_func

open Concrete_value

let syscall_faccessat (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let emscripten_date_now () : f64 Choice.t = assert false

let emscripten_get_now_is_monotonic () : i32 Choice.t = assert false

let emscripten_get_now () : f64 Choice.t = assert false

let syscall_fchmod (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_chmod (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_fchown32 (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_fcntl64 (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_openat (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let syscall_ioctl (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_fstat64 (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_stat64 (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_newfstatat (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let syscall_lstat64 (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_ftruncate64 (_ : i32) (_ : i64) : i32 Choice.t = assert false

let syscall_getcwd (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_mkdirat (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t = assert false

let tzset_js (_ : i32) (_ : i32) (_ : i32) (_ : i32) : unit Choice.t =
  assert false

let localtime_js (_ : i64) (_ : i32) : unit Choice.t = assert false

let munmap_js (_ : i32) (_ : i32) (_ : i32) (_ : i32) (_ : i32) (_ : i64) :
  i32 Choice.t =
  assert false

let mmap_js (_ : i32) (_ : i32) (_ : i32) (_ : i32) (_ : i64) (_ : i32) (_ : i32)
  : i32 Choice.t =
  assert false

let syscall_readlinkat (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let syscall_rmdir (_ : i32) : i32 Choice.t = assert false

let syscall_unlinkat (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t = assert false

let syscall_utimensat (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let emscripten_resize_heap (_ : i32) : i32 Choice.t = assert false

let env_extern_module =
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ( "__syscall_faccessat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, syscall_faccessat) )
    ; ("emscripten_date_now", Extern_func (unit ^->. f64, emscripten_date_now))
    ; ( "_emscripten_get_now_is_monotonic"
      , Extern_func (unit ^->. i32, emscripten_get_now_is_monotonic) )
    ; ("emscripten_get_now", Extern_func (unit ^->. f64, emscripten_get_now))
    ; ("__syscall_fchmod", Extern_func (i32 ^-> i32 ^->. i32, syscall_fchmod))
    ; ("__syscall_chmod", Extern_func (i32 ^-> i32 ^->. i32, syscall_chmod))
    ; ( "__syscall_fchown32"
      , Extern_func (i32 ^-> i32 ^-> i32 ^->. i32, syscall_fchown32) )
    ; ( "__syscall_fcntl64"
      , Extern_func (i32 ^-> i32 ^-> i32 ^->. i32, syscall_fcntl64) )
    ; ( "__syscall_openat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, syscall_openat) )
    ; ( "__syscall_ioctl"
      , Extern_func (i32 ^-> i32 ^-> i32 ^->. i32, syscall_ioctl) )
    ; ("__syscall_fstat64", Extern_func (i32 ^-> i32 ^->. i32, syscall_fstat64))
    ; ("__syscall_stat64", Extern_func (i32 ^-> i32 ^->. i32, syscall_stat64))
    ; ( "__syscall_newfstatat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, syscall_newfstatat)
      )
    ; ("__syscall_lstat64", Extern_func (i32 ^-> i32 ^->. i32, syscall_lstat64))
    ; ( "__syscall_ftruncate64"
      , Extern_func (i32 ^-> i64 ^->. i32, syscall_ftruncate64) )
    ; ("__syscall_getcwd", Extern_func (i32 ^-> i32 ^->. i32, syscall_getcwd))
    ; ( "__syscall_mkdirat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^->. i32, syscall_mkdirat) )
    ; ( "_tzset_js"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. unit, tzset_js) )
    ; ("_localtime_js", Extern_func (i64 ^-> i32 ^->. unit, localtime_js))
    ; ( "_munmap_js"
      , Extern_func
          (i32 ^-> i32 ^-> i32 ^-> i32 ^-> i32 ^-> i64 ^->. i32, munmap_js) )
    ; ( "_mmap_js"
      , Extern_func
          (i32 ^-> i32 ^-> i32 ^-> i32 ^-> i64 ^-> i32 ^-> i32 ^->. i32, mmap_js)
      )
    ; ( "__syscall_readlinkat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, syscall_readlinkat)
      )
    ; ("__syscall_rmdir", Extern_func (i32 ^->. i32, syscall_rmdir))
    ; ( "__syscall_unlinkat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^->. i32, syscall_unlinkat) )
    ; ( "__syscall_utimensat"
      , Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, syscall_utimensat) )
    ; ( "emscripten_resize_heap"
      , Extern_func (i32 ^->. i32, emscripten_resize_heap) )
    ]
  in
  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }

let fd_close (_ : i32) : i32 Choice.t = assert false

let fd_write (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let fd_read (_ : i32) (_ : i32) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let fd_sync (_ : i32) : i32 Choice.t = assert false

let environ_sizes_get (_ : i32) (_ : i32) : i32 Choice.t = assert false

let environ_get (_ : i32) (_ : i32) : i32 Choice.t = assert false

let fd_seek (_ : i32) (_ : i64) (_ : i32) (_ : i32) : i32 Choice.t =
  assert false

let fd_fdstat_get (_ : i32) (_ : i32) : i32 Choice.t = assert false

let wasi_snapshot_preview1_extern_module =
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("fd_close", Extern_func (i32 ^->. i32, fd_close))
    ; ("fd_write", Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, fd_write))
    ; ("fd_read", Extern_func (i32 ^-> i32 ^-> i32 ^-> i32 ^->. i32, fd_read))
    ; ("fd_sync", Extern_func (i32 ^->. i32, fd_sync))
    ; ( "environ_sizes_get"
      , Extern_func (i32 ^-> i32 ^->. i32, environ_sizes_get) )
    ; ("environ_get", Extern_func (i32 ^-> i32 ^->. i32, environ_get))
    ; ("fd_seek", Extern_func (i32 ^-> i64 ^-> i32 ^-> i32 ^->. i32, fd_seek))
    ; ("fd_fdstat_get", Extern_func (i32 ^-> i32 ^->. i32, fd_fdstat_get))
    ]
  in
  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }
