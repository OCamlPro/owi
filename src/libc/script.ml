(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let () =
  if Array.length Sys.argv < 2 then
    Format.ksprintf failwith "usage: %s <C files>" Sys.argv.(0)

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let c_filename = Sys.argv.(i) in
    if Filename.extension c_filename = ".c" then begin
      let filename = Filename.basename c_filename |> Filename.chop_extension in
      let bc_filename = Format.sprintf "%s.bc" filename in
      let n =
        Format.ksprintf Sys.command
          "clang -emit-llvm -g -O3 -ffreestanding --target=wasm32 -c -m32 \
           -Iinclude -Wno-parentheses-equality -Wall -Wno-attributes \
           -Wno-return-type -Wno-int-conversion \
           -Wno-incompatible-pointer-types \
           -Wno-incompatible-function-pointer-types -Wno-pointer-sign \
           -Wno-bitfield-constant-conversion \
           -Wno-implicit-function-declaration -fbracket-depth=512 -o %s %s"
          bc_filename c_filename
      in
      if n <> 0 then exit n;
      let n =
        Format.ksprintf Sys.command "opt -O3 %s -o %s" bc_filename bc_filename
      in
      if n <> 0 then exit n;
      let o_filename = Format.sprintf "%s.o" filename in
      let n =
        Format.ksprintf Sys.command
          "llc -O3 -march=wasm32 -filetype=obj %s -o %s" bc_filename o_filename
      in
      if n <> 0 then exit n
    end
  done
