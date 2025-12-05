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
      let out = Format.sprintf "%s.o" filename in
      let n =
        Format.ksprintf Sys.command
          "clang -O3 -ffreestanding --no-standard-libraries \
           --target=wasm32-unknown-unknown -c -m32 -Iinclude \
           -I../libc/include/ -Wall -Werror -Wno-int-conversion \
           -Wno-return-type -fbracket-depth=512 \
           -DWANT_STRTOD_WITHOUT_LONG_DOUBLE -o %s %s"
          out c_filename
      in
      if n <> 0 then exit n
    end
    else assert false
  done
