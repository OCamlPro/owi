(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let out_testcase ~dst ~err testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag ?(atts = []) name = (("", name), atts) in
  let atts = if err then Some [ (("", "coversError"), "true") ] else None in
  let to_string v = Format.asprintf "%a" Smtml.Value.pp_num v in
  let input v = `El (tag "input", [ `Data (to_string v) ]) in
  let testcase = `El (tag ?atts "testcase", List.map input testcase) in
  let dtd =
    {|<!DOCTYPE testcase PUBLIC "+//IDN sosy-lab.org//DTD test-format testcase 1.1//EN" "https://sosy-lab.org/test-format/testcase-1.1.dtd">|}
  in
  Xmlm.output o (`Dtd (Some dtd));
  Xmlm.output_tree Fun.id o testcase

let write_testcase =
  let cnt = ref 0 in
  fun ~dir ~err testcase ->
    incr cnt;
    let name = Format.ksprintf Fpath.v "testcase-%d.xml" !cnt in
    let path = Fpath.append dir name in
    let* res =
      Bos.OS.File.with_oc path
        (fun chan () -> Ok (out_testcase ~dst:(`Channel chan) ~err testcase))
        ()
    in
    res
