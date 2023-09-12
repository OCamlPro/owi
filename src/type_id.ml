(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type ('a, 'b) eq = Eq : ('a, 'a) eq

type _ externref_ty = ..

type 'a ty =
  { name : string
  ; witness : 'a externref_ty
  ; test : 'ty. 'ty externref_ty -> ('a, 'ty) eq option
  }

let fresh (type t) name : t ty =
  let module M = struct
    type _ externref_ty += T : t externref_ty
  end in
  let test (type a) (witness : a externref_ty) : (t, a) eq option =
    match witness with M.T -> Some Eq | _ -> None
  in
  { name; test; witness = M.T }

let name { name; _ } = name

let eq a b = a.test b.witness
