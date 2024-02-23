type ('a, 'conf) cond = 'conf -> 'a option

type fuzz_conf =
  | Symbolic
  | Concrete

type 'a condgen = ('a, fuzz_conf) cond

let return a _c = Some a

let bind (cond : _ cond) (fcond : _ -> _ cond) : _ cond =
 fun c -> Option.bind (cond c) (fun ag -> fcond ag c)

let ( let* ) = bind

let run (cond : _ cond) conf : 'a option = cond conf

let lift1 f ma =
  let* ma in
  return @@ f ma

let ( let+ ) ma f = lift1 f ma

let flatten (condl : _ cond list) conf =
  match condl with
  | [] -> Some []
  | l -> (
    let l = List.filter_map (fun ma -> ma conf) l in
    match l with [] -> None | _ -> Some l )

let ( *@* ) (l1 : _ cond) (l2 : _ cond) : _ cond =
 fun conf ->
  let l1 = l1 conf in
  let l2 = l2 conf in
  match (l1, l2) with
  | _, None -> l1
  | None, _ -> l2
  | Some l1, Some l2 -> Some (l1 @ l2)

let ( *@ ) (l1 : _ cond) l2 conf =
  let l1 = l1 conf in
  match l1 with None -> Some l2 | Some l1 -> Some (l1 @ l2)

let ( **::** ) (cond : _ cond) (lcond : _ cond) : _ cond =
 fun conf ->
  let c = cond conf in
  let lc = lcond conf in
  match (c, lc) with
  | Some c, None -> Some [ c ]
  | None, (_ as lc) -> lc
  | Some c, Some lc -> Some (c :: lc)

let ( **:: ) (cond : _ cond) l : _ cond =
 fun conf ->
  let c = cond conf in
  match c with None -> Some l | Some c -> Some (c :: l)

let whn pred v c = if pred c then v c else None

let not_symbolic v = whn (fun c -> c != Symbolic) (return v)

type 'conf configurator = { configure : 'a. ('a, 'conf) cond -> 'a }

let with_unwrapping_configurator (f : 'conf configurator -> _) : _ cond =
 fun (conf : 'conf) ->
  Some (f { configure = (fun mx -> Option.get (run mx conf)) })
