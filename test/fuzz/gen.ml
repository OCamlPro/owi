open Crowbar
open Owi
open Owi.Types.Symbolic

let globals_len l : int gen =       (* TODO : check + chg name *)
  let len = List.length l in
  if len = 0 then const 0 else range (List.length l)

let drop : expr gen = const [ Drop ]

let nop : expr gen = const [ Nop ]

let ibinop : Types.ibinop gen =
  let open Types in
  choose [ const (Add : ibinop); const (Sub : ibinop); const (Mul : ibinop) ]

let num_type : Types.num_type gen =
  let open Types in
  choose [ const I32; const I64; const F32; const F64 ]

let mut : Types.mut gen =
  let open Types in
  choose [ const Const; const Var ]

let val_type : val_type gen =
  choose [ map [ num_type ] (fun nt -> Num_type nt) (* TODO add Ref_type *) ]

let global_type_const_i32 : global_type gen =
  pair mut (map [ const Types.I32 ] (fun nt -> Num_type nt))

let global_type_const_i64 : global_type gen =
  pair mut (map [ const Types.I64 ] (fun nt -> Num_type nt))

let global_type_const_f32 : global_type gen =
  pair mut (map [ const Types.F32 ] (fun nt -> Num_type nt))

let global_type_const_f64 : global_type gen =
  pair mut (map [ const Types.F64 ] (fun nt -> Num_type nt))

let const_i32 : expr gen = map [ int32 ] (fun i -> [ I32_const i ])

let const_i64 : expr gen = map [ int64 ] (fun i -> [ I64_const i ])

(* let const_f32 : instr gen =
     map ([float]) (fun f -> F32_const f)

   let const_f64 : instr gen =
     map ([float]) (fun f -> F64_const f) *)

let global_i32 : expr gen =
  map
    [ globals_len !Env.v.globals_i32; nop ]
    (fun len nope ->
      if List.length !Env.v.globals_i32 = 0 then nope
      else [ Global_get (Symbolic (List.nth !Env.v.globals_i32 len)) ] )

let ibinop_32 : expr gen =
  map [ const Types.S32; ibinop ] (fun c bop -> [ I_binop (c, bop) ])

let ibinop_64 : expr gen =
  map [ const Types.S64; ibinop ] (fun c bop -> [ I_binop (c, bop) ])

let binop_i32 : expr gen =
  map [ const_i32; const_i32; ibinop_32 ] (fun a b op -> a @ b @ op)

let gen_i32 : expr gen = choose [ const_i32; global_i32; binop_i32 ]

let binop_i64 : expr gen =
  map [ const_i64; const_i64; ibinop_64 ] (fun a b op -> a @ b @ op)

let gen_i64 : expr gen = choose [ const_i64; binop_i64 ]

let gen_const : expr gen = choose [ gen_i32; gen_i64 ]

let mglobal_field_const_i32 : module_field gen =
  map [ global_type_const_i32; const_i32 ] (fun gt c ->
    let n = Env.add_global_i32 () in
    MGlobal { type_ = gt; init = c; id = Some n } )

let mglobal_field_const_i64 : module_field gen =
  map [ global_type_const_i64; const_i64 ] (fun gt c ->
    let n = Env.add_global_i64 () in
    MGlobal { type_ = gt; init = c; id = Some n } )

(* let mglobal_field_const_f32 : module_field gen =
     map ([global_type_const_f32; const_f32]) (fun gt c -> MGlobal {type_ = gt; init = [c]; id = None})

   let mglobal_field_const_f64 : module_field gen =
     map ([global_type_const_f64; const_f64]) (fun gt c -> MGlobal {type_ = gt; init = [c]; id = None}) *)

let mglobal_field : module_field gen =
  choose
    [ mglobal_field_const_i32
    ; mglobal_field_const_i64
      (* mglobal_field_const_f32;
         mglobal_field_const_f64; *)
    ]

let mfunc_local : param gen =   (* TODO : same system add_func or add_global *)
  (* let n = Utils.name_local () in *)
  (* let sid = const (Some n) in *)
  let sid = const None in
  pair sid val_type

let mfunc_field : Types.Symbolic.module_field gen =
  let mfunc_locals = list mfunc_local in
  let mfunc_args = list mfunc_local in
  let results = list val_type in
  map [ mfunc_locals; mfunc_args; results; gen_const; drop ] (fun l a r b d ->
    let type_f = Arg.Bt_raw (None, (a, r)) in
    let n = Env.add_func type_f in
    MFunc { type_f; locals = l; body = b @ d; id = Some n } )

let mstart_field : module_field gen = const (MStart (Raw 0))
(* default : always first func pointer *)

let modul_field : module_field gen =
  choose
    [ mglobal_field
      (* mfunc_field *)
      (* exit: specific processing *)
      (* TODO other fields *)
    ]

let modul : Types.Symbolic.modul gen =
  let sid = option bytes in
  let modul_fields = list modul_field in
  let mfunc_fields = list1 mfunc_field in
  map
    [ sid; mfunc_fields; mstart_field; modul_fields ]
    (fun _ fu st fi -> { id = Some "modul"; fields = fi @ fu @ (st :: []) })

(* in progress *)

(* let indice : indice gen =
   choose [
     map ([bytes]) (fun b -> Symbolic b);
     map ([int]) (fun i -> Raw i)
   ] *)

(*
  let nullable : nullable gen =
    choose [const No_null; const Null]
  
  let heap_type : heap_type gen =
    choose [
      const Any_ht;
      const None_ht;
      const Eq_ht;
      const I31_ht;
      const Struct_ht;
      const Array_ht;
      const Func_ht;
      const No_func_ht;
      const Extern_ht;
      const No_extern_ht
      (* TODO Def_ht of indice *)    
    ]
  
  let ref_type : ref_type gen =
    pair nullable heap_type
  
  let global_type : global_type gen =
    pair mut val_type

*)
