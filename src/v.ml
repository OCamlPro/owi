(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

include (
  struct
    type vbool = bool

    type int32 = Int32.t

    type int64 = Int64.t

    type float32 = Float32.t

    type float64 = Float64.t

    let const_i32 x = x

    let const_i64 x = x

    let const_f32 x = x

    let const_f64 x = x

    include Concrete_value

    module Ref = struct
      let get_func (r : ref_value) : Func_intf.t Value_intf.get_ref =
        match r with
        | Funcref (Some f) -> Ref_value f
        | Funcref None -> Null
        | _ -> Type_mismatch

      let get_externref (type t) (r : ref_value) (t : t Type.Id.t) :
        t Value_intf.get_ref =
        match r with
        | Externref (Some (E (ety, v))) -> (
          match Type.Id.provably_equal t ety with
          | None -> assert false
          | Some Equal -> Ref_value v )
        | _ -> assert false
    end

    module Bool = struct
      let const c = c

      let not = not

      let and_ = ( && )

      let or_ = ( || )

      let int32 = function true -> 1l | false -> 0l

      let pp = Format.pp_bool
    end

    module I32 = struct
      include Int32
      include Convert.Int32

      let to_bool i = Int32.ne i 0l
    end

    module I64 = struct
      include Int64
      include Convert.Int64
    end

    module F32 = struct
      include Float32
      include Convert.Float32
    end

    module F64 = struct
      include Float64
      include Convert.Float64
    end
  end :
    Value_intf.T
      with type vbool = Bool.t
       and type int32 = Int32.t
       and type int64 = Int64.t
       and type float32 = Float32.t
       and type float64 = Float64.t
       and type ref_value = Concrete_value.ref_value
       and type t = Concrete_value.t )
