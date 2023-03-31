(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(********************)
(* Descriptors *)

module type Descriptor = sig
  type t
  val top : t
  val sub : t -> t -> bool
end

module Layout = struct

  (* short for [Representable] *)
  module Rep = struct

    module Const = struct
      type t =
        | Value
        | Void

      let equal t1 t2 = match t1, t2 with
        | Value, Value -> true
        | Void, Void -> true
        | (Value | Void), _ -> false
    end

    module Tc = struct
      type var = t option ref

      (* CR layouts: would it be more efficient to inline [const] here? *)
      and t =
        | Const of Const.t
        | Var of var

      (* also does path compression *)
      (* post-condition: if this returns Var v, then !v = None *)
      let deref v : t = match !v with
        | Some (Const _ as t) -> t
        | Some (Var next_v) ->
           let result = deref next_v in
           v := Some result; (* CR layouts: should we only write when there is a change? *)
           result
        | None -> Var v

      let equate_const_var c1 v2 = match deref v2 with
        | Const c2 -> Const.equal c1 c2
        | Var v2' -> v2' := Some c1; true

      let equate_var_var v1 v2 = v1 == v2 || match deref v1, deref v2 with
        | Const c1, Const c2 -> Const.equal c1 c2
        | Var v1, (Var v2 as t2) -> v1 == v2 || (* this avoids self-reference *)
                                    (v1 := t2; true)
        | Var v1, (Const _ as t2) -> v1 := t2; true
        | (Const _ as t1), Var v2 -> v2 := t1; true

      let equate t1 t2 = match t1, t2 with
        | Const c1, Const c2 -> Const.equal c1 c2
        | Const c1, Var v2 -> equate_const_var c1 v2
        | Var v1, Const c2 -> equate_const_var c2 v1
        | Var v1, Var v2 -> equate_var_var v1 v2

      let void = Const void
      let value = Const Value
    end

    (* this equality is not exported in the mli file; maybe someday we'll actually
       make a different type here.
       INVARIANT: No variables here. *)
    type t = Tc.t

    let equal t1 t2 = match t1, t2 with
      | Const c1, Const c2 -> Const.equal c1 c2
      | (Var _, _) | (_, Var _) -> assert false

    let void = Tc.void
    let value = Tc.value
  end

  module Tc = struct
    type t =
      | Rep of Rep.Tc.t
      | Any

    let top = Any

    let sub sub super = match sub, super with
      | _, Any -> true
      | Any, Rep _ -> false
      | Rep r1, Rep r2 -> Rep.Tc.equate r1 r2

    let void = Rep Rep.Tc.void
    let value = Rep Rep.Tc.value
  end

  (* This equality is not exported in the mli file. *)
  type t = Tc.t

  let top = Any

  let sub sub super = match sub, super with
    | _, Any -> true
    | Any, Rep _ -> false
    | Rep r1, Rep r2 -> Rep.equal r1 r2

  let void = Tc.void
  let value = Tc.value
end

module External = struct
  type t =
    | External
    | External64
    | Internal

  let top = Internal
  let sub sub super = match sub, super with
    | Internal, (External | External64) -> false
    | External64, External -> false
    | (External | External64, Internal), _ -> true
end

module Local = struct
  type t =
    | Global
    | Local

  let top = Local
  let sub sub super = match sub, super with
    | Local, Global -> false
    | (Local | Global), _ -> true
end

(*******************************)
(* Kkinds *)

module Tc = struct
  type t =
    { layout : Layout.Tc.t
    ; external_ : External.t
    ; local : Local.t
    }
end

type t = Tc.t

(******************************)
(* constants *)

let any =
  { layout = Any
  ; external_ = External.top
  ; local = Local.top
  }

let void =
  { layout = Layout.Tc.void
  ; external_ = External
  ; local = Global
  }

let value =
  { layout = Layout.Tc.value
  ; external_ = External.top
  ; local = Local.top
  }

let immediate64 =
  { layout = Layout.Tc.value
  ; external_ = External64
  ; local = Local.top
  }

let immediate =
  { layout = Layout.Tc.value
  ; external_ = External
  ; local = Global
  }

type const = Asttypes.const_kkind =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate

let string_of_const : const -> _ = function
  | Any -> "any"
  | Value -> "value"
  | Void -> "void"
  | Immediate64 -> "immediate64"
  | Immediate -> "immediate"

let equal_const (c1 : const) (c2 : const) = match c1, c2 with
  | Any, Any -> true
  | Immediate64, Immediate64 -> true
  | Immediate, Immediate -> true
  | Void, Void -> true
  | Value, Value -> true
  | (Any | Immediate64 | Immediate | Void | Value), _ -> false

(******************************)
(* construction *)

let of_new_layout_var () = Layout (Layout.new_var ())

let of_layout s = Layout s

let of_const : const -> t = function
  | Any -> Any
  | Immediate -> Immediate
  | Immediate64 -> Immediate64
  | Value -> value
  | Void -> void

let of_const_option annot ~default =
  match annot with
  | None -> default
  | Some annot -> of_const annot

let of_attributes ~default attrs =
  of_const_option ~default (Builtin_attributes.kkind attrs)

(******************************)
(* elimination *)

type desc =
  | Const of const
  | Var of Layout.var

let repr ~default : t -> desc = function
  | Any -> Const Any
  | Immediate -> Const Immediate
  | Immediate64 -> Const Immediate64
  | Layout s -> begin match Layout.repr ~default s with
    (* NB: this match isn't as silly as it looks: those are
       different constructors on the left than on the right *)
    | Const Void -> Const Void
    | Const Value -> Const Value
    | Var v -> Var v
  end

let get = repr ~default:None

let of_desc = function
  | Const c -> of_const c
  | Var v -> of_layout (Layout.of_var v)

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let layout_of_kkind l =
  match get l with
  | Const Void -> Layout.void
  | Const (Value | Immediate | Immediate64) -> Layout.value
  | Const Any -> Misc.fatal_error "Kkind.layout_of_kkind"
  | Var v -> Layout.of_var v

(*********************************)
(* pretty printing *)

let to_string lay = match get lay with
  | Const c -> string_of_const c
  | Var _ -> "<layout variable>"

let format ppf t = Format.fprintf ppf "%s" (to_string t)

(******************************)
(* errors *)

module Violation = struct
  type nonrec t =
    | Not_a_subkkind of t * t
    | No_intersection of t * t

  let report_with_offender ~offender ppf t =
    let pr fmt = Format.fprintf ppf fmt in
    match t with
    | Not_a_subkkind (l1, l2) ->
        pr "%t has layout %a, which is not a sublayout of %a." offender
          format l1 format l2
    | No_intersection (l1, l2) ->
        pr "%t has layout %a, which does not overlap with %a." offender
          format l1 format l2

  let report_with_offender_layout ~offender ppf t =
    let layout_expected =
      "A representable layout was expected, but"
    in
    let pr fmt = Format.fprintf ppf fmt in
    match t with
    | Not_a_subkkind (l1, l2) ->
      pr "%s@ %t has layout %a, which is not a sublayout of %a."
        layout_expected offender format l1 format l2
    | No_intersection (l1, l2) ->
      pr "%s@ %t has layout %a, which does not overlap with %a."
        layout_expected offender format l1 format l2

  let report_with_name ~name ppf t =
    let pr fmt = Format.fprintf ppf fmt in
    match t with
    | Not_a_subkkind (l1,l2) ->
        pr "%s has layout %a, which is not a sublayout of %a." name
          format l1 format l2
    | No_intersection (l1, l2) ->
        pr "%s has layout %a, which does not overlap with %a." name
          format l1 format l2
end

(******************************)
(* relations *)

let equate (l1 : t) (l2 : t) = match l1, l2 with
  | Any, Any -> true
  | Immediate64, Immediate64 -> true
  | Immediate, Immediate -> true
  | Layout s1, Layout s2 -> Layout.equate s1 s2
  | (Any | Immediate64 | Immediate | Layout _), _ -> false

let intersection l1 l2 =
  let err = Error (Violation.No_intersection (l1, l2)) in
  let equality_check is_eq l = if is_eq then Ok l else err in
  (* it's OK not to cache the result of [get], because [get] does path
     compression *)
  match get l1, get l2 with
  | Const Any, _ -> Ok l2
  | _, Const Any -> Ok l1
  | Const c1, Const c2 when equal_const c1 c2 -> Ok l1
  | Const (Immediate64 | Immediate), Const (Immediate64 | Immediate) ->
    Ok immediate
  | Const ((Immediate64 | Immediate) as imm), l
  | l, Const ((Immediate64 | Immediate) as imm) ->
    equality_check (equate (of_desc l) value)
      (of_const imm)
  | _, _ -> equality_check (equate l1 l2) l1

let sub { layout = layout_sub
        ; concrete = concrete_sub
        ; external_ = external_sub
        ; local = local_sub }
        { layout = layout_super
        ; concrete = concrete_super
        ; external_ = external_super
        ; local = local_super } =
  Layout.equal layout_sub layout_super &&
  Concrete.sub concrete_sub concrete_super &&
  External.sub external_sub external_super &&
  Local.sub local_sub local_super

(*********************************)
(* defaulting *)

let get_defaulting ~default t =
  match repr ~default:(Some default) t with
  | Const result -> result
  | Var _ -> assert false

let constrain_default_void = get_defaulting ~default:Layout.Void
let can_make_void l = Void = constrain_default_void l
let default_to_value t =
  ignore (get_defaulting ~default:Value t)

(*********************************)
(* debugging *)

module Debug_printers = struct
  open Format

  let t ppf : t -> unit = function
    | Any         -> fprintf ppf "Any"
    | Layout s    -> fprintf ppf "Layout %a" Layout.Debug_printers.t s
    | Immediate64 -> fprintf ppf "Immediate64"
    | Immediate   -> fprintf ppf "Immediate"
end
