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

module Layout = struct
  type t =
    | Value
    | Void
end

module External = struct
  type t =
    | External
    | External64   (* external only on 64-bit platforms *)
    | Internal

  let top = Internal
end

module Concrete = struct
  type t =
    | Abstract
    | Concrete
end

module Local = struct
  type t =
    | Global
    | Local

  let top = Local
end

type t =
  { layout : Layout.t option
  ; concrete : Concrete.t

  ; external_ : External.t
  ; local : Local.t
  }

(******************************)
(* constants *)

let any =
  { layout = None
  ; concrete = Abstract
  ; external_ = External.top
  ; local = Local.top
  }

let void =
  { layout = Void
  ; concrete = Concrete
  ; external_ = External
  ; local = Global
  }

let value =
  { layout = Value
  ; concrete = Concrete
  ; external_ = External.top
  ; local = Local.top
  }

let immediate64 =
  { layout = Value
  ; concrete = Concrete
  ; external_ = External64
  ; local = Local.top
  }

let immediate =
  { layout = Value
  ; concrete = Concrete
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

let sub sub super =
  let ok = Ok sub in
  let err = Error (Violation.Not_a_subkkind (sub,super)) in
  let equality_check is_eq = if is_eq then ok else err in
  match get sub, get super with
  | _, Const Any -> ok
  | Const c1, Const c2 when equal_const c1 c2 -> ok
  | Const Immediate, Const Immediate64 -> ok
  | Const (Immediate64 | Immediate), _ ->
    equality_check (equate super value)
  | _, _ -> equality_check (equate sub super)

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
