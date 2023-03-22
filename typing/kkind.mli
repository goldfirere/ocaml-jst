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

(* This module is named Kkind, with an extra k, to distinguish kkinds
   as used here from type kinds (which might be abstract or record or variant,
   etc.). This is clearly far from ideal, but the current scheme has these
   positives:

   * It allows us to call kkinds "kinds" to users, connecting that word with
     a word that actually appears in the code.

   * It allows us to use "layout" to refer to the component of a kkind that
     describes the in-memory/in-register layout of a type. Using "layout"
     to refer to the whole kkind seems worse than the already-terrible "kkind".

   * We could imagine renaming the existing "kind" to something else ("shape"?),
     but that would introduce merge conflicts. Perhaps, with broad support from
     OCaml developers, we can make this switch someday.

   * It is very easy to search for and replace when we have a better name.
*)

module Sort : sig
  (** A sort classifies how a type is represented at runtime. Every concrete
      kkind has a sort, and knowing the sort is sufficient for knowing the
      calling convention of values of a given type. *)
  type t

  (** These are the constant sorts -- fully determined and without variables *)
  type const =
    | Void
      (** No run time representation at all *)
    | Value
      (** Standard ocaml value representation *)

  (** A sort variable that can be unified during type-checking. *)
  type var

  (** Create a new sort variable that can be unified. *)
  val new_var : unit -> t

  val of_const : const -> t
  val of_var : var -> t

  val void : t
  val value : t

  (** This checks for equality, and sets any variables to make two sorts
      equal, if possible *)
  val equate : t -> t -> bool

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
    val var : Format.formatter -> var -> unit
  end
end

type sort = Sort.t

(* This module describes kkinds, which classify types. Layouts are arranged
   in the following lattice:

   {[
               any
             /    \
          value  void
            |
        immediate64
            |
        immediate
   ]}
*)

(** A Kkind.t is a full description of the runtime representation of values
    of a given type. It includes sorts, but also the abstract top kkind
    [Any] and subkkinds of other sorts, such as [Immediate]. *)
type t

(******************************)
(* constants *)

(** Constant kkinds are used both for user-written annotations and within
    the type checker when we know a kkind has no variables *)
type const = Asttypes.const_kkind =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate
val string_of_const : const -> string
val equal_const : const -> const -> bool

(** This kkind is the top of the kkind lattice. All types have kkind [any].
    But we cannot compile run-time manipulations of values of types with kkind
    [any]. *)
val any : t

(** Value of types of this kkind are not retained at all at runtime *)
val void : t

(** This is the kkind of normal ocaml values *)
val value : t

(** Values of types of this kkind are immediate on 64-bit platforms; on other
    platforms, we know nothing other than that it's a value. *)
val immediate64 : t

(** We know for sure that values of types of this kkind are always immediate *)
val immediate : t

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a kkind. *)
val of_new_sort_var : unit -> t

val of_sort : sort -> t
val of_const : const -> t

(** Translate a user kkind annotation to a kkind *)
val of_const_option : const option -> default:t -> t

(** Find a kkind in attributes, defaulting to ~default *)
val of_attributes : default:t -> Parsetree.attributes -> t

(******************************)
(* elimination *)

type desc =
  | Const of const
  | Var of Sort.var

(** Extract the [const] from a [Kkind.t], looking through unified
    sort variables. Returns [Var] if the final, non-variable kkind has not
    yet been determined. *)
val get : t -> desc

val of_desc : desc -> t

(** Returns the sort corresponding to the kkind.  Call only on representable
    kkinds - errors on Any. *)
val sort_of_kkind : t -> sort

(*********************************)
(* pretty printing *)

val to_string : t -> string
val format : Format.formatter -> t -> unit

(******************************)
(* errors *)
module Violation : sig
  type nonrec t =
    | Not_a_subkkind of t * t
    | No_intersection of t * t

  val report_with_offender :
    offender:(Format.formatter -> unit) -> Format.formatter -> t -> unit
  val report_with_offender_sort :
    offender:(Format.formatter -> unit) -> Format.formatter -> t -> unit
  val report_with_name : name:string -> Format.formatter -> t -> unit
end

(******************************)
(* relations *)

(** This checks for equality, and sets any variables to make two kkinds
    equal, if possible. e.g. [equate] on a var and [value] will set the
    variable to be [value] *)
val equate : t -> t -> bool

(** Finds the intersection of two kkinds, or returns a [Violation.t]
    if an intersection does not exist. *)
val intersection : t -> t -> (t, Violation.t) Result.t

(** [sub t1 t2] returns [Ok t1] iff [t1] is a subkkind of
  of [t2].  The current hierarchy is:

  Any > Sort Value > Immediate64 > Immediate
  Any > Sort Void

  Return [Error _] if the coercion is not possible. We return a kkind in the
  success case because it sometimes saves time / is convenient to have the
  same return type as intersection. *)
val sub : t -> t -> (t, Violation.t) result

(*********************************)
(* defaulting *)
val constrain_default_void : t -> const
val can_make_void : t -> bool
(* CJC XXX at the moment we default to void whenever we can.  But perhaps it
   would be better to default to value before we actually ship. *)

val default_to_value : t -> unit

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> t -> unit
end
