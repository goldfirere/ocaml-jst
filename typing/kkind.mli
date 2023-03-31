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

(** This module describes kkinds, which classify types. Kkinds are arranged
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

    Kkinds are a composition of several {i descriptors}, where a
    descriptor describes one aspect of a type, such as its memory
    representation or whether it needs to be scanned by the garbage collector.

    The kkinds listed in the lattice above are the user-available ones, but other
    combinations of descriptors are also possible.
*)

(** The module type [Descriptor] describes the interface to kkind descriptors. *)
module type Descriptor = sig
  type t

  (** Each descriptor forms a lattice; [top] is the top element of the lattice. *)
  val top : t

  (** Check whether one descriptor is beneath another in the lattice. *)
  val sub : t -> t -> bool
end

(** A [Layout.t] classifies how a type is represented at runtime. Every concrete
    kkind has a layout, and knowing the sort is sufficient for knowing the
    calling convention of values of a given type. *)
module Layout : sig
  type t

  include Descriptor with type t := t
end

(** [External.t] describes whether a type is external to the garbage
    collector.  Values of types that are external to the garbage
    collector need not be scanned during garbage collection. This
    descriptor is not relevant for non-[Value] types, as the garbage
    collector scans only [Value]s.
*)
module External : sig
  (** [External <= External64 <= Internal], meaning that an [External]
      can always safely be treated as an [Internal] (scanned unnecessarily).
  *)
  type t =
    | External
    | External64  (** external only on 64-bit platforms *)
    | Internal

  include Descriptor with type t := t
end

(** [Local.t] describes whether a type can escape from a local context. *)
module Local : sig
  (** [Global <= Local], because we can always forget that a type has
      the ability to escape from a local context. *)
  type t =
    | Global
    | Local

  include Descriptor with type t := t
end

(** A Kkind.t is a full description of the runtime representation of values
    of a given type. It includes layouts, but also the abstract top kkind
    [Any] and subkkinds of other layouts, such as [Immediate]. *)
type t

(******************************)
(* constants *)

(** Constant kkinds are used both for user-written annotations and within
    the type checker when we know a kkinds has no variables *)
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

(** We know for sure that values of types of this kkind are always immediate;
    this means that they are [Value]s that do not need to be scanned by the
    garbage collector. *)
val immediate : t

(******************************)
(* construction *)

(** Create a fresh layout variable, packed into a kkind. *)
val of_new_layout_var : unit -> t

val of_layout : Layout.t -> t
val of_const : const -> t

(** Translate a user kkind annotation to a kkind *)
val of_const_option : const option -> default:t -> t

(** Find a kkind in attributes, defaulting to ~default *)
val of_attributes : default:t -> Parsetree.attributes -> t

(******************************)
(* elimination *)

type desc =
  | Const of const
  | Var of Layout.var

(** Extract the [const] from a [Layout.t], looking through unified
    sort variables. Returns [Var] if the final, non-variable layout has not
    yet been determined. *)
val get : t -> desc

val of_desc : desc -> t

(** Returns the layout corresponding to the kkind.  Call only on concrete
    kkinds - errors on Any. *)
val layout_of_kkind : t -> Layout.t

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
  val report_with_offender_layout :
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

  Any > Layout Value > Immediate64 > Immediate
  Any > Layout Void

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
