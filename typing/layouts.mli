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

module Sort : sig
  (** A sort classifies how a type is represented at runtime. Every concrete
      layout has a sort, and knowing the sort is sufficient for knowing the
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

  (** These names are generated lazily and only when this function is called,
      and are not guaranteed to be efficient to create *)
  val var_name : var -> string

  (** This checks for equality, and sets any variables to make two sorts
      equal, if possible *)
  val equate : t -> t -> bool

  val format : Format.formatter -> t -> unit

  (** Defaults this sort to void if possible; returns true if this succeeded
      (or if the sort was already void) *)
  val can_make_void : t -> bool

  (** Defaults any variables to value; leaves other sorts alone *)
  val default_to_value : t -> unit

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
    val var : Format.formatter -> var -> unit
  end
end

type sort = Sort.t

(** This module describes layouts, which classify types. Layouts are arranged
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
module Layout : sig
  (** A Layout.t is a full description of the runtime representation of values
      of a given type. It includes sorts, but also the abstract top layout
      [Any] and sublayouts of other sorts, such as [Immediate]. *)
  type t

  (******************************)
  (* errors *)

  type concrete_layout_reason =
    | Match
    | Constructor_declaration of int
    | Label_declaration of Ident.t
    | Unannotated_type_parameter
    | Record_projection
    | Record_assignment
    | Let_binding
    | Structure_element

  type annotation_context =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string
    | With_constraint of string
    | Newtype_declaration of string

   type value_creation_reason =
    | Class_let_binding
    | Function_argument
    | Function_result
    | Tuple_element
    | Probe
    | Package_hack
    | Object
    | Instance_variable
    | Object_field
    | Class_field
    | Boxed_record
    | Boxed_variant
    | Extensible_variant
    | Primitive of Ident.t
    | Type_argument (* CR layouts: Should this take a Path.t? *)
    | Tuple
    | Row_variable
    | Polymorphic_variant
    | Arrow
    | Tfield
    | Tnil
    | First_class_module
    | Separability_check
    | Univar
    | Polymorphic_variant_field
    | Default_type_layout
    | Float_record_field
    | Existential_type_variable
    | Array_element
    | Lazy_expression
    | Class_argument
    | Structure_element
    | Debug_printer_argument
    | V1_safety_check
    | Unknown of string  (* CR layouts: get rid of these *)

  type immediate_creation_reason =
    | Empty_record
    | Empty_variant
    | Primitive of Ident.t
    | Immediate_polymorphic_variant
    | Gc_ignorable_check
    | Value_kind

  type immediate64_creation_reason =
    | Local_mode_cross_check
    | Gc_ignorable_check
    | Separability_check

  type void_creation_reason =
    | V1_safety_check

  type any_creation_reason =
    | Missing_cmi of Path.t
    | Wildcard
    | Unification_var
    | Initial_typedecl_env
    | Dummy_layout

  type creation_reason =
    | Annotated of annotation_context * Location.t
    | Value_creation of value_creation_reason
    | Immediate_creation of immediate_creation_reason
    | Immediate64_creation of immediate64_creation_reason
    | Void_creation of void_creation_reason
    | Any_creation of any_creation_reason
    | Concrete_creation of concrete_layout_reason

  type intersection_reason =
    | Gadt_equation of Path.t
    | Tyvar_refinement (* CR layouts: this needs to carry a type_expr,
                          but that's loopy *)

  module Violation : sig
    type nonrec t =
      | Not_a_sublayout of t * t
      | No_intersection of t * t
      | Missing_cmi of Path.t * t * t
        (* means: we can't prove that lay1 is a sublayout of lay2 because
           lay1's cmi is missing *)


    (* CR layouts: The [offender] arguments below are always
       [Printtyp.type_expr], so we should either stash that in a ref (like with
       [set_printtyp_path] below) or just move all the printing machinery
       downstream of both [Layouts] and [Printtyp]. *)

    (* CR layouts: Having these options for printing a violation was a choice
       made based on the needs of expedient debugging during development, but
       probably should be rethought at some point. *)
    (** Prints a violation and the thing that had an unexpected layout
        ([offender], which you supply an arbitrary printer for). *)
    val report_with_offender :
      offender:(Format.formatter -> unit) ->
      Format.formatter -> t -> unit

    (** Like [report_with_offender], but additionally prints that the issue is
        that a representable layout was expected. *)
    val report_with_offender_sort :
      offender:(Format.formatter -> unit) ->
      Format.formatter -> t -> unit

    (** Simpler version of [report_with_offender] for when the thing that had an
        unexpected layout is available as a string. *)
    val report_with_name : name:string -> Format.formatter -> t -> unit
  end

  (******************************)
  (* constants *)

  (** Constant layouts are used both for user-written annotations and within
      the type checker when we know a layout has no variables *)
  type const = Asttypes.const_layout =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate
  val string_of_const : const -> string
  val equal_const : const -> const -> bool

  (** This layout is the top of the layout lattice. All types have layout [any].
      But we cannot compile run-time manipulations of values of types with layout
      [any]. *)
  val any : creation:any_creation_reason -> t

  (** Value of types of this layout are not retained at all at runtime *)
  val void : creation:void_creation_reason -> t

  (** This is the layout of normal ocaml values *)
  val value : creation:value_creation_reason -> t

  (** Values of types of this layout are immediate on 64-bit platforms; on other
      platforms, we know nothing other than that it's a value. *)
  val immediate64 : creation:immediate64_creation_reason -> t

  (** We know for sure that values of types of this layout are always immediate *)
  val immediate : creation:immediate_creation_reason -> t

  (******************************)
  (* construction *)

  (** Create a fresh sort variable, packed into a layout. *)
  val of_new_sort_var : creation:concrete_layout_reason -> t

  val of_sort : creation:concrete_layout_reason -> sort -> t
  val of_const : creation:creation_reason -> const -> t

  (** Find a layout in attributes.  Returns error if a disallowed layout is
      present, but always allows immediate attributes if ~legacy_immediate is
      true.  See comment on [Builtin_attributes.layout].  *)
  val of_attributes :
    legacy_immediate:bool -> reason:annotation_context -> Parsetree.attributes ->
    (t option, Location.t * const) result

  (** Find a layout in attributes, defaulting to ~default.  Returns error if a
      disallowed layout is present, but always allows immediate if
      ~legacy_immediate is true.  See comment on [Builtin_attributes.layout]. *)
  val of_attributes_default :
    legacy_immediate:bool -> reason:annotation_context ->
    default:t -> Parsetree.attributes ->
    (t, Location.t * const) result

  (******************************)
  (* elimination *)

  type desc =
    | Const of const
    | Var of Sort.var

  (** Extract the [const] from a [Layout.t], looking through unified
      sort variables. Returns [Var] if the final, non-variable layout has not
      yet been determined. *)
  val get : t -> desc

  (** Returns the sort corresponding to the layout.  Call only on representable
      layouts - raises on Any. *)
  val sort_of_layout : t -> sort

  (*********************************)
  (* pretty printing *)

  val to_string : t -> string
  val format : Format.formatter -> t -> unit

  (** XXX layouts RAE: document *)
  val format_history :
    intro:(Format.formatter -> unit) ->
    Format.formatter -> t -> unit

  (** Provides the [Printtyp.path] formatter back up the dependency chain to
      this module. *)
  val set_printtyp_path : (Format.formatter -> Path.t -> unit) -> unit

  (******************************)
  (* relations *)

  (** This checks for equality, and sets any variables to make two layouts
      equal, if possible. e.g. [equate] on a var and [value] will set the
      variable to be [value].

      This function ignores the [missing_cmi_for] medatadata for [any]s. *)
  val equate : t -> t -> bool

  (** This checks for equality, but has the invariant that it can only be called
      when there is no need for unification; e.g. [equal] on a var and [value]
      will crash.

      CR layouts (v1.5): At the moment, this is actually the same as [equate]! *)
  val equal : t -> t -> bool

  (** Finds the intersection of two layouts, constraining sort variables to
      create one if needed, or returns a [Violation.t] if an intersection does
      not exist.  Can update the layouts.  The returned layout's history
      consists of the provided reason followed by the history of the first
      layout argument.  That is, due to histories, this function is asymmetric;
      it should be thought of as modifying the first layout to be the
      intersection of the two, not something that modifies the second layout. *)
  val intersection :
    reason:intersection_reason -> t -> t -> (t, Violation.t) Result.t

  (** [sub t1 t2] returns [Ok t1] iff [t1] is a sublayout of
    of [t2]. [t1]'s history will be updated to record the sublayout
    check. The current hierarchy is:

    Any > Sort Value > Immediate64 > Immediate
    Any > Sort Void

    Returns [Error _] if the coercion is not possible. *)
  val sub : t -> t -> (t, Violation.t) result

  (** Checks to see whether a layout is void. Call only after type-checking
      is complete (no unfilled sort variables allowed here!). *)
  val is_void : t -> bool
  (* CR layouts v5: When we have proper support for void, we'll want to change
     things to default to void - it's the most efficient thing when we have a
     choice. *)

  (** Checks to see whether a layout is any. Never does any mutation. *)
  val is_any : t -> bool

  (*********************************)
  (* defaulting *)
  val constrain_default_value : t -> const

  val default_to_value : t -> unit

  (*********************************)
  (* debugging *)

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
  end
end

type layout = Layout.t
