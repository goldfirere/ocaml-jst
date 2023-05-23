(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typechecking of type expressions for the core language *)

open Types

val valid_tyvar_name : string -> bool

type poly_univars
val make_poly_univars : string Location.loc list ->
  Asttypes.const_layout Location.loc option list ->
  poly_univars
  (* Create a set of univars with given names and layouts. Any call
     of this function *must* call [check_poly_univars] when done
     e.g. translating the type in which the univars are in scope. *)
val check_poly_univars :
   Env.t -> Location.t -> poly_univars -> type_expr list
  (* Verify that the given univars are universally quantified,
     and return the list of variables. The type in which the
     univars are used must be generalised *)
val instance_poly_univars :
   Env.t -> Location.t -> poly_univars -> type_expr list
  (* Same as [check_poly_univars], but instantiates the resulting
     type scheme (i.e. variables become Tvar rather than Tunivar) *)

val transl_simple_type:
        Env.t -> ?univars:poly_univars -> bool -> alloc_mode_const
        -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_univars:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_delayed
  :  Env.t -> alloc_mode_const
  -> Parsetree.core_type
  -> Typedtree.core_type * type_expr * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type, an instance of the corresponding type_expr, and a
           function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val reset_type_variables: unit -> unit
val type_variable: Location.t -> string -> type_expr
val transl_type_param:
  Env.t -> Parsetree.core_type -> Type_layout.t -> Typedtree.core_type

val get_alloc_mode : Parsetree.core_type -> alloc_mode_const

type variable_context
val narrow: unit -> variable_context
val widen: variable_context -> unit

exception Already_bound

type value_loc =
    Fun_arg | Fun_ret | Tuple | Poly_variant | Package_constraint | Object_field

type cannot_quantify_reason
type layout_info
type error =
    Unbound_type_variable of string
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * cannot_quantify_reason
  | Bad_univar_layout of
      { name : string; layout_info : layout_info; inferred_layout : layout }
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unsupported_extension of Clflags.Extension.t
  | Polymorphic_optional_param
  | Non_value of
      {vloc : value_loc; typ : type_expr; err : Type_layout.Violation.t}
  | Bad_layout_annot of type_expr * Type_layout.Violation.t

exception Error of Location.t * Env.t * error

val report_error: Env.t -> Format.formatter -> error -> unit

(* Support for first-class modules. *)
val transl_modtype_longident:  (* from Typemod *)
    (Location.t -> Env.t -> Longident.t -> Path.t) ref
val transl_modtype: (* from Typemod *)
    (Env.t -> Parsetree.module_type -> Typedtree.module_type) ref
val create_package_mty:
    Location.t -> Env.t -> Parsetree.package_type ->
    (Longident.t Asttypes.loc * Parsetree.core_type) list *
      Parsetree.module_type
