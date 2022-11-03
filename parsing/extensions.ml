open Asttypes
open Parsetree
open Extensions_parsing

(******************************************************************************)
(** Individual language extension modules *)

(*
Note [Check for immutable extension in comprehensions code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we spot a comprehension for a mutable array, we need to make
sure that both [comprehensions] and [immutable_arrays] are enabled.
But our general mechanism for checking for enabled extensions
(in Extensions_parsing.Translate(...).of_ast) won't work well here:
it triggers when converting from e.g. [[%extensions.comprehensions.array] ...]
to the comprehensions-specific AST. But if we spot a
[[%extensions.comprehensions.immutable]], there is no expression to translate.

The alternative would be to track the shape of comprehension in some expression,
just so it can be translated, just to report if an extension is off. This is
not a good tradeoff, so we just check the [immutable_arrays] extension when we
encounter it within the comprehensions code.
*)

(** List and array comprehensions *)
module Comprehensions = struct
  type iterator =
    | Range of { start     : expression
               ; stop      : expression
               ; direction : direction_flag }
    | In of expression

  type clause_binding =
    { pattern    : pattern
    ; iterator   : iterator
    ; attributes : attribute list }

  type clause =
    | For of clause_binding list
    | When of expression

  type comprehension =
    { body    : expression
    ; clauses : clause list
    }

  type comprehension_expr =
    | Cexp_list_comprehension  of comprehension
    | Cexp_array_comprehension of mutable_flag * comprehension

  let extension_string = Clflags.Extension.to_string Comprehensions

  (* CR aspectorzabusky: new name? *)
  let comprehension_expr ~loc names =
    Expression.make_extension ~loc (extension_string :: names)

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in
      [expr_of_comprehension_expr]. *)

  let expr_of_iterator ~loc = function
    | Range { start; stop; direction } ->
        comprehension_expr
          ~loc
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        comprehension_expr ~loc ["for"; "in"] seq

  let expr_of_clause_binding ~loc { pattern; iterator; attributes } =
    Ast_helper.Vb.mk
      ~loc
      ~attrs:attributes
      pattern
      (expr_of_iterator ~loc iterator)

  let expr_of_clause ~loc = function
    | For iterators -> fun rest ->
        comprehension_expr
          ~loc
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive
             (List.map (expr_of_clause_binding ~loc) iterators)
             rest)
    | When cond -> fun rest ->
        comprehension_expr
          ~loc
          ["when"]
          (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~loc ~type_ { body; clauses } =
    comprehension_expr
      ~loc
      type_
      (List.fold_right
         (expr_of_clause ~loc)
         clauses
         (comprehension_expr ~loc ["body"] body))

  let expr_of_comprehension_expr ~loc eexpr =
    let ghost_loc = { loc with Location.loc_ghost = true } in
    let expr_of_comprehension_type type_ =
      expr_of_comprehension ~loc:ghost_loc ~type_
    in
    match eexpr with
    | Cexp_list_comprehension comp ->
        expr_of_comprehension_type ["list"]  comp
    | Cexp_array_comprehension (amut, comp) ->
        expr_of_comprehension_type
          [ "array"
          ; match amut with
            | Mutable   ->
                "mutable"
            | Immutable ->
                "immutable"
          ]
          comp

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in
      [comprehension_expr_of_expr]. *)

  let expand_comprehension_extension_expr expr =
    match Expression.match_extension expr with
    | Some (comprehensions :: name, expr)
      when String.equal comprehensions extension_string ->
        name, expr
    | Some (name, _) ->
        failwith ("Tried to desugar the non-comprehension extension point \
                   \"extension." ^ String.concat "." name ^ "\" as part of a \
                   comprehension expression")
    | None ->
        failwith "Tried to desugar a non-extension expression as part of a \
                  comprehension expression"

  let expand_comprehension_extension_expr_failure name =
    failwith ("Unknown, unexpected, or malformed comprehension extension point \
               \"extension.comprehension." ^ String.concat "." name ^ "\"")

  let iterator_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["for"; "range"; "upto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
      (* CR aspectorzabusky: Check other parts of the [pexp_desc]? *)
        Range { start; stop; direction = Upto }
    | ["for"; "range"; "downto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
      (* CR aspectorzabusky: Check other parts of the [pexp_desc]? *)
        Range { start; stop; direction = Downto }
    | ["for"; "in"], seq ->
        In seq
    | bad, _ ->
        expand_comprehension_extension_expr_failure bad

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let rec raw_comprehension_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["for"], { pexp_desc = Pexp_let(Nonrecursive, iterators, rest); _ } ->
        add_clause
          (For (List.map clause_binding_of_vb iterators))
          (raw_comprehension_of_expr rest)
    | ["when"], { pexp_desc = Pexp_sequence(cond, rest); _ } ->
        add_clause
          (When cond)
          (raw_comprehension_of_expr rest)
    | ["body"], body ->
        { body; clauses = [] }
    | bad, _ ->
        expand_comprehension_extension_expr_failure bad

  let comprehension_of_expr expr =
    match raw_comprehension_of_expr expr with
    | { body = _; clauses = [] } ->
        failwith "Tried to desugar a comprehension with no clauses"
    | comp -> comp

  let comprehension_expr_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["list"], comp ->
        Cexp_list_comprehension (comprehension_of_expr comp)
    | ["array"; "mutable"], comp ->
        Cexp_array_comprehension (Mutable, comprehension_of_expr comp)
    | ["array"; "immutable"], comp ->
        (* assert_extension_enabled:
           See Note [Check for immutable extension in comprehensions code] *)
        assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays;
        Cexp_array_comprehension (Immutable, comprehension_of_expr comp)
    | bad, _ ->
        expand_comprehension_extension_expr_failure bad
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list
        (** [: E1; ...; En :] *)

  type nonrec pattern =
    | Iapat_immutable_array of pattern list
        (** [: P1; ...; Pn :] **)

  let extension_string = Clflags.Extension.to_string Immutable_arrays

  let expr_of ~loc = function
    | Iaexp_immutable_array elts -> Ast_helper.Exp.array ~loc elts

  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts -> Ast_helper.Pat.array ~loc elts

  let of_pat expr = match expr.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"
end

(** We put our grouped ASTs in modules so that we can export them later;
    however, we need to extend these modules later, so we have to give these
    modules backup names, and we drop the [Ext] for export. *)

module Ext_expression (* : Translation *) = struct
  (* We leave off the module signature so that t can be exported transparently,
     to allow the `include` to work, below. *)

  module AST = Expression

  type t =
    | Eexp_comprehension   of Comprehensions.comprehension_expr
    | Eexp_immutable_array of Immutable_arrays.expression

  let ast_of ~loc = function
    | Eexp_comprehension cexpr ->
      (* TODO: shave yak further? perhaps by passing Comprehensions as fcm *)
      Expression.make_extension ~loc [Comprehensions.extension_string]
        (Comprehensions.expr_of_comprehension_expr ~loc cexpr)
    | Eexp_immutable_array iaexpr ->
      Expression.make_extension ~loc [Immutable_arrays.extension_string]
        (Immutable_arrays.expr_of ~loc iaexpr)

  let of_ast_internal (ext : Clflags.Extension.t) expr = match ext with
    | Comprehensions ->
      Some (Eexp_comprehension (Comprehensions.comprehension_expr_of_expr expr))
    | Immutable_arrays ->
      Some (Eexp_immutable_array (Immutable_arrays.of_expr expr))
    | _ -> None
end

module Ext_pattern (* : Translation *) = struct
  (* We leave off the module signature so that t can be exported transparently,
     to allow the `include` to work, below. *)

  module AST = Pattern

  type t =
    | Epat_immutable_array of Immutable_arrays.pattern

  let ast_of ~loc = function
    | Epat_immutable_array iapat ->
      Pattern.make_extension ~loc [Immutable_arrays.extension_string]
        (Immutable_arrays.pat_of ~loc iapat)

  let of_ast_internal (ext : Clflags.Extension.t) pat = match ext with
    | Immutable_arrays ->
      Some (Epat_immutable_array (Immutable_arrays.of_pat pat))
    | _ -> None
end

(******************************************************************************)
(** The interface to language extensions, which we export; at this point we're
    willing to shadow the module (types) imported from [Extensions_parsing]. *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
  val ast_of : loc:Location.t -> t -> ast
end

module Expression = struct
  include Ext_expression
  include Make_of_ast(Ext_expression)
end

module Pattern = struct
  include Ext_pattern
  include Make_of_ast(Ext_pattern)
end
