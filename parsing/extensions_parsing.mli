(** This module handles the logic around the syntax of our extensions to OCaml
    for [ocaml-jst], keeping the gory details wrapped up behind a clean
    interface.

    As we've started to work on syntactic extensions to OCaml, three concerns
    arose about the mechanics of how we wanted to maintain these changes in our
    fork.

    1. We don't want to extend the AST for our fork, as we really want to make
       sure things like ppxen are cross-compatible between upstream and
       [ocaml-jst].  Thankfully, OCaml already provides places to add extra
       syntax: extension points and annotations!  Thus, we have to come up with
       a way of representing our new syntactic constructs in terms of extension
       points (or annotations, but we went with the former).

    2. We don't want to actually match on extension points whose names are
       specific strings all over the compiler; that's incredibly messy, and it's
       easy to miss cases, etc.

    3. We want to keep different language extensions distinct so that we can add
       them to upstream independently, work on them separately, and so on.

    We have come up with a design that addresses those concerns by providing
    both a nice compiler-level interface for working with our syntactic
    extensions as first-class AST nodes, as well as a uniform scheme for
    translating this to and from OCaml AST values containing extension points.
    One wrinkle is that OCaml has many ASTs, one for each syntactic category
    (expressions, patterns, etc.); we have to provide this facility for each
    syntactic category where we want to provide extensions.

    a. For each language extension, we will define a module (e.g.,
       [Comprehensions]), in which we define a proper AST type per syntactic
       category we care about (e.g., [Comprehensions.comprehension_expr] and its
       subcomponents).  This addresses concern (3); we've now contained each
       extension in a module.  But just that would leave them too siloed, so…

    b. We define an *overall auxiliary AST* for each syntactic category that's
       just for our language extensions; for expressions, it's called
       [extension_expr].  It contains one constructor for each of the AST types
       defined as described in design point (1).  This addresses concern (2); we
       can now match on actual OCaml constructors, as long as we can get ahold
       of them.  And to do that…

    c. We define a general scheme for how we represent language extensions in terms
       of the existing ASTs, and provide a few primitives for consuming/creating
       AST nodes of this form, for each syntactic category.  There's not a lot
       of abstraction to be done, or at least it's not (yet) apparent what
       abstraction there is to do, so most of this remains manual.  (Setting up
       a full lens-based/otherwise bidirectional approach sounds like a great
       opportunity for yak-shaving, but not *actually* a good idea.)  This
       solves concern (3), and by doing it uniformly helps us address multiple
       cases at one stroke.

    We then bundle this all up for each individual extension into the type
    TODO (update) [ast_extension] containing, for one syntactic category, two different
    (partial) isomorphisms: the fully isomorphic (up to exceptions) ability to
    lift and lower between the custom AST type (from design point (a)) and
    existing AST expressions, leveraging the common format for representing
    things in the existing AST from design point (c); and the partial ability to
    lift and lower between the custom AST type and our overall auxiliary AST
    type (from design point (b)), which is just a constructor application in one
    direction and a pattern match against a constructor in the other.  Each
    syntactic category  (or the lack of support for one) can then be stored
    inside an existential type ([optional_ast_extension]) that hides the
    extension-specific type, allowing us to collect all of our extensions
    together.

    This module contains the logic for moving to and from OCaml ASTs; the gory
    details of the encoding are detailed in the implementation.  All the actual
    ASTs should live in [Extensions], which is the only module that should
    directly depend on this one.  Here, we parameterize by the eventual
    auxiliary all-extension ASTs; these are the ['ext_ast] type parameters seen
    below.  We must parameterize as we can't define the auxiliary types until
    we've defined every language extension; by parameterizing, we get to keep
    all the messy AST-manipulation here, and work with it abstractly while
    defining the language extensions themselves.  *)

(** The type of modules that lift and lower language extension terms from and
    to an OCaml AST type ([ast]) *)
module type AST = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The name for this syntactic category in the plural form; used for error
      messages (e.g., "expressions") *)
  val plural : string

  (** How to get the location attached to an AST node *)
  val location : ast -> Location.t

  (** Embed a language extension term in the AST with the given name
      and body (the [ast]).  The name will be joined with dots
      and preceded by [extension.].  Partial inverse of [match_extension]. *)
  val make_extension  : loc:Location.t -> string list -> ast -> ast

  (** Given an AST node, check if it's a language extension term; if it is,
      split it back up into its name (the [string list]) and the body (the
      [ast]); the resulting name is split on dots and the leading [extension]
      component is dropped.  If the language extension term is malformed in any
      way, raises an error; if the input isn't a language extension term,
      returns [None].  Partial inverse of [make_extension]. *)
  val match_extension : ast -> (string list * ast) option
end

(** One [AST] module per syntactic category we currently care about; we're
    adding these lazily as we need them. *)

module Expression : sig
  include AST with type ast = Parsetree.expression
  module type Extension = sig
    type expression
    val extension_string : string
    val expr_of : loc:Location.t -> expression -> Parsetree.expression
  end
  val make_extension_expr :
    (module Extension with type expression = 'expr) ->
      loc:Location.t -> 'expr -> Parsetree.expression
end

module Pattern    : AST with type ast = Parsetree.pattern

(** Create the two core functions of this module: lifting and lowering OCaml AST
    terms from any syntactic category to and from our auxiliary all-extension
    ASTs.

    This will only get instantiated once; however, by making it a functor, we
    can keep all the general logic together here in this module, and keep the
    extension-specific stuff in [Extensions].. *)

module type Translation = sig
  module AST : AST

  type t

  val ast_of : loc:Location.t -> t -> AST.ast
  val of_ast_internal : Clflags.Extension.t -> AST.ast -> t option
end

module Make_of_ast (Translation : Translation) : sig

  (** Interpret an AST term in the specified syntactic category as a term of the
      appropriate auxiliary language extension AST if possible.  Raises an error
      if the extension it finds is disabled or if the language extension
      embedding is malformed.  *)
  val of_ast : Translation.AST.ast -> Translation.t option
end

(** Require that an extension is enabled, or else throw an exception (of an
    unexported type) at the provided location saying otherwise.  This is
    intended to be used in "extensions.ml" when a certain piece of syntax
    requires two extensions to be enabled at once (e.g., immutable array
    comprehensions such as [[:x for x = 1 to 10:]]). *)
val assert_extension_enabled : loc:Location.t -> Clflags.Extension.t -> unit
