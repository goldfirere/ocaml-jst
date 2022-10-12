(* TEST
   * expect
*)

(* Tests for layouts in algebraic datatypes *)

type t_void [@@void]
type t_any [@@any]
type t_value [@@value]
type t_immediate [@@immediate];;


(* Test 1: constructor arguments may have any sort *)
type t1_void = T1_void of t_void
type t1_value = T1_value of t_value
type t1_immediate = T1_value of t_immediate

type t1_mixed1 = T1_mixed1 of t_void * t_immediate
type t1_mixed2 = T1_mixed2 of t_immediate * t_value * t_void
type t1_mixed3 = T1_mixed3 of t_value * t_immediate
[%%expect {|
type t_void [@@void]
type t_any [@@any]
type t_value [@@value]
type t_immediate [@@immediate]
type t1_void = T1_void of t_void
type t1_value = T1_value of t_value
type t1_immediate = T1_value of t_immediate
type t1_mixed1 = T1_mixed1 of t_void * t_immediate
type t1_mixed2 = T1_mixed2 of t_immediate * t_value * t_void
type t1_mixed3 = T1_mixed3 of t_value * t_immediate
|}];;

type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_void
[%%expect {|
type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_void
|}]

(* Test 2: but not the "any" layout *)
type t2_any1 = T2_any1 of t_any
[%%expect {|
Line 1, characters 26-31:
1 | type t2_any1 = T2_any1 of t_any
                              ^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t2_any2 = T2_any2 of t_immediate * t_any
[%%expect {|
Line 1, characters 40-45:
1 | type t2_any2 = T2_any2 of t_immediate * t_any
                                            ^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t2_any3 = T2_any3 of t_any * t_value
[%%expect {|
Line 1, characters 26-31:
1 | type t2_any3 = T2_any3 of t_any * t_value
                              ^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_any
[%%expect {|
Line 2, characters 0-29:
2 | and 'b t1_constraint' = t_any
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       'a t1_constraint'/2 has layout any, which is not a sublayout of <unification variable>.
       Line 2, characters 0-30:
         Definition of type t1_constraint'/1
|}]
(* CJC XXX better errors *)

(* Test 3: void allowed in records *)
type t3_void = { x : t_void }
type t3_value = { x : t_value }
type t3_immediate = { x : t_immediate }

type t3_cvoid = C of { x : t_void }
type t3_cvalue = C of { x : t_value }
type t3_cimmediate = C of { x : t_immediate }


type t3_mixed1 = { x : t_void; y : t_immediate }
type t3_mixed2 = { x : t_immediate; y : t_value; z : t_void }
type t3_mixed3 = { x : t_value; y : t_immediate }

type t3_cmixed1 = C of { x : t_void; y : t_immediate }
type t3_cmixed2 = C of { x : t_immediate; y : t_value; z : t_void }
type t3_cmixed3 = C of { x : t_value; y : t_immediate }
[%%expect {|
type t3_void = { x : t_void; }
type t3_value = { x : t_value; }
type t3_immediate = { x : t_immediate; }
type t3_cvoid = C of { x : t_void; }
type t3_cvalue = C of { x : t_value; }
type t3_cimmediate = C of { x : t_immediate; }
type t3_mixed1 = { x : t_void; y : t_immediate; }
type t3_mixed2 = { x : t_immediate; y : t_value; z : t_void; }
type t3_mixed3 = { x : t_value; y : t_immediate; }
type t3_cmixed1 = C of { x : t_void; y : t_immediate; }
type t3_cmixed2 = C of { x : t_immediate; y : t_value; z : t_void; }
type t3_cmixed3 = C of { x : t_value; y : t_immediate; }
|}]

(* Test 4: but any is not *)
type t4_any1 = { x : t_any }
[%%expect {|
Line 1, characters 21-26:
1 | type t4_any1 = { x : t_any }
                         ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t4_any2 = { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 38-43:
1 | type t4_any2 = { x : t_immediate; y : t_any }
                                          ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t4_any3 =  { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 22-27:
1 | type t4_any3 =  { x : t_any; y : t_value }
                          ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t4_cany1 = C of { x : t_any }
[%%expect {|
Line 1, characters 27-32:
1 | type t4_cany1 = C of { x : t_any }
                               ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t4_cany2 = C of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 44-49:
1 | type t4_cany2 = C of { x : t_immediate; y : t_any }
                                                ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;

type t4_cany3 = C of { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 27-32:
1 | type t4_cany3 = C of { x : t_any; y : t_value }
                               ^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not a sublayout of <unification variable>.
|}];;
