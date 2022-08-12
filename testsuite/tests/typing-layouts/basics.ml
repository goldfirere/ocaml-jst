(* TEST
   * expect
*)
type t_any   [@@any]
type t_value [@@value]
type t_imm   [@@immediate]
type t_imm64 [@@immediate64]
type t_void  [@@void];;
[%%expect{|
type t_any
type t_value
type t_imm [@@immediate]
type t_imm64 [@@immediate64]
type t_void
|}];;

(* Test 1: Reject non-value function arg/returns *)
module type S = sig
  val f : t_any -> int
end;;
[%%expect {|
Line 2, characters 10-15:
2 |   val f : t_any -> int
              ^^^^^
Error: Function argument types must have layout value.
        t_any has layout any, which is not a sublayout of value.
|}]

module type S = sig
  val f : int -> t_void
end;;
[%%expect {|
Line 2, characters 17-23:
2 |   val f : int -> t_void
                     ^^^^^^
Error: Function return types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

(* Test 2: Permit value function arg/returns *)
module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|
module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

(* Test 3: basic annotated parameters *)
type 'a [@immediate] imm_id = 'a

[%%expect{|
type 'a imm_id = 'a
|}];;

type my_int = int imm_id
let plus_3 (x : my_int) = x + 3
let plus_3' (x : int imm_id) = x + 3;;

[%%expect{|
type my_int = int imm_id
val plus_3 : my_int -> int = <fun>
val plus_3' : int imm_id -> int = <fun>
|}];;

let string_id (x : string imm_id) = x;;
[%%expect{|
Line 1, characters 19-25:
1 | let string_id (x : string imm_id) = x;;
                       ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}];;

let id_for_imms (x : 'a imm_id) = x

let three = id_for_imms 3
let true_ = id_for_imms true;;
[%%expect{|
val id_for_imms : 'a imm_id -> 'a imm_id = <fun>
val three : int imm_id = 3
val true_ : bool imm_id = true
|}]

let not_helloworld = id_for_imms "hello world";;
[%%expect{|
Line 1, characters 33-46:
1 | let not_helloworld = id_for_imms "hello world";;
                                     ^^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         'a imm_id = 'a
       string has layout value, which is not a sublayout of immediate.
|}]

(* Test 4: parameters and recursion *)
type 'a [@immediate] t4
and s4 = string t4;;

[%%expect{|
Line 2, characters 9-15:
2 | and s4 = string t4;;
             ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}];;

type s4 = string t4
and 'a [@immediate] t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}]

type s4 = int t4
and 'a [@immediate] t4;;

[%%expect{|
type s4 = int t4
and 'a t4
|}]

type s4 = s5 t4
and 'a [@immediate] t4
and s5 = int;;

[%%expect{|
type s4 = s5 t4
and 'a t4
and s5 = int
|}]

type s4 = s5 t4
and 'a [@immediate] t4
and s5 = string;;

[%%expect{|
Line 3, characters 0-15:
3 | and s5 = string;;
    ^^^^^^^^^^^^^^^
Error: This type constructor expands to type s5 = string
       but is used here with type 'a
       s5 has layout value, which is not a sublayout of immediate.
|}]

type 'a [@any] t4 = 'a
and s4 = string t4;;
[%%expect{|
type 'a t4 = 'a
and s4 = string t4
|}];;

type s4 = string t4
and 'a [@any] t4;;
[%%expect{|
type s4 = string t4
and 'a t4
|}];;

(* Test 4: You can touch a void, but not return it directly *)
type 'a [@void] void4 = Void4  of 'a

type 'a [@any] any4 = Any4 of 'a

  (* f4 and g4 here are allowed, but you can only use them on voids *)
let f4 : 'a void4 -> 'a any4 = function
    Void4 x -> Any4 x

let g4 : 'a any4 -> 'a void4 = function
  Any4 x -> Void4 x
;;

[%%expect{|
type 'a void4 = Void4 of 'a
type 'a any4 = Any4 of 'a
val f4 : 'a void4 -> 'a any4 = <fun>
val g4 : 'a any4 -> 'a void4 = <fun>
|}];;


  (* disallowed attempts to use f4 and g4 on non-voids *)
let h4 (x : int void4) = f4 x
[%%expect{|
Line 1, characters 12-15:
1 | let h4 (x : int void4) = f4 x
                ^^^
Error: This type int should be an instance of type 'a
       int has layout immediate, which is not a sublayout of void.
|}];;

let h4' (x : int any4) = g4 x
[%%expect{|
Line 1, characters 28-29:
1 | let h4' (x : int any4) = g4 x
                                ^
Error: This expression has type int any4
       but an expression was expected of type 'a any4
       int has layout immediate, which is not a sublayout of void.
|}];;

  (* disallowed - tries to return void *)
let g (x : 'a void4) =
  match x with
  | Void4 x -> x;;
[%%expect{|
Line 3, characters 15-16:
3 |   | Void4 x -> x;;
                   ^
Error: This expression has type 'a but an expression was expected of type 'a0
       'a has layout value, which does not overlap with void.
|}, Principal{|
Lines 2-3, characters 2-16:
2 | ..match x with
3 |   | Void4 x -> x..
Error: This expression has type 'a but an expression was expected of type 'a0
       'a has layout value, which does not overlap with void.
|}]
(* CJC XXX understand what's going on with Principal mode here (and improve
   error messages generally *)
