(* TEST
   * expect
*)

(* CR layouts: bring this test up to date after we have real
   unboxed literals *)

let (++) = Nativeint.add
let (--) = Nativeint.sub

let x = 1
let x = 1.
let x = 1n
let x = #1
let x = #1.
let x = #1n
let x = -#1
let x = -#1.
let x = -#1n
let x = +#1
let x = +#1.
let x = +#1n
let x = - # 1
let x = - # 1.
let x = - # 1n
let x = + # 1
let x = + # 1.
let x = + # 1n
let x = #1 + #1
let x = #1. +. #1.
let x = #1n ++ #1n
let x = #1+#1
let x = #1.+.#1.
let x = #1n++#1n
let x = #1 - #1
let x = #1. -. #1.
let x = #1n -- #1n
let x = #1-#1
let x = #1.-.#1.
let x = #1n--#1n
let x =
#1
let x =
#1.
let x =
#1n

let f x _ = x

let x = f
#1 "hi"
(* this last one is weird, but expected behavior. That #1 "hi" is a
   line-number directive. It's thus stripped out before the parser
   can see it. So the parser just sees [f].

   Note that our unboxed literals all have suffixes, so the case
   tested here cannot happen in practice. *)

let x = f
#1. "hi"
let x = f
#1n "hi"

[%%expect {|
val ( ++ ) : nativeint -> nativeint -> nativeint = <fun>
val ( -- ) : nativeint -> nativeint -> nativeint = <fun>
val x : int = 1
val x : float = 1.
val x : nativeint = 1n
val x : int = 1
val x : float = 1.
val x : nativeint = 1n
val x : int = -1
val x : float = -1.
val x : nativeint = -1n
val x : int = 1
val x : float = 1.
val x : nativeint = 1n
val x : int = -1
val x : float = -1.
val x : nativeint = -1n
val x : int = 1
val x : float = 1.
val x : nativeint = 1n
val x : int = 2
val x : float = 2.
val x : nativeint = 2n
val x : int = 2
val x : float = 2.
val x : nativeint = 2n
val x : int = 0
val x : float = 0.
val x : nativeint = 0n
val x : int = 0
val x : float = 0.
val x : nativeint = 0n
val x : int = 1
val x : float = 1.
val x : nativeint = 1n
val f : 'a -> 'b -> 'a = <fun>
val x : 'a -> 'b -> 'a = <fun>
val x : float = 1.
val x : nativeint = 1n
|}]
