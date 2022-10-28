(* TEST
   flags = "-extension immutable_arrays"
   * expect
*)

(** Create some immutable and mutable arrays *)

let iarray  : int   iarray = [:1;2;3;4;5:];;
let ifarray : float iarray = [:1.5;2.5;3.5;4.5;5.5:];;

let marray  : int   array = [|1;2;3;4;5|];;
let mfarray : float array = [|1.5;2.5;3.5;4.5;5.5|];;

[%%expect{|
val iarray : int iarray = [:1; 2; 3; 4; 5:]
val ifarray : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
val marray : int array = [|1; 2; 3; 4; 5|]
val mfarray : float array = [|1.5; 2.5; 3.5; 4.5; 5.5|]
|}];;

(** Pattern-match on some immutable arrays, and check the typing of array
    patterns, both mutable and immutable *)

match iarray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "1--5"
|}];;

match ifarray with
| [::]                    -> "empty"
| [:1.5;2.5;3.5;4.5;5.5:] -> "1.5--5.5"
| _                       -> "who knows?"
;;
[%%expect{|
- : string = "1.5--5.5"
|}];;

match iarray with
| [::]          -> "empty"
| [:1;2;3;4;6:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "who knows?"
|}];;

match ifarray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 3, characters 4-5:
3 | | [:1;2;3;4;5:] -> "1--5"
        ^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type float
  Hint: Did you mean `1.'?
|}];;

match marray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-6:
2 | | [::]          -> "empty"
      ^^^^
Error: This pattern matches values of type 'a iarray
       but a pattern was expected which matches values of type int array
|}];;

match iarray with
| [||]          -> "empty"
| [|1;2;3;4;5|] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-6:
2 | | [||]          -> "empty"
      ^^^^
Error: This pattern matches values of type 'a array
       but a pattern was expected which matches values of type int iarray
|}];;

(** Confirm that immutable and mutable arrays have the same representation, even
    when they're different objects *)

Obj.repr iarray = Obj.repr marray;;
[%%expect{|
- : bool = true
|}];;

Obj.repr ifarray = Obj.repr mfarray;;
[%%expect{|
- : bool = true
|}];;

iarray == Obj.magic marray;;
[%%expect{|
- : bool = false
|}];;

ifarray == Obj.magic mfarray;;
[%%expect{|
- : bool = false
|}];;

(** Confirm that immutable and mutable arrays don't collide *)

Obj.repr iarray <> Obj.repr ifarray;;
[%%expect{|
- : bool = true
|}];;

Obj.repr marray <> Obj.repr mfarray;;
[%%expect{|
- : bool = true
|}];;

(** Test basic functionality: One or a few tests for every function in [Iarray].
    We test both success and error cases, and in general try to have coverage of
    edge cases.  Comments are attached everywhere something subtle is being
    checked. *)

Iarray.length iarray, Iarray.length ifarray;;
[%%expect{|
- : int * int = (5, 5)
|}];;

iarray.:(0), Iarray.get iarray 1, ifarray.:(2), Iarray.get ifarray 3;;
[%%expect{|
- : int * int * float * float = (1, 2, 3.5, 4.5)
|}];;

iarray.:(10)
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.get iarray (-1);;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

ifarray.:(-10);;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.get ifarray 5;;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.make 7 "tada";;
[%%expect{|
- : string iarray =
[:"tada"; "tada"; "tada"; "tada"; "tada"; "tada"; "tada":]
|}];;

Iarray.make 10 1.5;;
[%%expect{|
- : float iarray = [:1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5; 1.5:]
|}];;

Iarray.create_float 10;;
[%%expect{|
- : float iarray = [:0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.:]
|}];;

Iarray.init 10 (fun x -> x * 2);;
[%%expect{|
- : int iarray = [:0; 2; 4; 6; 8; 10; 12; 14; 16; 18:]
|}];;

Iarray.make_matrix 3 10 ();;
[%%expect{|
- : unit iarray iarray =
[:[:(); (); (); (); (); (); (); (); (); ():];
  [:(); (); (); (); (); (); (); (); (); ():];
  [:(); (); (); (); (); (); (); (); (); ():]:]
|}];;

Iarray.append iarray iarray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5; 1; 2; 3; 4; 5:]
|}];;

Iarray.concat [];;
[%%expect{|
- : 'a iarray = [::]
|}];;

Iarray.concat [Iarray.make 1 'a'; Iarray.make 2 'b'; Iarray.make 3 'c'];;
[%%expect{|
- : char iarray = [:'a'; 'b'; 'b'; 'c'; 'c'; 'c':]
|}];;

Iarray.sub iarray 0 2, Iarray.sub iarray 2 3;;
[%%expect{|
- : int iarray * int iarray = ([:1; 2:], [:3; 4; 5:])
|}];;

Iarray.sub iarray (-1) 3;;
[%%expect{|
Exception: Invalid_argument "Array.sub".
|}];;

Iarray.sub iarray 1 (-3);;
[%%expect{|
Exception: Invalid_argument "Array.sub".
|}];;

Iarray.sub iarray 3 10;;
[%%expect{|
Exception: Invalid_argument "Array.sub".
|}];;

Iarray.to_list iarray;;
[%%expect{|
- : int list = [1; 2; 3; 4; 5]
|}];;

Iarray.of_list [10;20;30];;
[%%expect{|
- : int iarray = [:10; 20; 30:]
|}];;


Iarray.to_array iarray;;
[%%expect{|
- : int array = [|1; 2; 3; 4; 5|]
|}];;

Iarray.of_array mfarray;;
[%%expect{|
- : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
|}];;

(* [Array] has an analog to [Iarray.to_array] *)
Array.of_iarray ifarray;;
[%%expect{|
- : float array = [|1.5; 2.5; 3.5; 4.5; 5.5|]
|}];;

(* [Array] has an analog to [Iarray.of_array] *)
Array.to_iarray marray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5:]
|}];;

(* [Iarray.to_array] creates a fresh mutable array every time *)
Iarray.to_array iarray == marray;;
[%%expect{|
- : bool = false
|}];;

(* [Iarray.to_array] creates a fresh mutable array every time *)
Iarray.to_array ifarray == Iarray.to_array ifarray;;
[%%expect{|
- : bool = false
|}];;

(* Round-tripping from and to an [iarray] creates a fresh copy every time *)
Iarray.of_array (Iarray.to_array iarray) == iarray;;
[%%expect{|
- : bool = false
|}];;

let sum = ref 0. in
Iarray.iter (fun x -> sum := !sum +. x) ifarray;
!sum;;
[%%expect{|
- : float = 17.5
|}];;

let total = ref 0 in
Iarray.iteri (fun i x -> total := !total + i*x) iarray;
!total;;
[%%expect{|
- : int = 40
|}];;

Iarray.map Int.neg iarray;;
[%%expect{|
- : int iarray = [:-1; -2; -3; -4; -5:]
|}];;

Iarray.mapi (fun i x -> i, 10.*.x) ifarray;;
[%%expect{|
- : (int * float) iarray =
[:(0, 15.); (1, 25.); (2, 35.); (3, 45.); (4, 55.):]
|}];;

Iarray.fold_left (fun acc x -> -x :: acc) [] iarray;;
[%%expect{|
- : int list = [-5; -4; -3; -2; -1]
|}];;

Iarray.fold_right (fun x acc -> -.x :: acc) ifarray [];;
[%%expect{|
- : float list = [-1.5; -2.5; -3.5; -4.5; -5.5]
|}];;

let ints   = ref 0  in
let floats = ref 0. in
Iarray.iter2
  (fun i f ->
    ints   := i +  !ints;
    floats := f +. !floats)
  iarray
  ifarray;
!ints, !floats;;
[%%expect{|
- : int * float = (15, 17.5)
|}];;

Iarray.map2 (fun i f -> f, i) iarray ifarray;;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.for_all (fun i -> i > 0) iarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all (fun f -> f < 5.) ifarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.exists (fun f -> f < 5.) ifarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.exists (fun i -> i > 10) iarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all2 (fun i f -> Float.of_int i < f) iarray ifarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all2 (fun f i -> i = 1 && f = 1.5) ifarray iarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.exists2 (fun f i -> Float.of_int i +. f = 8.5) ifarray iarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.exists2 (fun i f -> Float.of_int i > f) iarray ifarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.mem 3 iarray, Iarray.mem 3.5 ifarray;;
[%%expect{|
- : bool * bool = (true, true)
|}];;

Iarray.mem 30 iarray, Iarray.mem 35. ifarray;;
[%%expect{|
- : bool * bool = (false, false)
|}];;

let x = ref 0 in
Iarray.memq x (Iarray.make 3 x);;
[%%expect{|
- : bool = true
|}];;

Iarray.memq (ref 0) (Iarray.make 3 (ref 0))
[%%expect{|
- : bool = false
|}];;

Iarray.to_seq iarray |> List.of_seq;;
[%%expect{|
- : int list = [1; 2; 3; 4; 5]
|}];;

Iarray.to_seqi ifarray |> List.of_seq;;
[%%expect{|
- : (int * float) list = [(0, 1.5); (1, 2.5); (2, 3.5); (3, 4.5); (4, 5.5)]
|}];;

["hello"; "world"] |> List.to_seq |> Iarray.of_seq;;
[%%expect{|
- : string iarray = [:"hello"; "world":]
|}];;

(** Confirm that we haven't edited the immutable arrays, and that editing
    mutable siblings or copies does nothing *)

Array.fill marray 0 3 0;
marray;;
[%%expect{|
- : int array = [|0; 0; 0; 4; 5|]
|}];;

Array.fill (Iarray.to_array iarray) 3 2 10;
iarray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5:]
|}];;

Array.fill mfarray 3 2 0.;
mfarray;;
[%%expect{|
- : float array = [|1.5; 2.5; 3.5; 0.; 0.|]
|}];;

Array.fill (Iarray.to_array ifarray) 0 3 10.;
ifarray;;
[%%expect{|
- : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
|}];;
