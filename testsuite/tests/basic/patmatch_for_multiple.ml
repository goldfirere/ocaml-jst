(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let
  (*match*/270 = 3
   *match*/271 = 2
   *match*/272 = 1
   *match*/273 = *match*/270
   *match*/274 = *match*/271
   *match*/275 = *match*/272)
  (catch
    (catch
      (catch (if (!= *match*/274 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/273 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/278 = 3
   *match*/279 = 2
   *match*/280 = 1
   *match*/281 = (makeblock 0 *match*/278 *match*/279 *match*/280))
  (catch
    (catch
      (let (*match*/282 =a (field 0 *match*/281))
        (catch
          (let (*match*/283 =a (field 1 *match*/281))
            (if (!= *match*/283 3) (exit 7)
              (let (*match*/284 =a (field 2 *match*/281))
                (exit 5 *match*/281))))
         with (7)
          (if (!= *match*/282 1) (exit 6)
            (let
              (*match*/286 =a (field 2 *match*/281)
               *match*/285 =a (field 1 *match*/281))
              (exit 5 *match*/281)))))
     with (6) 0)
   with (5 x/276[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/276) 1)))
- : bool = false
|}];;
