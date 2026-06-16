open Bidwhist.Bid

(* ---------- helpers ---------- *)

let sign n = compare n 0

let all_bids =
  List.concat_map
    (fun level -> List.map (fun kind -> { level; kind }) [ Uptown; Downtown; NoTrump ])
    [ Three; Four; Five; Six; Seven ]

(* ---------- bid ---------- *)

(* Invariant: bid number dominates kind — any higher level outranks any kind
   at a lower level (here: 4 Uptown > 3 NoTrump). *)
let test_number_dominates () =
  Alcotest.(check bool) "4 Uptown beats 3 NoTrump"
    true (compare_bid { level = Four; kind = Uptown } { level = Three; kind = NoTrump } > 0)

(* Invariant: at equal level, kind orders NoTrump > Downtown > Uptown. *)
let test_kind_breaks_ties () =
  Alcotest.(check bool) "5 NoTrump beats 5 Downtown"
    true (compare_bid { level = Five; kind = NoTrump } { level = Five; kind = Downtown } > 0);
  Alcotest.(check bool) "5 Downtown beats 5 Uptown"
    true (compare_bid { level = Five; kind = Downtown } { level = Five; kind = Uptown } > 0)

(* Invariant: compare_bid returns 0 exactly when bids are equal in both
   level and kind (the equality the boss rule depends on). *)
let test_equal_is_zero () =
  Alcotest.(check int) "identical bids are equal"
    0 (compare_bid { level = Six; kind = NoTrump } { level = Six; kind = NoTrump })

(* Invariant: compare_bid is a consistent ordering over all 15 bids —
   reflexive (compare a a = 0) and antisymmetric (sign (compare a b) = -sign (compare b a)). *)
let test_total_order () =
  List.iter (fun a ->
    Alcotest.(check int) "reflexive" 0 (compare_bid a a);
    List.iter (fun b ->
      Alcotest.(check int) "antisymmetric"
        (sign (compare_bid a b)) (- (sign (compare_bid b a))))
      all_bids)
    all_bids

(* ---------- runner ---------- *)

let () =
  Alcotest.run "bidwhist"
    [ ( "bid",
        [ Alcotest.test_case "number dominates" `Quick test_number_dominates;
          Alcotest.test_case "kind breaks ties" `Quick test_kind_breaks_ties;
          Alcotest.test_case "equal is zero"    `Quick test_equal_is_zero;
          Alcotest.test_case "total order"      `Quick test_total_order ] ) ]
