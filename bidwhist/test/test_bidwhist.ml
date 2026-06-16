open Bidwhist.Deck
open Bidwhist.Bid

(* ---------- helpers ---------- *)

let rng_of_seed seed = Random.State.make [| seed |]
let sorted_full = List.sort compare full_deck
let sign n = compare n 0

let all_bids =
  List.concat_map
    (fun level -> List.map (fun kind -> { level; kind }) [ Uptown; Downtown; NoTrump ])
    [ Three; Four; Five; Six; Seven ]

(* ---------- deck ---------- *)

(* Invariant: the deck has exactly 54 cards (52 ranked + 2 jokers). *)
let test_deck_size () =
  Alcotest.(check int) "full deck has 54 cards" 54 (List.length full_deck)

(* Invariant: every card in the deck is distinct (no card appears twice). *)
let test_no_duplicates () =
  Alcotest.(check int) "no duplicate cards"
    (List.length full_deck)
    (List.length (List.sort_uniq compare full_deck))

(* Invariant: shuffle is a permutation — it reorders the deck without
   adding, dropping, or duplicating any card (output multiset = input multiset). *)
let test_shuffle_conserves () =
  let shuffled = shuffle (rng_of_seed 42) full_deck in
  Alcotest.(check bool) "shuffle preserves the multiset of cards"
    true (List.sort compare shuffled = sorted_full)

(* Invariant: deal partitions the deck into four hands of 12 and a kitty of 6. *)
let test_deal_sizes () =
  let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed 42) full_deck in
  Alcotest.(check int) "hand 1 has 12" 12 (List.length h1);
  Alcotest.(check int) "hand 2 has 12" 12 (List.length h2);
  Alcotest.(check int) "hand 3 has 12" 12 (List.length h3);
  Alcotest.(check int) "hand 4 has 12" 12 (List.length h4);
  Alcotest.(check int) "kitty has 6" 6 (List.length kitty)

(* Invariant: deal conserves the deck — the four hands and the kitty together
   are exactly full_deck, with no card lost and none duplicated. *)
let test_deal_conserves () =
  let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed 42) full_deck in
  let all = h1 @ h2 @ h3 @ h4 @ kitty in
  Alcotest.(check bool) "deal uses every card exactly once"
    true (List.sort compare all = sorted_full)

(* Invariant: deal conservation holds for EVERY seed, not just a chosen one. *)
let prop_deal_conserves =
  QCheck.Test.make ~count:1000 ~name:"deal conserves the deck for any seed"
    QCheck.int
    (fun seed ->
       let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed seed) full_deck in
       List.sort compare (h1 @ h2 @ h3 @ h4 @ kitty) = sorted_full)

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
    [ ( "deck",
        [ Alcotest.test_case "size"              `Quick test_deck_size;
          Alcotest.test_case "no duplicates"     `Quick test_no_duplicates;
          Alcotest.test_case "shuffle conserves" `Quick test_shuffle_conserves;
          Alcotest.test_case "deal sizes"        `Quick test_deal_sizes;
          Alcotest.test_case "deal conserves"    `Quick test_deal_conserves ] );
      ( "deck (properties)",
        List.map QCheck_alcotest.to_alcotest [ prop_deal_conserves ] );
      ( "bid",
        [ Alcotest.test_case "number dominates" `Quick test_number_dominates;
          Alcotest.test_case "kind breaks ties" `Quick test_kind_breaks_ties;
          Alcotest.test_case "equal is zero"    `Quick test_equal_is_zero;
          Alcotest.test_case "total order"      `Quick test_total_order ] ) ]