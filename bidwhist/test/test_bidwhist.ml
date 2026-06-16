open Bidwhist.Deck

(* Reproducible generator from a fixed seed *)
let rng_of_seed seed = Random.State.make [| seed |]

(* The canonical deck, sorted once for comparison *)
let sorted_full = List.sort compare full_deck

(* ---- concrete cases ---- *)

let test_deck_size () =
  Alcotest.(check int) "full deck has 54 cards" 54 (List.length full_deck)

let test_no_duplicates () =
  Alcotest.(check int) "no duplicate cards"
    (List.length full_deck)
    (List.length (List.sort_uniq compare full_deck))

let test_shuffle_conserves () =
  let shuffled = shuffle (rng_of_seed 42) full_deck in
  Alcotest.(check bool) "shuffle preserves the multiset of cards"
    true (List.sort compare shuffled = sorted_full)

let test_deal_sizes () =
  let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed 42) full_deck in
  Alcotest.(check int) "hand 1 has 12" 12 (List.length h1);
  Alcotest.(check int) "hand 2 has 12" 12 (List.length h2);
  Alcotest.(check int) "hand 3 has 12" 12 (List.length h3);
  Alcotest.(check int) "hand 4 has 12" 12 (List.length h4);
  Alcotest.(check int) "kitty has 6" 6 (List.length kitty)

let test_deal_conserves () =
  let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed 42) full_deck in
  let all = h1 @ h2 @ h3 @ h4 @ kitty in
  Alcotest.(check bool) "deal uses every card exactly once"
    true (List.sort compare all = sorted_full)

(* ---- property: conservation holds for ANY seed ---- *)

let prop_deal_conserves =
  QCheck.Test.make ~count:1000 ~name:"deal conserves the deck for any seed"
    QCheck.int
    (fun seed ->
       let { hands = (h1, h2, h3, h4); kitty } = deal (rng_of_seed seed) full_deck in
       List.sort compare (h1 @ h2 @ h3 @ h4 @ kitty) = sorted_full)

(* ---- runner ---- *)

let () =
  Alcotest.run "bidwhist"
    [ ( "deck",
        [ Alcotest.test_case "size"             `Quick test_deck_size;
          Alcotest.test_case "no duplicates"    `Quick test_no_duplicates;
          Alcotest.test_case "shuffle conserves" `Quick test_shuffle_conserves;
          Alcotest.test_case "deal sizes"       `Quick test_deal_sizes;
          Alcotest.test_case "deal conserves"   `Quick test_deal_conserves ] );
      ( "deck (properties)",
        List.map QCheck_alcotest.to_alcotest [ prop_deal_conserves ] ) ]