open Bidwhist
open Trick
open Player

(* builders *)
let card suit rank : Card.t = Card.Regular { suit; rank }
let big = Card.BigJoker
let little = Card.LittleJoker
let mk leader plays : Trick.t = { leader; plays }
let same_cards a b = List.sort compare a = List.sort compare b

(* ---------- led_suit ---------- *)

(* Invariant: an empty trick has no led suit. *)
let test_led_empty () =
  Alcotest.(check bool) "no plays -> None" true (led_suit (mk North []) = None)

(* Invariant: the led suit is the suit of the first card when it's a real card. *)
let test_led_normal () =
  let tr = mk North [ (North, card Card.Hearts Card.Ten); (East, card Card.Spades Card.Two) ] in
  Alcotest.(check bool) "first card sets led" true (led_suit tr = Some Card.Hearts)

(* Invariant: when a joker leads, the led suit is the first NON-joker card's suit. *)
let test_led_joker_then_suit () =
  let tr = mk North [ (North, big); (East, card Card.Clubs Card.Five) ] in
  Alcotest.(check bool) "joker led, first non-joker sets led" true (led_suit tr = Some Card.Clubs)

(* Invariant: a trick of only jokers has no led suit yet. *)
let test_led_only_jokers () =
  let tr = mk North [ (North, big); (East, little) ] in
  Alcotest.(check bool) "only jokers -> None" true (led_suit tr = None)

(* ---------- legal_moves ---------- *)

(* Invariant: holding the led suit forces you to follow it (no jokers, no off-suit). *)
let test_must_follow () =
  let tr = mk North [ (North, card Card.Hearts Card.Ace) ] in
  let hand = [ card Card.Hearts Card.Two; card Card.Clubs Card.Five; big ] in
  Alcotest.(check bool) "must follow led" true
    (same_cards (legal_moves (Card.Uptown Card.Spades) hand tr) [ card Card.Hearts Card.Two ])

(* Invariant: void of the led suit in a trump regime -> any card is legal. *)
let test_void_trump_anything () =
  let tr = mk North [ (North, card Card.Hearts Card.Ace) ] in
  let hand = [ card Card.Clubs Card.Five; big ] in
  Alcotest.(check bool) "void in trump -> whole hand" true
    (same_cards (legal_moves (Card.Uptown Card.Spades) hand tr) hand)

(* Invariant: in No-Trump, void of led suit while holding a joker -> must play a joker. *)
let test_notrump_forced_joker () =
  let tr = mk North [ (North, card Card.Hearts Card.Ace) ] in
  let hand = [ card Card.Clubs Card.Five; big ] in
  Alcotest.(check bool) "NoTrump void + joker -> joker only" true
    (same_cards (legal_moves Card.NoTrumpHigh hand tr) [ big ])

(* Invariant: in No-Trump, void of led suit with no joker -> any card is legal. *)
let test_notrump_void_no_joker () =
  let tr = mk North [ (North, card Card.Hearts Card.Ace) ] in
  let hand = [ card Card.Clubs Card.Five; card Card.Diamonds Card.Two ] in
  Alcotest.(check bool) "NoTrump void no joker -> whole hand" true
    (same_cards (legal_moves Card.NoTrumpHigh hand tr) hand)

(* Invariant: when trump is led, a joker counts as the led suit and must follow. *)
let test_trump_led_joker_follows () =
  let tr = mk North [ (North, card Card.Hearts Card.Ace) ] in  (* hearts trump, trump led *)
  let hand = [ big; card Card.Clubs Card.Two ] in
  Alcotest.(check bool) "trump led -> joker must follow" true
    (same_cards (legal_moves (Card.Uptown Card.Hearts) hand tr) [ big ])

(* Invariant: the leader (empty trick) may play any card in hand. *)
let test_leader_anything () =
  let tr = mk North [] in
  let hand = [ card Card.Hearts Card.Two; big ] in
  Alcotest.(check bool) "leader -> whole hand" true
    (same_cards (legal_moves (Card.Uptown Card.Spades) hand tr) hand)

(* ---------- winner ---------- *)

(* Invariant: with no trump played, the highest card of the led suit wins. *)
let test_winner_high_of_led () =
  let tr = mk North
    [ (North, card Card.Hearts Card.Ten); (East, card Card.Hearts Card.Ace);
      (South, card Card.Hearts Card.Two); (West, card Card.Clubs Card.King) ] in
  Alcotest.(check bool) "highest of led wins" true (winner Card.NoTrumpHigh tr = East)

(* Invariant: any trump beats the highest card of the led suit. *)
let test_winner_trump_beats_led () =
  let tr = mk North
    [ (North, card Card.Hearts Card.Ace); (East, card Card.Spades Card.Two);
      (South, card Card.Hearts Card.King); (West, card Card.Clubs Card.Five) ] in
  Alcotest.(check bool) "trump beats led" true (winner (Card.Uptown Card.Spades) tr = East)

(* Invariant: the big joker is the top trump in a trump regime. *)
let test_winner_big_joker () =
  let tr = mk North
    [ (North, card Card.Hearts Card.Ace); (East, big);
      (South, little); (West, card Card.Hearts Card.Two) ] in
  Alcotest.(check bool) "big joker tops trump" true (winner (Card.Uptown Card.Hearts) tr = East)

(* Invariant: under Downtown, rank inverts — the 2 outranks the Ace in the led suit. *)
let test_winner_downtown () =
  let tr = mk North
    [ (North, card Card.Clubs Card.Ace); (East, card Card.Clubs Card.Two);
      (South, card Card.Clubs Card.Five); (West, card Card.Diamonds Card.King) ] in
  Alcotest.(check bool) "downtown: 2 beats ace" true (winner (Card.Downtown Card.Hearts) tr = East)

(* Invariant: in No-Trump, a joker is dead and never wins a trick. *)
let test_winner_notrump_joker_dead () =
  let tr = mk North
    [ (North, card Card.Hearts Card.King); (East, big);
      (South, card Card.Hearts Card.Ace); (West, card Card.Hearts Card.Two) ] in
  Alcotest.(check bool) "joker dead in NoTrump" true (winner Card.NoTrumpHigh tr = South)

(* ---------- suite ---------- *)

let () =
  Alcotest.run "bidwhist"
    [ ( "trick",
        [ Alcotest.test_case "led empty"               `Quick test_led_empty;
          Alcotest.test_case "led normal"              `Quick test_led_normal;
          Alcotest.test_case "led joker then suit"     `Quick test_led_joker_then_suit;
          Alcotest.test_case "led only jokers"         `Quick test_led_only_jokers;
          Alcotest.test_case "must follow"             `Quick test_must_follow;
          Alcotest.test_case "void trump anything"     `Quick test_void_trump_anything;
          Alcotest.test_case "notrump forced joker"    `Quick test_notrump_forced_joker;
          Alcotest.test_case "notrump void no joker"   `Quick test_notrump_void_no_joker;
          Alcotest.test_case "trump led joker follows" `Quick test_trump_led_joker_follows;
          Alcotest.test_case "leader anything"         `Quick test_leader_anything;
          Alcotest.test_case "winner high of led"      `Quick test_winner_high_of_led;
          Alcotest.test_case "winner trump beats led"  `Quick test_winner_trump_beats_led;
          Alcotest.test_case "winner big joker"        `Quick test_winner_big_joker;
          Alcotest.test_case "winner downtown"         `Quick test_winner_downtown;
          Alcotest.test_case "winner notrump joker dead" `Quick test_winner_notrump_joker_dead ] ) ]