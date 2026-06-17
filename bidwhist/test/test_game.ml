open Bidwhist

(* builders *)
let card s r : Card.t = Card.Regular { suit = s; rank = r }
let bid l k : Bid.t = { Bid.level = l; kind = k }
let zero : Game.scores = { Game.north_south = 0; east_west = 0 }

let make_playing ~scores ~regime ~tally ~bid ~declarer : Game.t =
  { Game.scores;
    dealer = Player.North;
    phase = Game.Playing {
      regime;
      hands = { Game.north = []; south = []; east = []; west = [] };
      tally; bid; declarer;
      trick = { Trick.leader = declarer; plays = [] } } }

(* ---------- start_hand ---------- *)

(* Invariant: a fresh deal contains all 54 cards across the four hands and the kitty. *)
let test_start_conserves () =
  let st = Game.start_hand (Random.State.make [| 7 |]) Player.North zero in
  match st.Game.phase with
  | Game.Auction { hands; kitty; _ } ->
      let all = hands.Game.north @ hands.south @ hands.east @ hands.west @ kitty in
      Alcotest.(check bool) "all 54 present" true
        (List.sort compare all = List.sort compare Deck.full_deck)
  | _ -> Alcotest.fail "expected Auction"

(* Invariant: a fresh deal is four hands of 12, a kitty of 6, no bid yet, dealer's left to act. *)
let test_start_setup () =
  let st = Game.start_hand (Random.State.make [| 7 |]) Player.North zero in
  match st.Game.phase with
  | Game.Auction { hands; kitty; high_bid; to_act } ->
      Alcotest.(check int) "12 in a hand" 12 (List.length hands.Game.north);
      Alcotest.(check int) "6 in kitty" 6 (List.length kitty);
      Alcotest.(check bool) "no high bid" true (high_bid = None);
      Alcotest.(check bool) "left of dealer acts" true (to_act = Player.East)
  | _ -> Alcotest.fail "expected Auction"

(* ---------- legal_action ---------- *)

(* Invariant: bidding legality — pass rules, strict-dominate for others, boss (=) for the dealer. *)
let test_legal_action () =
  let high = Some (Player.North, bid Bid.Four Bid.Uptown) in
  Alcotest.(check bool) "non-dealer may pass" true
    (Game.legal_action ~high:None ~dealer:false Bid.Pass);
  Alcotest.(check bool) "dealer can't pass with no bid" false
    (Game.legal_action ~high:None ~dealer:true Bid.Pass);
  Alcotest.(check bool) "dealer may pass once bid" true
    (Game.legal_action ~high ~dealer:true Bid.Pass);
  Alcotest.(check bool) "non-dealer can't match" false
    (Game.legal_action ~high ~dealer:false (Bid.Bid (bid Bid.Four Bid.Uptown)));
  Alcotest.(check bool) "non-dealer outbids" true
    (Game.legal_action ~high ~dealer:false (Bid.Bid (bid Bid.Five Bid.Uptown)));
  Alcotest.(check bool) "dealer bosses (matches)" true
    (Game.legal_action ~high ~dealer:true (Bid.Bid (bid Bid.Four Bid.Uptown)))

(* ---------- regime_of_win ---------- *)

(* Invariant: a won bid plus a matching declaration yields the right regime. *)
let test_regime_of_win () =
  Alcotest.(check bool) "uptown + suit" true
    (Game.regime_of_win (bid Bid.Three Bid.Uptown) (Game.Suit Card.Hearts) = Card.Uptown Card.Hearts);
  Alcotest.(check bool) "notrump + high" true
    (Game.regime_of_win (bid Bid.Three Bid.NoTrump) (Game.NoTrump Game.High) = Card.NoTrumpHigh)

(* Invariant: a declaration that doesn't match the bid kind is rejected. *)
let test_regime_mismatch () =
  Alcotest.check_raises "mismatch raises"
    (Failure "regime_of_win: declaration does not match bid kind")
    (fun () -> ignore (Game.regime_of_win (bid Bid.Three Bid.Uptown) (Game.NoTrump Game.High)))

(* ---------- get_hand / set_hand ---------- *)

(* Invariant: setting a seat's hand is read back by get, and leaves other seats untouched. *)
let test_get_set_hand () =
  let hs = { Game.north = []; south = []; east = []; west = [] } in
  let c = card Card.Hearts Card.Ace in
  let hs = Game.set_hand Player.North [ c ] hs in
  Alcotest.(check bool) "get after set" true (Game.get_hand Player.North hs = [ c ]);
  Alcotest.(check bool) "neighbor untouched" true (Game.get_hand Player.South hs = [])

(* ---------- score_hand ---------- *)

let ns_score st = (st : Game.t).Game.scores.north_south

(* Invariant: making the bid scores (tricks won - 6) for the declaring team. *)
let test_score_made () =
  let st = make_playing ~scores:zero ~regime:(Card.Uptown Card.Hearts)
      ~tally:{ Game.north_south = 9; east_west = 4 }
      ~bid:(bid Bid.Three Bid.Uptown) ~declarer:Player.North in
  Alcotest.(check int) "+3 to declarers" 3 (ns_score (Game.score_hand (Random.State.make [| 0 |]) st))

(* Invariant: failing the contract sets the declaring team back by the bid's level. *)
let test_score_set () =
  let st = make_playing ~scores:zero ~regime:(Card.Uptown Card.Hearts)
      ~tally:{ Game.north_south = 8; east_west = 5 }
      ~bid:(bid Bid.Five Bid.Uptown) ~declarer:Player.North in
  Alcotest.(check int) "-5 to declarers" (-5) (ns_score (Game.score_hand (Random.State.make [| 0 |]) st))

(* Invariant: under No-Trump, the delta doubles. *)
let test_score_notrump_double () =
  let st = make_playing ~scores:zero ~regime:Card.NoTrumpHigh
      ~tally:{ Game.north_south = 9; east_west = 4 }
      ~bid:(bid Bid.Three Bid.NoTrump) ~declarer:Player.North in
  Alcotest.(check int) "+6 (doubled) to declarers" 6 (ns_score (Game.score_hand (Random.State.make [| 0 |]) st))

(* Invariant: taking all 13 tricks is Boston (+13) and ends the game for the declaring team. *)
let test_score_boston_wins () =
  let st = make_playing ~scores:zero ~regime:(Card.Uptown Card.Hearts)
      ~tally:{ Game.north_south = 13; east_west = 0 }
      ~bid:(bid Bid.Three Bid.Uptown) ~declarer:Player.North in
  match (Game.score_hand (Random.State.make [| 0 |]) st).Game.phase with
  | Game.Finished { winner } -> Alcotest.(check bool) "NS win" true (winner = Player.NorthSouth)
  | _ -> Alcotest.fail "expected Finished"

(* Invariant: a set that drops a team to -7 ends the game for their OPPONENT. *)
let test_score_loss_ends () =
  let st = make_playing
      ~scores:{ Game.north_south = -3; east_west = 0 } ~regime:(Card.Uptown Card.Hearts)
      ~tally:{ Game.north_south = 8; east_west = 5 }
      ~bid:(bid Bid.Five Bid.Uptown) ~declarer:Player.North in
  match (Game.score_hand (Random.State.make [| 0 |]) st).Game.phase with
  | Game.Finished { winner } -> Alcotest.(check bool) "EW win" true (winner = Player.EastWest)
  | _ -> Alcotest.fail "expected Finished"

let () =
  Alcotest.run "bidwhist" [
    ( "game",
      [ Alcotest.test_case "start conserves"     `Quick test_start_conserves;
        Alcotest.test_case "start setup"         `Quick test_start_setup;
        Alcotest.test_case "legal_action"        `Quick test_legal_action;
        Alcotest.test_case "regime_of_win"       `Quick test_regime_of_win;
        Alcotest.test_case "regime mismatch"     `Quick test_regime_mismatch;
        Alcotest.test_case "get/set hand"        `Quick test_get_set_hand;
        Alcotest.test_case "score made"          `Quick test_score_made;
        Alcotest.test_case "score set"           `Quick test_score_set;
        Alcotest.test_case "score notrump double" `Quick test_score_notrump_double;
        Alcotest.test_case "score boston wins"   `Quick test_score_boston_wins;
        Alcotest.test_case "score loss ends"     `Quick test_score_loss_ends ] ) ] 