type scores = { north_south: int; east_west: int }

type hand = Card.t list

type hands = { north: hand; south: hand; east: hand; west: hand }

type phase = 
| Auction of {
  hands : hands;
  kitty : Card.t list;
  high_bid : (Player.seat * Bid.t) option; (* the highest bid so far, and the player who made it *)
  to_act : Player.seat 
}
| Declaring of {
  hands : hands;
  kitty : Card.t list; (* picked up imediately after the declaring phase *)
  winner : Player.seat;
  bid : Bid.t
}
| Playing of {
  regime : Card.regime;
  hands : hands;
  tally : scores; (* per team trick count with declaring team starting at 1 *)
  trick : Trick.t; (* the current trick*)
  bid : Bid.t; (* level for the contract / set penalty *)
  declarer : Player.seat (* team_of declarer = the declaring team*)
}
| Finished of { winner : Player.team } 

type t = {
  scores : scores;
  dealer : Player.seat;
  phase : phase;
}

type notrump_dir = High | Down

type declaration =
  | Suit of Card.suit
  | NoTrump of notrump_dir

let start_hand (rng: Random.State.t) (dealer : Player.seat) (scores : scores) : t =
  (* Deal the cards and assign them to the players *)
  let { Deck.hands = (h1, h2, h3, h4); kitty } = Deck.deal rng Deck.full_deck in
  let p1 = Player.next_seat dealer in (* Clockwise order from the dealer*)
  let p2 = Player.next_seat p1 in
  let p3 = Player.next_seat p2 in
  let p4 = Player.next_seat p3 in
  let assign = [ (p1, h1); (p2, h2); (p3, h3); (p4, h4) ] in
  let hand_of p = List.assoc p assign in (* Map the player to their hand *)
  let hands =
    { north = hand_of North; south = hand_of South; east = hand_of East; west = hand_of West }
  in
  { scores;
  dealer;
  phase = Auction { hands; kitty; high_bid = None; to_act = p1 }
  }

(* Legal actions are defined in README.md. *)
let legal_action ~(high : (Player.seat * Bid.t) option) ~(dealer : bool) (action : Bid.action) : bool =
  match action with
  | Pass -> not dealer || (high <> None) (* Only dealers with no high bid cannot pass*)
  | Bid b -> 
    (match high with 
    | None -> true (* First bid can be any bid *)
    | Some (_, hb) -> 
      if dealer then Bid.compare_bid b hb >= 0 (* Dealer must bid at least the highest bid *)
      else Bid.compare_bid b hb > 0 (* Non-dealer must bid more than the highest bid *)
    )

let auction_step (t : t) (action : Bid.action) : t =
  match t.phase with
  | Auction { hands; kitty; high_bid; to_act } ->
    let dealer = (to_act = t.dealer) in
    if not (legal_action ~high:high_bid ~dealer action) then failwith "auction_step: illegal action"
    else 
      let high_bid = 
        match action with
        | Pass -> high_bid (* Pass does not change the highest bid *)
        | Bid b -> Some (to_act, b) (* legality already guaranteed this dominates or bosses *)
      in
      if dealer then (
        match high_bid with
        | Some (winner, bid) -> { t with phase = Declaring { hands; kitty; winner; bid }}
        | None -> failwith "auction_step: resolved with no bid"
        ) (* impossible since dealer must bid *)
      else { t with phase = Auction { hands; kitty; high_bid; to_act = Player.next_seat to_act }}
  | _ -> failwith "auction_step: not an auction phase"


let regime_of_win (b : Bid.t) (d : declaration): Card.regime =
  match b.Bid.kind, d with
  | Bid.Uptown, Suit s -> Card.Uptown s
  | Bid.Downtown, Suit s -> Card.Downtown s
  | Bid.NoTrump, NoTrump High -> Card.NoTrumpHigh
  | Bid.NoTrump, NoTrump Down -> Card.NoTrumpDown
  | _ -> failwith "regime_of_win: declaration does not match bid kind"


let get_hand (s : Player.seat) (hs : hands) : hand =
  match s with
  | North -> hs.north
  | South -> hs.south
  | East -> hs.east
  | West -> hs.west

let set_hand (s : Player.seat) (h : hand) (hs : hands) : hands =
  match s with
  | North -> { hs with north = h }
  | South -> { hs with south = h }
  | East -> { hs with east = h }
  | West -> { hs with west = h }

let declare (t : t) (d : declaration) (discards : Card.t list): t =
  match t.phase with
  | Declaring {hands; kitty; winner; bid} ->
    let regime = regime_of_win bid d in
    let picked_up = get_hand winner hands @ kitty in (* winner's hand now holds 18 until they discard 6 *)
    if List.length (List.sort_uniq compare discards) <> 6 
      || not (List.for_all (fun c -> List.mem c picked_up) discards) 
      then failwith "declare: invalid discards"
    else 
      let remaining = List.filter (fun c -> not (List.mem c discards)) picked_up in
      let hands = set_hand winner remaining hands in
      let tally = { north_south = if winner = North || winner = South then 1 else 0; east_west = if winner = East || winner = West then 1 else 0 } in
      let trick = { Trick.leader = winner; plays = []} in
      { t with phase = Playing { regime; hands; tally; trick; bid; declarer = winner } } 
    | _ -> failwith "declare: not a declaring phase"


let play_step (t : t) (card : Card.t): t =
  match t.phase with
    | Playing { regime; hands; tally; trick; bid; declarer } ->
      (* The player to act is the last player to play a card, or the leader if no cards have been played *)
      let player = Trick.current_player trick in
      (* The card must be a legal move *)
      if not (List.mem card (Trick.legal_moves regime (get_hand player hands) trick))
        then failwith "play_step: illegal card";
      let hand' = List.filter (fun c -> c <> card) (get_hand player hands) in
      let hands = set_hand player hand' hands in
      let trick = { trick with Trick.plays = trick.plays @ [(player, card)]} in 
      (* Check if the trick is complete *)
      if List.length trick.plays = 4 then 
        let winner = Trick.winner regime trick in
        let tally = { north_south = if winner = North || winner = South then 1 + tally.north_south 
                      else tally.north_south; 
                      east_west = if winner = East || winner = West then 1 + tally.east_west 
                      else tally.east_west } 
        in
        let trick = { Trick.leader = winner; plays = []} in (* next trick is led by the winner *)
        { t with phase = Playing { regime; hands; tally; trick; bid; declarer } }
      else
        { t with phase = Playing { regime; hands; tally; trick; bid; declarer } }
    | _ -> failwith "play_step: not a playing phase"



let hand_delta ~(regime : Card.regime) ~(tally : scores) ~(bid : Bid.t)
               ~(declarer : Player.seat) : int =
  let declaring = Player.team_of declarer in
  let declaring_tricks =
    match declaring with
    | Player.NorthSouth -> tally.north_south
    | Player.EastWest   -> tally.east_west
  in
  let level = Bid.level_to_int bid.Bid.level in
  let base =
    if declaring_tricks = 13 then 13                      (* Boston *)
    else if declaring_tricks >= 6 + level then declaring_tricks - 6   (* made *)
    else - level                                          (* set *)
  in
  if regime = Card.NoTrumpHigh || regime = Card.NoTrumpDown then 2 * base else base

let score_hand (rng: Random.State.t) (t : t) : t =
  match t.phase with
  | Playing { regime;  tally; bid; declarer; _ } ->
    let declaring_team = Player.team_of declarer in
    let delta = hand_delta ~regime ~tally ~bid ~declarer in
    let scores =
      match declaring_team with
      | NorthSouth -> { t.scores with north_south = t.scores.north_south + delta }
      | EastWest -> { t.scores with east_west   = t.scores.east_west   + delta }
    in
    let dealer = Player.next_seat t.dealer in
    (* check that only the declaring team's score changed*)
    let declaring_score = 
      match declaring_team with
      | NorthSouth -> scores.north_south
      | EastWest -> scores.east_west
    in
    if declaring_score >= 7 then
      { scores; dealer; phase = Finished { winner = declaring_team } }
    else if declaring_score <= -7 then
      { scores; dealer; phase = Finished { winner = Player.other_team declaring_team } }
    else
      start_hand rng dealer scores
  | _ -> failwith "score_hand: not a playing phase"
    