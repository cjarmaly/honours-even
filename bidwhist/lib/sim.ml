(* A random-legal-move bot and a driver that plays full games *)

let hand_over (hs : Game.hands) =
  hs.Game.north = [] && hs.Game.south = [] && hs.Game.east = [] && hs.Game.west = []

let pick rng lst = List.nth lst (Random.State.int rng (List.length lst))


(* Advance the game by one randomly chosen legal decision *)
let random_step (rng: Random.State.t) (t: Game.t) : Game.t =
  match t.Game.phase with
  | Game.Auction { high_bid; to_act; _ } ->
    let dealer = (to_act = t.Game.dealer) in
    let actions =
      (if Game.legal_action ~high:high_bid ~dealer Bid.Pass then [ Bid.Pass ] else [])
      @ (List.filter_map
         (fun b -> if Game.legal_action ~high:high_bid ~dealer (Bid.Bid b) then Some (Bid.Bid b) else None)
         Bid.all_bids)
    in
    Game.auction_step t (pick rng actions)
  | Game.Playing { regime; hands; trick; _ } ->
    if hand_over hands then Game.score_hand rng t
    else 
      let player = Trick.current_player trick in
      let legal = Trick.legal_moves regime (Game.get_hand player hands) trick in
      Game.play_step t (pick rng legal)
  | Game.Declaring {hands; kitty; winner; bid } ->
    let declaration =
      match bid.Bid.kind with
      | Bid.Uptown | Bid.Downtown -> Game.Suit (pick rng [Card.Clubs; Card.Diamonds; Card.Hearts; Card.Spades])
      | Bid.NoTrump -> Game.NoTrump (pick rng [Game.High; Game.Down])
    in
    let pile = Game.get_hand winner hands @ kitty in (* winner's hand now holds 18 until they discard 6 *)
    let discards = List.filteri (fun i _ -> i < 6) (Deck.shuffle rng pile) in
    Game.declare t declaration discards
  | Game.Finished _ -> t

let rec play_game (rng: Random.State.t) (t: Game.t) : Game.t =
  match t.Game.phase with
  | Game.Finished _ -> t
  | _ -> play_game rng (random_step rng t)
