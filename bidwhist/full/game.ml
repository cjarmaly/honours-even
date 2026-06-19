open Bidwhist

(* a declaration mid-flight: regime chosen, cards discarded so far *)
type pending = { decl : Game.declaration; discarded : Card.t list }

type astate = { game : Game.t; pending : pending option }

(* the uniform action type spanning all four kinds of decision *)
type action =
  | Bid of Bid.action        (* the auction *)
  | Declare of Game.declaration  (* choose the regime: Suit s | NoTrump dir *)
  | Discard of Card.t            (* one discarded card (six of these, in sequence) *)
  | Play of Card.t            (* a card into a trick *)


let current_player (s : astate) : Player.seat =
  match s.game.Game.phase with
  | Game.Auction { to_act; _ } -> to_act
  | Game.Declaring { winner; _ } -> winner   (* declarer picks regime + discards *)
  | Game.Playing { trick; _ }  -> Trick.current_player trick
  | Game.Finished  _  -> failwith "current_player: hand is over"

let is_terminal (s : astate) : bool =
  match s.game.Game.phase with
  | Game.Playing { hands; _ } ->
      hands.Game.north = [] && hands.east = [] && hands.south = [] && hands.west = []
  | _ -> false

let payoff (s : astate) (seat : Player.seat) : float =
  match s.game.Game.phase with
  | Game.Playing { regime; tally; bid; declarer; _ } ->
      let delta = Game.hand_delta ~regime ~tally ~bid ~declarer in
      if Player.team_of seat = Player.team_of declarer
      then float_of_int delta
      else float_of_int (- delta)
  | _ -> failwith "payoff: not a finished hand"


  let legal_actions (s : astate) : action list =
    match s.game.Game.phase, s.pending with
    | Game.Auction { high_bid; to_act; _ }, _ ->
        let dealer = (to_act = s.game.Game.dealer) in
        let pass = if Game.legal_action ~high:high_bid ~dealer Bid.Pass
                   then [ Bid Bid.Pass ] else [] in
        let bids =
          List.filter_map
            (fun b -> if Game.legal_action ~high:high_bid ~dealer (Bid.Bid b)
                      then Some (Bid (Bid.Bid b)) else None)
            Bid.all_bids
        in
        pass @ bids
    | Game.Declaring { bid; _ }, None ->            (* choose the regime; options depend on bid kind *)
        (match bid.Bid.kind with
         | Bid.Uptown | Bid.Downtown ->
             List.map (fun su -> Declare (Game.Suit su))
               [ Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades ]
         | Bid.NoTrump ->
             [ Declare (Game.NoTrump Game.High); Declare (Game.NoTrump Game.Down) ])
    | Game.Declaring { hands; kitty; winner; _ }, Some p ->   (* discard one of the remaining 18 *)
        let pile = Game.get_hand winner hands @ kitty in
        List.filter (fun c -> not (List.mem c p.discarded)) pile
        |> List.map (fun c -> Discard c)
    | Game.Playing { regime; hands; trick; _ }, _ ->
        let cur = Trick.current_player trick in
        Trick.legal_moves regime (Game.get_hand cur hands) trick
        |> List.map (fun c -> Play c)
    | Game.Finished _, _ -> []


let apply (s : astate) (a : action) : astate =
  match s.game.Game.phase, s.pending, a with
  | Game.Auction _, _, Bid ba ->
      { s with game = Game.auction_step s.game ba }
  | Game.Declaring _, None, Declare decl ->
      { s with pending = Some { decl; discarded = [] } }   (* record regime, don't declare yet *)
  | Game.Declaring _, Some p, Discard c ->
      let discarded = c :: p.discarded in
      if List.length discarded = 6
      then { game = Game.declare s.game p.decl discarded; pending = None }  (* 6 in: declare for real *)
      else { s with pending = Some { p with discarded } }
  | Game.Playing _, _, Play c ->
      { s with game = Game.play_step s.game c }
  | _ -> failwith "apply: action does not match phase"

let initial (rng : Random.State.t) : astate =
  { game = Game.start_hand rng Player.North { Game.north_south = 0; east_west = 0 };
    pending = None }