open Bidwhist

(* a declaration mid-flight: regime chosen, cards discarded so far *)
type pending = { decl : Game.declaration; discarded : Card.t list }

type astate = { game : Game.t; pending : pending option }

type abstract_play = Follow_lo | Follow_hi | Trump_lo | Trump_hi | Off_lo | Off_hi

let trump_of = function Card.Uptown s | Card.Downtown s -> Some s | _ -> None
let is_trump regime = function
  | Card.BigJoker | Card.LittleJoker ->
      (match regime with Card.NoTrumpHigh | Card.NoTrumpDown -> false | _ -> true)
  | Card.Regular { suit; _ } ->
      (match trump_of regime with Some t -> suit = t | None -> false)
let of_suit su = function Card.Regular { suit; _ } -> suit = su | _ -> false

(* a weak-to-strong sort key for any card (low = weak), regime-aware *)
let rank_int = function
  | Card.Two->2|Card.Three->3|Card.Four->4|Card.Five->5|Card.Six->6|Card.Seven->7
  | Card.Eight->8|Card.Nine->9|Card.Ten->10|Card.Jack->11|Card.Queen->12|Card.King->13|Card.Ace->14
let weak_key regime = function
  | Card.BigJoker -> 100 | Card.LittleJoker -> 99
  | Card.Regular { rank; _ } ->
      let v = rank_int rank in
      (match regime with Card.Downtown _ | Card.NoTrumpDown -> 16 - v | _ -> v)

let cat_of regime led c =
  match led with
  | None   -> if is_trump regime c then `T else `O
  | Some l -> if of_suit l c then `F else if is_trump regime c then `T else `O

(* derived from the ENGINE-legal cards, so it can only offer real moves *)
let legal_plays regime led (legal : Card.t list) : abstract_play list =
  let has cat = List.exists (fun c -> cat_of regime led c = cat) legal in
  (if has `F then [ Follow_lo; Follow_hi ] else [])
  @ (if has `T then [ Trump_lo; Trump_hi ] else [])
  @ (if has `O then [ Off_lo;   Off_hi   ] else [])

let translate regime led (legal : Card.t list) ap : Card.t =
  let want = match ap with
    | Follow_lo | Follow_hi -> `F | Trump_lo | Trump_hi -> `T | Off_lo | Off_hi -> `O in
  let cards = List.filter (fun c -> cat_of regime led c = want) legal in
  let sorted = List.sort (fun a b -> compare (weak_key regime a) (weak_key regime b)) cards in
  match ap with
  | Follow_lo | Trump_lo | Off_lo -> List.hd sorted
  | Follow_hi | Trump_hi | Off_hi -> List.hd (List.rev sorted)


(* the uniform action type spanning all four kinds of decision *)
type action =
  | Bid of Bid.action        (* the auction *)
  | Declare of Game.declaration  (* choose the regime: Suit s | NoTrump dir *)
  | Discard of Card.t            (* one discarded card (six of these, in sequence) *)
  | Play of abstract_play            (* a card into a trick *)


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
    let legal = Trick.legal_moves regime (Game.get_hand cur hands) trick in
    legal_plays regime (Trick.led_suit trick) legal |> List.map (fun ap -> Play ap)
  | Game.Finished _, _ -> []

let auto_discard regime pile =
  let nt = List.filter (fun c -> not (is_trump regime c)) pile in
  let suit_len su = List.length (List.filter (of_suit su) nt) in
  let dkey c =
    if is_trump regime c then (3, 0, 0)                 (* never discard a trump *)
    else
      let honor = if weak_key regime c >= 12 then 1 else 0 in   (* keep honors *)
      let len = (match c with Card.Regular { suit; _ } -> suit_len suit | _ -> 9) in
      (honor, len, weak_key regime c)                  (* else: short suit first, low rank first *)
  in
  List.sort (fun a b -> compare (dkey a) (dkey b)) pile
  |> List.filteri (fun i _ -> i < 6)

let apply (s : astate) (a : action) : astate =
  match s.game.Game.phase, s.pending, a with
  | Game.Auction _, _, Bid ba ->
      { s with game = Game.auction_step s.game ba }
  | Game.Declaring { hands; kitty; winner; bid; _ }, None, Declare decl ->
      let regime   = Game.regime_of_win bid decl in
      let pile     = Game.get_hand winner hands @ kitty in
      let discards = auto_discard regime pile in
      { game = Game.declare s.game decl discards; pending = None }
  | Game.Declaring _, Some p, Discard c ->
      let discarded = c :: p.discarded in
      if List.length discarded = 6
      then { game = Game.declare s.game p.decl discarded; pending = None }  (* 6 in: declare for real *)
      else { s with pending = Some { p with discarded } }
  | Game.Playing { regime; hands; trick; _ }, _, Play ap ->
      let cur = Trick.current_player trick in
      let legal = Trick.legal_moves regime (Game.get_hand cur hands) trick in
      { s with game = Game.play_step s.game (translate regime (Trick.led_suit trick) legal ap) }
  | _ -> failwith "apply: action does not match phase"

let initial (rng : Random.State.t) : astate =
  { game = Game.start_hand rng Player.North { Game.north_south = 0; east_west = 0 };
    pending = None }
