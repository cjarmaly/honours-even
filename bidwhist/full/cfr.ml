module Card = Bidwhist.Card
module Bid = Bidwhist.Bid
module Player = Bidwhist.Player
module Trick = Bidwhist.Trick
module Eng = Bidwhist.Game     (* the ENGINE; bare `Game` = the Full adapter *)

let bucket (hand : Card.t list) : string =
  let suit_count su =
    List.length (List.filter (function Card.Regular { suit; _ } -> suit = su | _ -> false) hand) in
  let honors =
    List.length (List.filter
      (function Card.Regular { rank = (Card.Ace | Card.King | Card.Queen); _ } -> true | _ -> false) hand) in
  let jokers =
    List.length (List.filter (function Card.BigJoker | Card.LittleJoker -> true | _ -> false) hand) in
  Printf.sprintf "%d%d%d%d-h%d-j%d"
    (suit_count Card.Hearts) (suit_count Card.Diamonds)
    (suit_count Card.Clubs)  (suit_count Card.Spades) honors jokers


let suit_str = function Card.Hearts->"H" | Card.Diamonds->"D" | Card.Clubs->"C" | Card.Spades->"S"
let rank_str = function
  | Card.Two->"2"|Card.Three->"3"|Card.Four->"4"|Card.Five->"5"|Card.Six->"6"|Card.Seven->"7"
  | Card.Eight->"8"|Card.Nine->"9"|Card.Ten->"T"|Card.Jack->"J"|Card.Queen->"Q"|Card.King->"K"|Card.Ace->"A"
let card_str = function
  | Card.Regular { suit; rank } -> rank_str rank ^ suit_str suit
  | Card.BigJoker -> "x" | Card.LittleJoker -> "y"
let seat_str = function Player.North->"N"|Player.East->"E"|Player.South->"O"|Player.West->"W"
let kind_str = function Bid.Uptown->"u"|Bid.Downtown->"d"|Bid.NoTrump->"n"
let bid_str (b : Bid.t) = string_of_int (Bid.level_to_int b.Bid.level) ^ kind_str b.Bid.kind
let high_str = function None -> "_" | Some (st, b) -> seat_str st ^ bid_str b
let regime_str = function
  | Card.Uptown s -> "u"^suit_str s | Card.Downtown s -> "d"^suit_str s
  | Card.NoTrumpHigh -> "nh" | Card.NoTrumpDown -> "nd"
let cards_str cs = cs |> List.sort compare |> List.map card_str |> String.concat ""
let trick_str (t : Trick.t) = String.concat "" (List.map (fun (_, c) -> card_str c) t.Trick.plays)
let tally_str (t : Eng.scores) = Printf.sprintf "%d.%d" t.Eng.north_south t.Eng.east_west

let play_bucket regime led legal =
  let cap n = min n 3 in
  let n cat = cap (List.length (List.filter (fun c -> Game.cat_of regime led c = cat) legal)) in
  Printf.sprintf "f%dt%do%d" (n `F) (n `T) (n `O)

let key (s : Game.astate) : string =
  let p = Game.current_player s in
  let g = s.game in
  match g.Eng.phase, s.pending with
  | Eng.Auction { hands; high_bid; _ }, _ ->
      Printf.sprintf "%s|A|%s|%s" (seat_str p) (bucket (Eng.get_hand p hands)) (high_str high_bid)
  | Eng.Declaring { hands; kitty; bid; _ }, None ->
      Printf.sprintf "%s|Dr|%s|%s" (seat_str p) (bucket (Eng.get_hand p hands @ kitty)) (bid_str bid)
  | Eng.Declaring { hands; kitty; _ }, Some pd ->
      let pile = Eng.get_hand p hands @ kitty in
      let remaining = List.filter (fun c -> not (List.mem c pd.Game.discarded)) pile in
      Printf.sprintf "%s|Dd|%s|%d" (seat_str p) (cards_str remaining) (List.length pd.Game.discarded)
  | Eng.Playing { regime; hands; tally; trick; _ }, _ ->
      let legal = Trick.legal_moves regime (Eng.get_hand p hands) trick in
      let led = Trick.led_suit trick in
      let led_s = match led with Some s -> suit_str s | None -> "-" in
      Printf.sprintf "%s|P|%s|%s|%s|%s" (seat_str p) (regime_str regime) led_s
        (play_bucket regime led legal) (tally_str tally)
  | Eng.Finished _, _ -> "T"

type node = { actions : Game.action array; regret_sum : float array; strategy_sum : float array }
let make_node acts =
  let a = Array.of_list acts and n = List.length acts in
  { actions = a; regret_sum = Array.make n 0.; strategy_sum = Array.make n 0. }

let nodes : (string, node) Hashtbl.t = Hashtbl.create 100_000
let get_node k acts =
  match Hashtbl.find_opt nodes k with
  | Some n -> n | None -> let n = make_node acts in Hashtbl.add nodes k n; n

let strategy (node : node) =
  let n = Array.length node.regret_sum in
  let pos = Array.map (fun r -> if r > 0. then r else 0.) node.regret_sum in
  let tot = Array.fold_left ( +. ) 0. pos in
  if tot > 0. then Array.map (fun r -> r /. tot) pos else Array.make n (1. /. float_of_int n)

let average_strategy (node : node) =
  let n = Array.length node.strategy_sum in
  let tot = Array.fold_left ( +. ) 0. node.strategy_sum in
  if tot > 0. then Array.map (fun s -> s /. tot) node.strategy_sum else Array.make n (1. /. float_of_int n)

let sample rng probs =
  let r = ref (Random.State.float rng 1.0) and n = Array.length probs in
  let rec go i = if i >= n-1 then i else if !r < probs.(i) then i else (r := !r -. probs.(i); go (i+1)) in
  go 0

let all_seats = [ Player.North; Player.East; Player.South; Player.West ]

let rec es_cfr rng (i : Player.seat) (s : Game.astate) : float =
  if Game.is_terminal s then Game.payoff s i
  else
    let acts = Game.legal_actions s in
    let node = get_node (key s) acts in
    let strat = strategy node in
    if Game.current_player s = i then begin
      let util = Array.make (Array.length strat) 0. and node_util = ref 0. in
      List.iteri (fun idx a ->
        util.(idx) <- es_cfr rng i (Game.apply s a);
        node_util := !node_util +. strat.(idx) *. util.(idx)) acts;
      Array.iteri (fun idx _ ->
        node.regret_sum.(idx) <- node.regret_sum.(idx) +. (util.(idx) -. !node_util)) node.actions;
      !node_util
    end else begin
      Array.iteri (fun idx _ ->
        node.strategy_sum.(idx) <- node.strategy_sum.(idx) +. strat.(idx)) node.actions;
      es_cfr rng i (Game.apply s node.actions.(sample rng strat))
    end

let train_mc rng iters =
  for _ = 1 to iters do
    let s0 = Game.initial rng in           (* sample a deal = the chance node *)
    List.iter (fun i -> ignore (es_cfr rng i s0)) all_seats
  done

let epsilon = 0.6

let rec os_cfr rng (i : Player.seat) (s : Game.astate) (pi : float) (po : float) (samp : float)
  : float * float =
  if Game.is_terminal s then (Game.payoff s i /. samp, 1.0)
  else
    let acts = Game.legal_actions s in
    let node = get_node (key s) acts in
    let strat = strategy node in
    let n = Array.length strat in
    let player = Game.current_player s in
    let sampling =
      if player = i
      then Array.map (fun p -> epsilon /. float_of_int n +. (1. -. epsilon) *. p) strat
      else strat
    in
    let a = sample rng sampling in
    let child = Game.apply s node.actions.(a) in
    if player = i then begin
      Array.iteri (fun b _ ->
        node.strategy_sum.(b) <- node.strategy_sum.(b) +. (pi /. samp) *. strat.(b)) node.actions;
      let (u, x) = os_cfr rng i child (pi *. strat.(a)) po (samp *. sampling.(a)) in
      let w = u *. po in
      let node_val = w *. x *. strat.(a) in
      Array.iteri (fun b _ ->
        let cfv = if b = a then w *. x else 0. in
        node.regret_sum.(b) <- node.regret_sum.(b) +. (cfv -. node_val)) node.actions;
      (u, x *. strat.(a))
    end
    else os_cfr rng i child pi (po *. strat.(a)) (samp *. sampling.(a))

let train_os rng iters =
  for iter = 1 to iters do
    let s0 = Game.initial rng in       (* the deal = the chance node, sampled once per iteration *)
    List.iter (fun i -> ignore (os_cfr rng i s0 1.0 1.0 1.0)) all_seats;
    if iter mod 100000 = 0 then Printf.printf "iteration %d/%d\n%!" iter iters
  done


let cfr_policy rng (s : Game.astate) : Game.action =
  match Hashtbl.find_opt nodes (key s) with
  | Some node -> node.actions.(sample rng (average_strategy node))   (* play the trained mix *)
  | None ->                                                          (* unseen state: random *)
      let acts = Game.legal_actions s in
      List.nth acts (Random.State.int rng (List.length acts))

let random_policy rng (s : Game.astate) : Game.action =
  let acts = Game.legal_actions s in
  List.nth acts (Random.State.int rng (List.length acts))

(* play one hand: team North/South uses CFR, East/West plays random; return NS's point swing *)
let play_hand rng : float =
  let rec go s =
    if Game.is_terminal s then Game.payoff s Player.North
    else
      let p = Game.current_player s in
      let act = (if Player.team_of p = Player.NorthSouth then cfr_policy else random_policy) rng s in
      go (Game.apply s act)
  in
  go (Game.initial rng)

let evaluate rng n =
  let total = ref 0. in
  for _ = 1 to n do total := !total +. play_hand rng done;
  !total /. float_of_int n


let play_hand_with rng polA polB =     (* polA plays North/South, polB plays East/West *)
let rec go s =
  if Game.is_terminal s then Game.payoff s Player.North
  else
    let p = Game.current_player s in
    let pol = if Player.team_of p = Player.NorthSouth then polA else polB in
    go (Game.apply s (pol rng s))
in
go (Game.initial rng)

let evaluate2 rng n polA polB =
  let t = ref 0. in
  for _ = 1 to n do t := !t +. play_hand_with rng polA polB done;
  !t /. float_of_int n