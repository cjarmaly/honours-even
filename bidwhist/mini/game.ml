(* Mini-CFR implementation for Bid Whist - see README.md for more details *)

type suit = Spades | Hearts
type rank = J | Q | K | A 
type card = { suit : suit; rank : rank }

let rank_value = function J -> 1 | Q -> 2 | K -> 3 | A -> 4

let deck = 
  [ { suit = Spades; rank = J }; { suit = Spades; rank = Q }; { suit = Spades; rank = K }; { suit = Spades; rank = A };
    { suit = Hearts; rank = J }; { suit = Hearts; rank = Q }; { suit = Hearts; rank = K }; { suit = Hearts; rank = A };
  ]

type action =
 | Declare of suit (* the declarer's one trump choice *)
 | Play of card (* a card played into a trick *)

let team p = p mod 2 (* 0 and 2 are team 0, 1 and 3 are team 1 *)
let partner p = (p + 2) mod 4
let next_player p = (p + 1) mod 4

type state = {
  hands: card list array; (* hands.(p) = player p's remaining cards*)
  trump : suit option; (* None if no trump has been declared *)
  to_act : int; (* 0, 1, 2, 3 = declarer, next, partner, opponent *)
  trick : (int * card) list; (* (player, card) list - empty if no tricks have been played *)
  leader : int; (* the player who led the current trick *)
  tricks : int array; (* tricks.(team) won so far; length 2 *)
  log : action list; (* observed history of actions *)
}

let initial (hands : card list array) : state =
  { hands; trump = None; to_act = 0; trick = []; leader = 0; tricks = [| 0; 0 |]; log = [] }

let is_terminal state = state.tricks.(0) + state.tricks.(1) = 2

let legal_moves (s : state) : action list =
  match s.trump with
  | None -> [ Declare Spades; Declare Hearts ] (* declaration phase *)
  | Some _ ->
    let hand = s.hands.(s.to_act) in
    let legal_cards =
      match s.trick with 
      | [] -> hand (* leading: anything *)
      | (_, led_card) :: _ -> 
        (match List.filter (fun c -> c.suit = led_card.suit) hand with
        | [] -> hand (*void: anything *)
        | following -> following) (*must follow suit *)
    in
    List.map (fun c -> Play c) legal_cards

let card_strength trump led c = 
  if c.suit = trump then (2, rank_value c.rank)
  else if c.suit = led then (1, rank_value c.rank)
  else (0, rank_value c.rank)

let trick_winner trump (plays : (int * card) list) : int =
  let led = (snd (List.hd plays)).suit in (* first card played sets the suit *)
  let best_player, _ =
  List.fold_left 
    (fun (bp, bc) (p, c) -> 
      if card_strength trump led c > card_strength trump led bc then (p,c) else (bp, bc))
      (List.hd plays) (List.tl plays)
    in
    best_player


let apply (s : state) (a : action) : state =
  match a with
  | Declare suit ->
      { s with trump = Some suit; log = s.log @ [ a ] }   (* declarer still leads, trick stays empty *)
  | Play card ->
      let p = s.to_act in
      let hands' = Array.copy s.hands in                    
      hands'.(p) <- List.filter (fun c -> c <> card) hands'.(p);
      let trick' = s.trick @ [ (p, card) ] in              (* append: play order *)
      let log' = s.log @ [ a ] in
      if List.length trick' = 4 then
        let trump = match s.trump with Some t -> t | None -> failwith "play before declare" in
        let w = trick_winner trump trick' in
        let tricks' = Array.copy s.tricks in          
        tricks'.(team w) <- tricks'.(team w) + 1;
        { s with hands = hands'; tricks = tricks'; trick = []; leader = w; to_act = w; log = log' }
      else
        { s with hands = hands'; trick = trick'; to_act = next_player p; log = log' }

let payoff (s : state) (player : int) : float = 
  let net = float_of_int (s.tricks.(0) - 1) in (* declaring team's tricks - 1: -1 / 0 / +1 *)
  if team player = 0 then net else -. net