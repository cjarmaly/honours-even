type t = {
  leader : Player.seat;
  plays : (Player.seat * Card.t) list;
}

let led_suit (t: t) : Card.suit option =
(* Each new play is concatenated to at the end of a list *)
(* List.find_map returns None if no element matches the predicate *)
  List.find_map 
    (fun (_, card) ->
      match card with
      | Card.Regular {suit; _} -> Some suit
      | Card.BigJoker | Card.LittleJoker -> None) 
    t.plays

let legal_moves (regime : Card.regime) (hand : Card.t list) (trick : t) : Card.t list =
  let led_suit = led_suit trick in

  (* Helper function to get all cards of a given suit from a hand *)
  let cards_of_suit (suit: Card.suit) (hand: Card.t list) : Card.t list =
    List.filter (fun card -> 
      match card with
      | Card.Regular {suit = s; _} -> s = suit
      | Card.BigJoker | Card.LittleJoker -> false) hand
    in

  (* Helper function to get all jokers from a hand *)
  let jokers_in_hand (hand: Card.t list) : Card.t list =
    List.filter (fun card -> 
      match card with
      | Card.BigJoker | Card.LittleJoker -> true
      | Card.Regular _ -> false) hand
   in
  (* Logic based on the rules of the game defined in README.md. 
  It's a bit convoluted, so tread lightly. *)
  match led_suit with
  | None -> hand (*leading, or only jokers led so far means anything goes *)
  | Some led ->
    let follows = 
      if regime = Card.Uptown led || regime = Card.Downtown led 
        then cards_of_suit led hand @ jokers_in_hand hand (* Jokers are the trump suit *)
      else cards_of_suit led hand (* Trump suit is not led, so only play the lead suit *)
    in
    if follows <> [] then follows (* if there are cards of the led suit, you must play them *)
    else if regime = Card.NoTrumpHigh || regime = Card.NoTrumpDown then
      let jokers = jokers_in_hand hand in
      if jokers <> [] then jokers (* if you have jokers, you must play them *)
      else hand (* No lead suit, no jokers means you can play anything *)
    else hand (* No lead suit, and not in NoTrump regime means you can play anything *)


let winner (regime : Card.regime) (t : t) : Player.seat = 
  (* Function precondition: called on a finished trick*)
  match led_suit t with
  | None -> failwith "winner: unfinished trick (no led suit)"
  | Some led -> 
    match t.plays with
    | [] -> failwith "winner: empty trick"
    | first :: rest ->
      let best_seat, _best_card =
      List.fold_left
        (fun (best_seat, best_card) (seat, card) ->
          (* Return whichever (seat, card) is stronger *)
          if Card.compare_in_trick regime led card best_card > 0 then (seat, card)
          else (best_seat, best_card))
            first rest
        in
        best_seat

let current_player (t: t) : Player.seat =
  match List.rev t.plays with
  | [] -> t.leader
  | (last_seat, _) :: _ -> Player.next_seat last_seat