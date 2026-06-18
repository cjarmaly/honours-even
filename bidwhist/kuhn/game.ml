type card = J | Q | K 
type action = Pass | Bet   

let value = function J -> 1 | Q -> 2 | K -> 3 

let current_player (h: action list) : int = List.length h mod 2

let is_terminal (h : action list) : bool =
  match h with
  | [Pass; Pass ] | [ Bet; Pass ] | [Bet; Bet ] 
  | [ Pass; Bet; Pass ] | [ Pass; Bet; Bet ] -> true
  | _ -> false

let payoff (c0 : card) (c1 : card) (h : action list) : int =
  let showdown amt = if value c0 > value c1 then amt else -amt in
  match h with
  | [ Pass; Pass ] -> showdown 1 (* check-check: showdown for the 2-ante pot, +/- 1 net *)
  | [ Bet; Pass ] -> 1 (* P0 bets, P1 folds: P0 wins P1's ante, +1 *)
  | [ Bet; Bet ] -> showdown 2 (* P0 bets, P1 calls: showdown for the 4-ante pot, +/- 2 net *)
  | [ Pass; Bet; Pass ] -> -1 (* P0 checks, P1 bets, P0 folds: P1 wins P0's ante, -1 *)
  | [ Pass; Bet; Bet ] -> showdown 2 (* P0 checks, P1 bets, P0 calls: showdown for the 4-ante pot, +/- 2 net *)
  | _ -> failwith "payoff: invalid history"