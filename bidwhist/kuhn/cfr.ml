let actions = [| Game.Pass; Game.Bet |] (* Index 0 = Pass, Index 1 = Bet *)

let n_actions = 2

type node = {
  regret_sum : float array; (* cumulative refret for each action - drives the strategy *)
  strategy_sum : float array; (* cumulative strategy weights - averaged into the answer *)
}

let make_node () =
  { regret_sum = Array.make n_actions 0.; strategy_sum = Array.make n_actions 0. }

let key (card : Game.card) (history : Game.action list) : string =
  let card_str = match card with
    | Game.J -> "J"
    | Game.Q -> "Q"
    | Game.K -> "K"
  in
  let history_str = String.concat "" (List.map (function Game.Pass -> "p" | Game.Bet -> "b") history)
  in
  card_str ^ " " ^ history_str

let nodes : (string, node) Hashtbl.t = Hashtbl.create 16
let get_node k = 
  match Hashtbl.find_opt nodes k with
  | Some n -> n
  | None ->
    let n = make_node () in
    Hashtbl.add nodes k n;
    n

let strategy (node : node) : float array =
  let pos = Array.map (fun r -> if r > 0. then r else 0.) node.regret_sum in
  let total = Array.fold_left ( +. ) 0. pos in
  if total > 0. then Array.map (fun r -> r /. total) pos
  else Array.make n_actions (1. /. float_of_int n_actions)

(* CFR iteration - good luck following along. We dip into some imperative programming here to avoid recursion limits. *)
let rec cfr (c0 : Game.card) (c1 : Game.card) (history : Game.action list) (p0 : float) (p1 : float) : float =
  let player = Game.current_player history in
  if Game.is_terminal history then begin
    let u = float_of_int (Game.payoff c0 c1 history) in (* payoff is from player 0's view *)
    if player = 0 then u else -. u 
  end
else begin
  let my_card = if player =0 then c0 else c1 in
  let node = get_node (key my_card history) in
  let strat = strategy node in
  let util = Array.make n_actions 0. in (* utility for each action *)
  let node_util = ref 0. in 
  Array.iteri (fun i a -> (* iterate over each action *)
    let next = history @ [ a ] in
    let child = 
      if player = 0 then cfr c0 c1 next (p0 *. strat.(i)) p1
      else cfr c0 c1 next p0 (p1 *. strat.(i)) 
    in 
    util.(i) <- -. child; (* negative because we're using regret-matching *)
    node_util := !node_util +. strat.(i) *. util.(i)
  ) actions;
  let cf_reach = if player = 0 then p1 else p0 in (* opponent reach probability *)
  let my_reach = if player = 0 then p0 else p1 in
  Array.iteri (fun i _ -> 
    let regret = util.(i) -. !node_util in
    node.regret_sum.(i) <- node.regret_sum.(i) +. cf_reach *. regret;
    node.strategy_sum.(i) <- node.strategy_sum.(i) +. my_reach *. strat.(i)
  ) actions;
  !node_util
end

let average_strategy (node : node) : float array =
  let total = Array.fold_left ( +. ) 0. node.strategy_sum in
  if total > 0. then Array.map (fun s -> s /. total) node.strategy_sum
  else Array.make n_actions (1. /. float_of_int n_actions)

let train (iters : int) : float =
  let deals = 
    [ (Game.J, Game.Q); (Game.J, Game.K); (Game.Q, Game.K); 
      (Game.K, Game.J); (Game.K, Game.Q); (Game.Q, Game.J); ] 
  in
  let total = ref 0. in
  for _ = 1 to iters do
    List.iter (fun (c0, c1) -> total := !total +. cfr c0 c1 [] 1.0 1.0) deals;
  done;
  !total /. float_of_int (iters * 6) (* average the payoffs over all deals *)