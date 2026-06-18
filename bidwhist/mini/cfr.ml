type node = {
  actions : Game.action array;
  regret_sum : float array;
  strategy_sum : float array;
}

let make_node (acts : Game.action list) : node =
  let a = Array.of_list acts in
  let n = Array.length a in
  { actions = a; regret_sum = Array.make n 0.; strategy_sum = Array.make n 0. }

(* string encoders for the key *)
let suit_str = function Game.Spades -> "S" | Game.Hearts -> "H"
let rank_str = function Game.J -> "J" | Game.Q -> "Q" | Game.K -> "K" | Game.A -> "A"
let card_str (c : Game.card) = rank_str c.Game.rank ^ suit_str c.Game.suit
let action_str = function
  | Game.Declare s -> "d" ^ suit_str s
  | Game.Play c -> card_str c

(* an information set = the acting player's hand + the public log *)
let key (s : Game.state) : string =
  let hand = List.sort compare s.Game.hands.(s.Game.to_act) in
  let hand_str = String.concat "" (List.map card_str hand) in
  let log_str = String.concat "" (List.map action_str s.Game.log) in
  string_of_int s.Game.to_act ^ ":" ^ hand_str ^ "|" ^ log_str

let nodes : (string, node) Hashtbl.t = Hashtbl.create 1024
let get_node (k : string) (acts : Game.action list) : node =
  match Hashtbl.find_opt nodes k with
  | Some n -> n
  | None -> let n = make_node acts in Hashtbl.add nodes k n; n

let strategy (node : node) : float array =
  let n = Array.length node.regret_sum in
  let pos = Array.map (fun r -> if r > 0. then r else 0.) node.regret_sum in
  let tot = Array.fold_left ( +. ) 0. pos in
  if tot > 0. then Array.map (fun r -> r /. tot) pos
  else Array.make n (1. /. float_of_int n)

let rec cfr (s : Game.state) (reach : float array) : float array =
  if Game.is_terminal s then
    Array.init 4 (fun p -> Game.payoff s p)   (* a utility per player *)
  else begin
    let player = s.Game.to_act in
    let node = get_node (key s) (Game.legal_moves s) in
    let strat = strategy node in
    let n = Array.length strat in
    let util = Array.make n [||] in          (* util.(i) = the 4-vector after action i *)
    let node_util = Array.make 4 0. in (* node utility = sum of each player's utility *)
    Array.iteri (fun i a ->
      let new_reach = Array.copy reach in (* COPY, like the hands *)
      new_reach.(player) <- new_reach.(player) *. strat.(i);
      let child = cfr (Game.apply s a) new_reach in
      util.(i) <- child;
      Array.iteri (fun p u -> node_util.(p) <- node_util.(p) +. strat.(i) *. u) child
    ) node.actions;
    (* counterfactual reach = product of every OTHER player's reach *)
    let cf_reach = ref 1.0 in
    Array.iteri (fun p r -> if p <> player then cf_reach := !cf_reach *. r) reach;
    Array.iteri (fun i _ ->
      let regret = util.(i).(player) -. node_util.(player) in
      node.regret_sum.(i) <- node.regret_sum.(i) +. !cf_reach *. regret;
      node.strategy_sum.(i) <- node.strategy_sum.(i) +. reach.(player) *. strat.(i)
    ) node.actions;
    node_util
  end

(* (element, the rest) for each element *)
let rec pick1 = function
  | [] -> []
  | x :: rest -> (x, rest) :: List.map (fun (y, r) -> (y, x :: r)) (pick1 rest)

(* ([a;b], complement) for each 2-subset *)
let rec choose2 = function
  | [] | [ _ ] -> []
  | x :: tl ->
      List.map (fun (y, rest) -> ([ x; y ], rest)) (pick1 tl)
      @ List.map (fun (pair, rest) -> (pair, x :: rest)) (choose2 tl)

(* every way to deal the 8 cards as four 2-card hands: 28 * 15 * 6 = 2520 *)
let all_deals =
  choose2 Game.deck |> List.concat_map (fun (h0, r0) ->
  choose2 r0 |> List.concat_map (fun (h1, r1) ->
  choose2 r1 |> List.map (fun (h2, h3) -> [| h0; h1; h2; h3 |])))


let average_strategy (node : node) : float array =
  let tot = Array.fold_left ( +. ) 0. node.strategy_sum in
  let n = Array.length node.strategy_sum in
  if tot > 0. then Array.map (fun s -> s /. tot) node.strategy_sum
  else Array.make n (1. /. float_of_int n)

let train (iters : int) : float array =
  let total = Array.make 4 0. in
  for _ = 1 to iters do
    List.iter (fun hands ->
      let u = cfr (Game.initial hands) [| 1.; 1.; 1.; 1. |] in
      Array.iteri (fun p x -> total.(p) <- total.(p) +. x) u)
      all_deals
  done;
  Array.map (fun x -> x /. float_of_int (iters * List.length all_deals)) total


(* Best Response & Exploitability Oracle which certifies that the trained strategy is a Nash equilibrium *)

(* the trained strategy at a state *)
let sigma (s : Game.state) : float array =
  average_strategy (get_node (key s) (Game.legal_moves s))

(* value to player i when EVERYONE plays sigma (the equilibrium value) *)
let rec ev_sigma (i : int) (s : Game.state) : float =
  if Game.is_terminal s then Game.payoff s i
  else
    let strat = sigma s and acts = Game.legal_moves s in
    let t = ref 0. in
    List.iteri (fun idx a -> t := !t +. strat.(idx) *. ev_sigma i (Game.apply s a)) acts;
    !t

let value_under_sigma (i : int) : float =
  List.fold_left (fun acc h -> acc +. ev_sigma i (Game.initial h)) 0. all_deals
  /. float_of_int (List.length all_deals)

(* player i's chosen best-response action per info set, filled by best_response *)
let br_action : (string, Game.action) Hashtbl.t = Hashtbl.create 256

(* value to i when i plays br_action and everyone else plays sigma *)
let rec brv (i : int) (s : Game.state) : float =
  if Game.is_terminal s then Game.payoff s i
  else
    let acts = Game.legal_moves s in
    if s.Game.to_act = i then
      brv i (Game.apply s (Hashtbl.find br_action (key s)))   (* must already be decided *)
    else
      let strat = sigma s in
      let t = ref 0. in
      List.iteri (fun idx a -> t := !t +. strat.(idx) *. brv i (Game.apply s a)) acts;
      !t

(* gather, for each of i's info sets, the states reaching it with their counterfactual reach
(product of the OTHER players' sigma-probs; i's own choices excluded) *)
let collect (i : int) : (string, (Game.state * float) list) Hashtbl.t =
  let tbl = Hashtbl.create 256 in
  let rec go s reach =
    if Game.is_terminal s then ()
    else
      let acts = Game.legal_moves s in
      if s.Game.to_act = i then begin
        let k = key s in
        Hashtbl.replace tbl k ((s, reach) :: (try Hashtbl.find tbl k with Not_found -> []));
        List.iter (fun a -> go (Game.apply s a) reach) acts          (* i's prob excluded *)
      end else begin
        let strat = sigma s in
        List.iteri (fun idx a -> go (Game.apply s a) (reach *. strat.(idx))) acts
      end
  in
  List.iter (fun h -> go (Game.initial h) 1.0) all_deals;
  tbl

(* exact best-response value for player i *)
let best_response (i : int) : float =
  Hashtbl.clear br_action;
  let reaching = collect i in
  let layer_of k =
    match Hashtbl.find reaching k with (s, _) :: _ -> List.length s.Game.log | [] -> 0 in
  (* decide info sets deepest-first, so deeper i-choices are fixed before shallower ones *)
  let keys = Hashtbl.fold (fun k _ acc -> k :: acc) reaching [] in
  let keys = List.sort (fun a b -> compare (layer_of b) (layer_of a)) keys in
  List.iter (fun k ->
    let states = Hashtbl.find reaching k in
    let acts = Game.legal_moves (fst (List.hd states)) in
    let best_a = ref (List.hd acts) and best_v = ref neg_infinity in
    List.iter (fun a ->
      let v = List.fold_left (fun acc (s, r) -> acc +. r *. brv i (Game.apply s a)) 0. states in
      if v > !best_v then (best_v := v; best_a := a)
    ) acts;
    Hashtbl.replace br_action k !best_a
  ) keys;
  List.fold_left (fun acc h -> acc +. brv i (Game.initial h)) 0. all_deals
  /. float_of_int (List.length all_deals)

(* total exploitability: how much each player gains by deviating, summed *)
let exploitability () : float =
  List.fold_left (fun acc i -> acc +. (best_response i -. value_under_sigma i)) 0.
    [ 0; 1; 2; 3 ]