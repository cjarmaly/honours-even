open Kuhn

let approx ?(tol = 0.02) a b = Float.abs (a -. b) < tol

(* look up a trained node's bet/pass probability by its (card, history) *)
let bet card hist  = (Cfr.average_strategy (Cfr.get_node (Cfr.key card hist))).(1)
let pass card hist = (Cfr.average_strategy (Cfr.get_node (Cfr.key card hist))).(0)

(* Invariant: CFR converges to Kuhn's known equilibrium — value -1/18 and the fixed
   strategy facts that hold for every member of the equilibrium family. *)
let test_converges () =
  Hashtbl.clear Cfr.nodes;
  let value = Cfr.train 50_000 in
  Alcotest.(check bool) "game value ~ -1/18" true (approx ~tol:0.01 value (-1.0 /. 18.0));
  Alcotest.(check bool) "Q never opens a bet"      true (pass Game.Q [] > 0.97);
  Alcotest.(check bool) "K calls a bet (K b)"      true (bet  Game.K [ Game.Bet ] > 0.97);
  Alcotest.(check bool) "K calls a bet (K pb)"     true (bet  Game.K [ Game.Pass; Game.Bet ] > 0.97);
  Alcotest.(check bool) "J folds to a bet (J b)"   true (pass Game.J [ Game.Bet ] > 0.97);
  Alcotest.(check bool) "J folds to a bet (J pb)"  true (pass Game.J [ Game.Pass; Game.Bet ] > 0.97);
  (* the signature 1:3 bluff ratio: K opens a bet ~3x as often as J *)
  let jb = bet Game.J [] and kb = bet Game.K [] in
  Alcotest.(check bool) "1:3 bluff ratio" true (approx ~tol:0.05 kb (3.0 *. jb))

let () =
Alcotest.run "kuhn" [
  ( "cfr",
    [ Alcotest.test_case "converges to equilibrium" `Quick test_converges ] )
]