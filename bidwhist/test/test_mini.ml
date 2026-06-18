open Mini

(* Phase 4: CFR solves Mini Bid Whist, and the best-response oracle certifies it.
   Marked `Slow — it trains and runs four best-response passes. *)
let test_exploitability () =
  Hashtbl.clear Cfr.nodes;
  let v = Cfr.train 100 in

  (* Invariant: the value is zero-sum between teams and shared within a team. *)
  Alcotest.(check (float 1e-9)) "zero-sum across teams" (-. v.(1)) v.(0);
  Alcotest.(check (float 1e-9)) "partners share value"  v.(2)      v.(0);

  (* Invariant: best-responding never does worse than conforming — every gain >= 0.
     (This is the oracle's own correctness check; it must hold for any strategy.) *)
  let gains = List.map (fun i -> Cfr.best_response i -. Cfr.value_under_sigma i) [ 0; 1; 2; 3 ] in
  List.iteri (fun i g ->
    Alcotest.(check bool) (Printf.sprintf "player %d gain >= 0" i) true (g >= -1e-9)) gains;

  (* Invariant: CFR reaches near-equilibrium — total exploitability is small.
     (Loose bound: this catches a broken pipeline, not a tight convergence assertion.) *)
  let total = List.fold_left ( +. ) 0. gains in
  Alcotest.(check bool) "low exploitability" true (total < 0.1)

let () =
Alcotest.run "bidwhist" [
  ( "mini",
  [ Alcotest.test_case "CFR reaches low exploitability" `Slow test_exploitability ] );
]