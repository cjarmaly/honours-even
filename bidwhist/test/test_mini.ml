open Mini

(*exact Mini — CFR reaches a per-player near-equilibrium. *)
let test_exact_equilibrium () =
  Cfr.key_ref := Cfr.key_exact;
  Hashtbl.clear Cfr.nodes;
  Cfr.train_mc (Random.State.make [| 1 |]) 50_000;
  Alcotest.(check bool) "exact: low exploitability" true (Cfr.exploitability () < 0.05)

(* A lossy declare abstraction looks converged within the abstract game but is
   heavily exploitable by a full-resolution opponent — abstraction error, measured. *)
let test_abstraction_error () =
  Cfr.key_ref := Cfr.key_abstract;
  Hashtbl.clear Cfr.nodes;
  Cfr.train_mc (Random.State.make [| 1 |]) 50_000;
  Alcotest.(check bool) "abstraction is truly exploitable" true (Cfr.exploitability () > 0.1)

  let () =
  Alcotest.run "bidwhist" [
    ( "mini",
      [ Alcotest.test_case "exact equilibrium"      `Slow test_exact_equilibrium;
        Alcotest.test_case "abstraction error"    `Slow test_abstraction_error ] );
  ]