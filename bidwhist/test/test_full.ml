open Full

(* Trains a short run and checks the trained strategy beats the random baseline.
   `Slow — it trains and plays a few thousand hands. *)
let test_beats_random () =
  Hashtbl.clear Cfr.nodes;
  Cfr.train_os (Random.State.make [| 1 |]) 200_000;
  let n = 2000 in
  let base = Cfr.evaluate2 (Random.State.make [| 7 |]) n Cfr.random_policy Cfr.random_policy in
  let cfr  = Cfr.evaluate2 (Random.State.make [| 7 |]) n Cfr.cfr_policy    Cfr.random_policy in
  (* the dealer's team is handicapped under random play, so we test the *improvement* *)
  Alcotest.(check bool)
    (Printf.sprintf "CFR improves on baseline (base=%.2f cfr=%.2f)" base cfr)
    true (cfr -. base > 0.5)

let () = 
  Alcotest.run "bidwhist" [("full", [ Alcotest.test_case "CFR beats random baseline" `Slow test_beats_random ])]