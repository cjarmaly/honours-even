let () =
  let rng = Random.State.make [| 1 |] in
  Printf.printf "training...\n%!";
  Full.Cfr.train_os rng 2000000;
  Printf.printf "nodes: %d\n%!" (Hashtbl.length Full.Cfr.nodes);
  let n = 5000 in
  let base = Full.Cfr.evaluate2 (Random.State.make [| 7 |]) n
               Full.Cfr.random_policy Full.Cfr.random_policy in
  let cfr  = Full.Cfr.evaluate2 (Random.State.make [| 7 |]) n
               Full.Cfr.cfr_policy    Full.Cfr.random_policy in
  Printf.printf "random vs random (NS swing): %.3f\n%!" base;
  Printf.printf "cfr    vs random (NS swing): %.3f\n%!" cfr;
  Printf.printf "CFR improvement over baseline: %.3f\n%!" (cfr -. base)