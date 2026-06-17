open Bidwhist

let fresh rng = Game.start_hand rng Player.North Game.{ north_south = 0; east_west = 0 }

(* drive a game to Finished, capped so a non-terminating bug fails instead of hanging *)
let run_to_finish seed =
  let rng = Random.State.make [| seed |] in
  let rec loop steps t =
    if steps > 1_000_000 then failwith "game did not terminate"
    else match t.Game.phase with
      | Game.Finished _ -> t
      | _ -> loop (steps + 1) (Sim.random_step rng t)
  in
  loop 0 (fresh rng)

(* Invariant: every random game reaches Finished — it terminates and never takes an illegal step
   (any illegal action would have thrown inside a transition before we got here). *)
let test_terminates () =
  for seed = 0 to 200 do
    match (run_to_finish seed).Game.phase with
    | Game.Finished _ -> ()
    | _ -> Alcotest.failf "seed %d did not finish" seed
  done

(* Invariant: the declared winner actually satisfies the win condition (>=7, or opponent <=-7). *)
let test_win_condition () =
  for seed = 0 to 200 do
    let g = run_to_finish seed in
    match g.Game.phase with
    | Game.Finished { winner } ->
        let s = g.Game.scores in
        let ok = match winner with
          | Player.NorthSouth -> s.north_south >= 7 || s.east_west <= -7
          | Player.EastWest   -> s.east_west   >= 7 || s.north_south <= -7
        in
        Alcotest.(check bool) (Printf.sprintf "seed %d win is legitimate" seed) true ok
    | _ -> Alcotest.fail "not finished"
  done

(* Invariant: at the start of every hand within a game, all 54 cards are present (hands + kitty). *)
let test_conservation () =
  let rng = Random.State.make [| 42 |] in
  let full = List.sort compare Deck.full_deck in
  let rec loop steps t =
    if steps > 1_000_000 then failwith "did not terminate" else begin
      (match t.Game.phase with
       | Game.Auction { hands; kitty; _ } ->
           let all = hands.Game.north @ hands.east @ hands.south @ hands.west @ kitty in
           Alcotest.(check bool) "auction conserves the deck" true (List.sort compare all = full)
       | _ -> ());
      match t.Game.phase with
      | Game.Finished _ -> ()
      | _ -> loop (steps + 1) (Sim.random_step rng t)
    end
  in
  loop 0 (fresh rng)

(* Property: hold for arbitrary seeds, not just 0..200 *)
let prop_terminates =
  QCheck.Test.make ~count:500 ~name:"random games terminate in Finished"
    QCheck.int
    (fun seed ->
       match (run_to_finish seed).Game.phase with
       | Game.Finished _ -> true
       | _ -> false)

let () =
  Alcotest.run "bidwhist" [
    ( "sim",
      [ Alcotest.test_case "terminates"     `Quick test_terminates;
        Alcotest.test_case "win condition" `Quick test_win_condition;
        Alcotest.test_case "conservation"  `Quick test_conservation ] );
    ( "sim (properties)",
      [ QCheck_alcotest.to_alcotest prop_terminates ] ) ] 