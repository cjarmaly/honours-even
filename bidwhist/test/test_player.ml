open Bidwhist.Player

let all_seats = [ North; East; South; West ]

(* Invariant: partner is an involution — partnering twice returns the original seat. *)
let test_partner_involution () =
  List.iter (fun s ->
    Alcotest.(check bool) "partner (partner s) = s" true (partner (partner s) = s))
    all_seats

(* Invariant: no seat partners itself. *)
let test_partner_distinct () =
  List.iter (fun s ->
    Alcotest.(check bool) "partner s <> s" true (partner s <> s))
    all_seats

(* Invariant: partners are on the same team. *)
let test_partners_same_team () =
  List.iter (fun s ->
    Alcotest.(check bool) "team_of (partner s) = team_of s" true
      (team_of (partner s) = team_of s))
    all_seats

(* Invariant: the seat to your left is an opponent — you sit between the other team. *)
let test_neighbors_are_opponents () =
  List.iter (fun s ->
    Alcotest.(check bool) "team_of (next_seat s) <> team_of s" true
      (team_of (next_seat s) <> team_of s))
    all_seats

(* Invariant: next_seat is a 4-cycle — four steps return to start, and the four
   seats reached from any start are all distinct (a full permutation of the table). *)
let test_next_seat_cycle () =
  List.iter (fun s ->
    let a = next_seat s in
    let b = next_seat a in
    let c = next_seat b in
    let d = next_seat c in
    Alcotest.(check bool) "four steps return to start" true (d = s);
    Alcotest.(check int) "four distinct seats" 4
      (List.length (List.sort_uniq compare [ a; b; c; d ])))
    all_seats

    let () =
    Alcotest.run "bidwhist"
      [ ( "player",
      [ Alcotest.test_case "partner involution"  `Quick test_partner_involution;
        Alcotest.test_case "partner distinct"    `Quick test_partner_distinct;
        Alcotest.test_case "partners same team"  `Quick test_partners_same_team;
        Alcotest.test_case "neighbors opponents" `Quick test_neighbors_are_opponents;
        Alcotest.test_case "next_seat cycle"     `Quick test_next_seat_cycle ] ) ]