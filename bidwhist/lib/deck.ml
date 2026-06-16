open Card

type dealt = { hands : t list * t list * t list * t list; kitty : t list }

let all_suits = [Hearts; Diamonds; Clubs; Spades]

let all_ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]

let full_deck = List.concat_map (fun s -> 
  List.map (fun r -> 
            Regular { suit = s; rank = r }) all_ranks) all_suits @ [BigJoker; LittleJoker]

(* Fisher-Yates shuffle *)
let shuffle (rng: Random.State.t) (deck: t list) : t list =
  let arr = Array.of_list deck in
  for i = Array.length arr - 1 downto 1 do 
    let j = Random.State.int rng (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let deal (rng: Random.State.t) (deck: t list) : dealt =

  let split_at (n: int) (lst: 'a list) : 'a list * 'a list =
    let rec aux n acc lst = 
      match n, lst with
      | 0, rest -> (List.rev acc, rest)
      | _, [] -> failwith "split_at: list is too short"
      | n, hd :: tl -> aux (n-1) (hd :: acc) tl
    in
    aux n [] lst
  in
    let shuffled = shuffle rng deck in
    let h1, rest = split_at 12 shuffled in
    let h2, rest = split_at 12 rest in
    let h3, rest = split_at 12 rest in
    let kitty, h4 = split_at 6 rest in (* reverse order to avoid unnecessary iterations *)
    
    { hands = (h1, h2, h3, h4); kitty = kitty }




