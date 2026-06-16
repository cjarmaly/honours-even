open Bidwhist.Deck

let _ = assert (List.length full_deck = 54) (* 52 cards + 2 jokers *)
let _ = assert (List.length (List.sort_uniq compare full_deck) = List.length full_deck) (* No duplicates *)