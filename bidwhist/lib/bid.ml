type bid_kind = Uptown | Downtown | NoTrump

type level = Three | Four | Five | Six | Seven

type t = {
  kind: bid_kind;
  level: level;
}

type action = Pass | Bid of t

let compare_bid (b1: t) (b2: t) : int =
  let tier = function
  | Uptown -> 0
  | Downtown -> 1
  | NoTrump -> 2
  in
  let level_to_int (l: level) : int =
    match l with
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
  in
  let lvl = Int.compare (level_to_int b1.level) (level_to_int b2.level) in
  if lvl <> 0 then lvl
  else Int.compare (tier b1.kind) (tier b2.kind)