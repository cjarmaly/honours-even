type suit = Hearts | Diamonds | Clubs | Spades

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type card =
  | Regular of { suit: suit; rank: rank }
  | BigJoker
  | LittleJoker

type regime = 
  | Uptown of suit (* trump suit *)
  | Downtown of suit (* trump suit *)
  | NoTrumpHigh
  | NoTrumpDown

let height : regime -> rank -> int =
  fun regime rank -> 
    let direction = 
      match regime with 
      | Uptown _ | NoTrumpHigh -> 1
      | Downtown _ | NoTrumpDown -> -1
    in
    direction * match rank with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14
  
type strength = 
  | Dead (* off suit, not trump- can never take the trick*)
  | Led of int (* on suit, int is the height of the card *)
  | Trump of int (* trump, int is the height of the card *)
  | TrumpLittle (* little joker *)
  | TrumpBig (* big joker *)

let strength_of_card (regime: regime) (led: suit) (card: card) : strength =
  match regime with 
  | Uptown trump | Downtown trump -> (
    match card with 
    | Regular { suit; rank } -> 
      if suit = trump then Trump (height regime rank)
      else if suit = led then Led (height regime rank)
      else Dead
    | BigJoker -> TrumpBig
    | LittleJoker -> TrumpLittle
  )
  | NoTrumpHigh | NoTrumpDown -> (
    match card with 
    | Regular { suit; rank } -> 
      if suit = led then Led (height regime rank)
      else Dead
    | BigJoker | LittleJoker -> Dead
  )

let compare_strength (strength1: strength) (strength2: strength) : int =
  let tier = function
  | Dead -> 0
  | Led _ -> 1
  | Trump _ -> 2
  | TrumpLittle -> 3
  | TrumpBig -> 4
  in
  match strength1, strength2 with
  | Led a, Led b 
  | Trump a, Trump b -> Int.compare a b
  | _, _ -> Int.compare (tier strength1) (tier strength2)
  
let compare_in_trick (regime: regime) (led: suit) (card1: card) (card2: card) : int =
  compare_strength (strength_of_card regime led card1) (strength_of_card regime led card2)



