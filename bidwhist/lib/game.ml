type scores = { north_south: int; east_west: int }

type hand = Card.t list

type hands = { north: hand; south: hand; east: hand; west: hand }

type phase = 
| Auction of {
  hands : hands;
  kitty : Card.t list;
  high_bid : (Player.seat * Bid.t) option; (* the highest bid so far, and the player who made it *)
  to_act : Player.seat 
}
| Declaring of {
  hands : hands; (* winner's hand now holds 18 until they discard 6*)
  winner : Player.seat;
  bid : Bid.t
}
| Playing of {
  regime : Card.regime;
  hands : hands;
  tally : scores; (* per team trick count with declaring team starting at 1 *)
  trick : (Player.seat * Card.t) list (* plays so far this trick*)
}
| Finished of { winer : Player.team } 