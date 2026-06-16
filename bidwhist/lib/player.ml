type seat = North | East | South | West

type team = NorthSouth | EastWest

let next_seat (s: seat) : seat =
  match s with
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let partner (s: seat) : seat =
  match s with
  | North -> South
  | East -> West
  | South -> North
  | West -> East

let team_of (s: seat) : team =
  match s with
  | North -> NorthSouth
  | East -> EastWest
  | South -> NorthSouth
  | West -> EastWest

let same_team (s1: seat) (s2: seat) : bool =
  team_of s1 = team_of s2

let other_team (t: team) : team =
  match t with
  | NorthSouth -> EastWest
  | EastWest -> NorthSouth