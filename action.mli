open State
(* [action] represents an action that changes the state of the game.
   Move moves a player int number of spaces, take money takes from that player
   an int amount of money, give money does the opposite, SendToJA sends the
   player to our version of jail, the JA, GiveProperty gives the property to a
   a player after it has been bought, TakeProperty takes it back after the player
   sells it, and UpdateRent updates the rent *)
type action =
  | Move of player * int (* move a player int spaces *)
  | TakeMoney of player * int
  | GiveMoney of player * int
  | SendToJA of player
  | GiveProperty of player * string
  | TakeProperty of player * string
  | UpdateRent of property

(* [move_player] takes in a state, the original state, and an int, which is
   the number of spaces that the play should move, then outputs the result
   state from that move *)
val move_player : state -> int -> state

(* [take_money] takes in a state, the original state, and an int, which is
   the amount of money, another int, which is the player number, and takes
   away that amount of money from that player, outputting the state from that *)
val take_money : state -> int -> int -> state

(* [give_money] takes in a state, the original state, and an int, which is
   the amount of money, another int, which is the player number, and gives
   that amount of money to that player, outputting the state from that *)
val give_money : state -> int -> int -> state

(* [update_rent] takes in a state, the name of the property, and outputs
   the state with the rent changed on that property *)
val update_rent : state -> string -> state

(* [draw_chance] takes in a card list, which is the deck of chance cards,
   and outputs the card from that list *)
val draw_chance : card list ->  card

(* [execute_chance] takes in a state and the card drawn, and executes what
   command is written on that card, and outputs the state that results from
   that execution *)
val execute_chance : state ->  card -> state

(* [draw_community_chest] takes in a card list, which is the deck of
   community chest cards, and outputs a card from that list *)
val draw_community_chest : card list ->  card

(* [execute_community_chest] takes in a state and the card draw, and executes
   what command is written on that card, and outputs the state that results
   from that execution *)
val execute_community_chest : state ->  card -> state

(* [get_net_worth] takes in a state, original net worth, and outputs the new
   net worth after taking what is in the state into account *)
val get_net_worth: state -> int -> int
