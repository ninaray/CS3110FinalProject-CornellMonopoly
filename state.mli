(*********************************************************)

type strategy =
  | Nuisance
  | TwoColor
  | Smart
  | Human

(* [player] is a record that represents each player in the game. It contains the
   following fields: a number for us to keep track internally so we who which
   player is which, and id that is displayed in the GUI, money to keep track
   of how much money they have on hand, properties to keep track of how many
   properties they own, cards which keeps track of the get out of jail cards,
   location which keeps track of where on monopoly board they are, and
   transports to keep track of how many transports they own *)
type player = {
  num: int;
  id: string;
  strategy: strategy;
  token: string;
  money: int;
  properties: string list;
  cards: int;
  location: int;
  transports: int;
  utilities: int;
}

(* [transport] is a record that represents the spaces on the game that keeps
   track of the name of the spot, whether or not it is own, if it is,
   who the owner is so that they can properly charge the player the amount of
   current_rent and give money to the correct player *)
type transport = {
  tname: string;
  towned: bool;
  towner: int;
  tcurrent_rent: int;
  tprice: int;
}

(* [utility] is a record that represents the spaces on the game that keeps
   track of the name of the spot, whether or not it is own, if it is,
   who the owner is so that they can properly charge the player the amount of
   current_rent and give money to the correct player *)
type utility = {
  uname: string;
  uowned: bool;
  uowner: int;
  ucurrent_rent: int;
  uprice: int;
}

(* [property] is a record that represents the spaces on the game that are
   properties. They each have a name to identify them, whether or not they're
   owned, who the owner is if it is owned, which is player 0 in the case that
   it is not owned, color which keeps track of which color the property belongs
   too, which is important because when you own all of the same color, you can
   buy houses, houses, which is the number of houses on that the owner owns
   on that property, current_rent which is the penalty that a player pays when
   they land on this spot if they don't own it, and rent_list which is a *)
type property = {
  pname: string;
  powned: bool;
  powner: int;
  pcolor: string;
  phouses: int;
  pcurrent_rent: int;
  prent_list: int list; (* will implement if we want to make game more complex *)
  pprice: int;
  pbuild_cost: int;
}

(* [space] is a variant that represents all of the different spaces that a
   player can land on in a game of monopoly, all are named after the game *)
type space =
  | Enroll
  | Property of property
  | Transport of transport
  | Chance
  | CommunityChest
  | JAsOffice
  | FreeParking
  | Visiting
  | GoToJA
  | Utility of utility
  | Tax of string * int

(* [card] is a variant that describes the different cards that one can draw
   from chance and community chest, and we've split the different things
   that might pop up on the card into four categories. You can either get money
   from drawing the card, lose money, move a certain number of spaces, get
   a get out of JA's office card, or go to JA's office card *)
(* type card =
  | AddMoney
  | SubMoney
  | MoveSpaces
  | GetOutofJA
  | GoToJA *)

(* [state] is an abstract type representing the state of a game. It contains
   players, which is their number in the order and player list, the current player,
   the double_counter, which is used to send players to the JA's office,
   round of the game, properties, which is a list of all the properties in a
   game, updated for the current state of the game. Utilities is the list of
   utility spaces. Transports is the list of transport spaces. Board is a list
   of (int, space) such that the int represents the number that the space
   occupies, and space is the type of space as specified by the variant above *)
type state = {
  players: (int * player) list;
  current_player: int;
  double_counter: int;
  round: int;
  properties: property list;
  utilities: utility list;
  transports: transport list;
  board: (int * space) list;
  cc_cards: (string * Command.command) list;
  chance_cards: (string * Command.command) list;
}


(* [instantiate_player] takes in int for id, a string for name, and string
   for token, and the rest of fields in the record begin at zero *)
val instantiate_player : int -> string -> strategy -> string -> player
(* val instantiate_player : string -> player *)

(* [roll_die] takes in how many dice it rolls and outputs the list of results *)
val roll_die : int -> int list

(* [is_bought] takes in a property, and returns whether or not it has been
   purchased by another user *)
val is_bought : property -> bool

(* [turns s] is the number of turns the players have taken so far.
 * not used currently, but might be used in the future *)
(* val turns : state -> int *)

(* [shuffle_card takes in a deck of cards ] *)

(* [inv s] is the string representation of the player's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant.
 * Items that appear in inventory are properties, special cards such as
 * 'Get out of the JA's office free', and money *)
val inv : state -> string

(* [inv_all s] is the string representation of the inventory of all players.*)
val inv_all : state -> string

(* [print_rankings s] is the string representation of the ranking of all
 * players by liquid assests.*)
val print_rankings : state -> string

(* [init_state lst] is the initial state of the game given
 * that lst are the names of the players in order of play.
 * requires: [lst] contains 2 to 4 distinct alpha-numeric strings. *)
val init_state : player list -> state

(* [locations s] is an association list mapping players to the
 * id of the space on which they are currently located.
 * No player may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(player, space)]. *)
val locations : state -> string


(* [can_buy_house st prop_str] checks whether a player can buy a house on the
  *  property they've just landed on . It returns a tuple of three booleans.
  * the first is whether the player owns all properties of the given color,
  * because players can't buy houses otherwise.
  * The second is whether the player has enough money to buy a house on the property.
  * the third is whether the player can build a house without exceeding the 5-house limit.
*)
 val can_buy_house : state -> string -> (bool)

val can_buy_n_houses : state -> string -> int -> (bool)

(* [buy_house st prop_str] buys a house at the location of the current player,
 * provided that the current player already owns all the properties of the
 * current location's color, and building another house would not exceed the
 * limit of 5 houses per property.  *)
   val buy_house : state -> string -> state



(* [can_sell_house st prop_str] checks whether a player can sell a house on the
   property they've just landed on. It returns a boolean. *)
  val can_sell_house : state -> string -> bool

(* [has_houses] checks whether or not a play owns houses, returns a bool *)
  val has_houses : state -> bool

(** [sell_house st prop_str] sells a house at the location of the current player,
 *  provided that the current player already owns houses there.
 *)
   val sell_house : state -> string -> state


(** [can_buy_prop st prop_str] checks whether a player can buy the
 * property they've just landed on . It returns a tuple of two booleans.
 * the first is whether the property is not yet sold.
 * The second is whether the player has enough money to buy a house on the property.
*)
   val can_buy_prop : state -> string -> (bool)


(** [buy_property st prop_str] buys the property at the location of the current player,
 *  provided that no other player already owns that property.  *)
   val buy_property : state -> string -> state

(** [sell_property st prop_str] sells the property at the location of the current player,
 * provided that the current player already that property.
 *)
val sell_property : state -> string -> state


(* [can_sell_property st prop_str] checks whether a player can sell the specified
 *   property. It returns a boolean.
 *)
   val can_sell_property : state -> string -> bool


(** [ nuisance_helper st prop] returns a boolean indicating whether the nuisance
 * AI player should buy the property they just landed on
*)
   val nuisance_helper : state -> string -> bool

val smart_helper : state -> string -> bool

val twocolor_helper : state -> string -> bool

(* [can_sell_utility st util] returns a boolean indicating whether the current player
     can sell the utility [util] *)
val can_sell_utility : state -> string -> bool

(* [can_sell_transport st trans_str] returns a boolean indicating whether the current player
   can sell the transport [trans_str] *)
val can_sell_transport : state -> string -> bool

(* [get_top_card st deck] returns a (string, command) pair; the bool 'deck'
 * refers to Community Chest when true and Chance when false *)
val get_top_card : state -> bool -> string*Command.command


(**
  * [has_enough_money state p_payer min_money] returns a boolean indicating
  * whether player [p_payer] has at least [min_money] amount of money in
  * liquid assets
*)
val has_enough_money: state -> int -> int -> bool

val in_jail: state -> bool

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
 * requires: the input state is the predefined starting state given the number
 * of players, or by repeated applications of [do'] to such a state.
*)
val do' : Command.command -> state -> state

(* [win_state] returns when the game has ended, which is when one player exits
   the game, aka they have no money left or properties to sell, and winners
   are in order of highest to lowers amount of total net worth *)
val win_state : state -> state

(*********************************************************)

(* [description s] is the description of the current game state, including
 * player locations, inventory, money, and which player's turn it is*)
val description : state -> string
