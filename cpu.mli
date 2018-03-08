module type Player = sig
  type t

  (* functions here *)
end

module type CPU = sig
  type t

(* functions here *)
(* [check_enough] takes in the current amount of money that the CPU has and
out puts whether or not they have enough money to purchase the property *)
  val check_enough : int -> bool

(* [purchase_property] takes in the original state, and outputs the state
   if they purchase in the strategy decision *)
  val purchase_property : state -> state

(* [purchase_house] takes in the original state, and outputs the state
   if they purchase in the strategy decision *)
  val purchase_house : state -> state

(* [sell_property] takes in the original state, and outputs the state
   if they sell in the strategy decision *)
  val sell_property : state -> state

(* [sell_house] takes in the original state, and outputs the state
   if they sell in the strategy decision *)
  val sell_house : state -> state

  include Player with type t:= t

end

(* functor to create multiple players *)
module type PlayerMaker =
  functor () -> Player

(* Always buy a property if they land on it as long as they have enough money
   + a savings amount determined at a later date *)
module AnyCPU : PlayerMaker
(* will use nested helper functions to excute the functions in the module,
   thus not in mli *)

(* Always buy a property when another player owns the same color property
   (basically preventing any player from ever setting up houses),
   as long as they have enough money + savings amount determined at a later date *)
module NuisanceCPU : PlayerMaker
(* will use nested helper functions to excute the functions in the module,
   thus not in mli *)

(* Always buy only two colored properties depending on which two are the first
   two properties that they land on. This way they build up from only two
   properties, keep adding houses, etc *)
module TwoColorCPU : PlayerMaker
(* will use nested helper functions to excute the functions in the module,
   thus not in mli *)
