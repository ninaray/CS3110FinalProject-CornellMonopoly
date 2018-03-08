(* [command] represents a command input by a player. *)
type command =
  | Quit (* quit game *)
  | Help (* print rules or valid commands *)
  | Locations (* current locations of pieces *)
  | Turns (* Number of turns so far. 1 turn = each player going once *)
  | Inventory (* Properties, cards, money *)
  | BuyProperty of string
  | BuyHouse of string (* buy houses and hotels for properties *)
  | SellProperty of string (* sell properties back to bank *)
  | SellHouse of string (* sell houses back to the bank *)
(* | Mortgage of string (* mortgage properties *) *)
  | BuyTransport of string
  | SellTransport of string
  | BuyUtility of string
  | SellUtility of string
  | UseCard(* i.e. use get out of JA's office free card *)
  | UpdateMoney of int (* changes current player's money by int *)
  | PayRent of int * int (* first int is the player being paid, second int is rent *)
  | MoveTo of int (* moves current player to space int *)
  | MoveForward of int (* moves current player forward int spaces *)
  | AddCard of bool (* adds JA card to current player; bool is the deck it is from true = cc, false = chance *)
  | CollectFromAll of int
  | ForEachBuilding of int * int
  | SendToJA
  | IncrementDouble
  | IncrementRound
  | IncrementPlayer
  | DrawCard of bool (* does NOT return a card; takes card from start of list and puts it at end; true = CC; false = chance *)
  | Invalid

(*********************************************************)

(* [remove_empty lst] returns a list with all "" elements removed from [lst] *)
val remove_empty : string list -> string list

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the valid commands forms described in
 * in our proposal (TO DO). *)
val parse : string -> command
