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
  | AddCard of bool(* adds JA card to current player *)
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
let rec remove_empty lst =
  match lst with
  | [] -> []
  | h::t -> if (h="") then (remove_empty t) else (h::(remove_empty t))

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the valid commands forms described in
 * in our proposal (TO DO). *)
let parse str =
  let lower_str : string = String.lowercase_ascii str in
  let word_list : string list = (String.split_on_char ' ' lower_str) |> remove_empty in
  let first_word = List.hd word_list in
  let second_plus = String.concat (" ") (List.tl word_list) in
  if (List.length word_list) = 1 then
    match first_word with
    | "quit" -> Quit
    | "help" -> Help
    | "locations" -> Locations
    | "turns" -> Turns
    | "inventory" -> Inventory
    | "use" -> UseCard
    | "pay" -> UpdateMoney(0)
    | _ -> Invalid
  else match first_word with
    | "buy" -> begin
        try match String.sub second_plus 0 9 with
          | "house at " -> BuyHouse(String.sub second_plus 9 ((String.length second_plus)-9)) (* Need to later check if this is a valid "[house] at [property]" command *)
          | p -> BuyProperty(p)
        with _ -> BuyProperty(second_plus)
      end
    | "sell" -> begin
        try match String.sub second_plus 0 9 with
          | "house at " -> SellHouse(String.sub second_plus 9 ((String.length second_plus)-9)) (* Need to later check if this is a valid "[house] at [property]" command *)
          | "transport" -> SellTransport(String.sub second_plus 10 ((String.length second_plus)-10))
          | _ -> begin match String.sub second_plus 0 8 with
              | "utility " -> SellUtility(String.sub second_plus 8 ((String.length second_plus)-8))
              | _ -> failwith ""
            end
        with _ -> SellProperty(second_plus)
      end
    | _ -> Invalid
