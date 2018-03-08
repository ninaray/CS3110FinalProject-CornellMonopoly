open State
open Command

(* strings for the beginning of the game *)
let start_string = "\nWelcome to Monopoly!\nTo play the game, you need at least \
                    one player!\n"

let help_string = "Type any of the follow commands at any point in the game: \n \
'quit' : to end a game \n \
'locations' : to return the current locations of the players and piece in the game \n \
'inventory' or 'inv' : to return the current player's inventory \n \
'inventory all' or 'inv all' : to return the current player's inventory \n \
'turns' : to return the number of turns so far \n"

let invalid = "You've entered an invalid command. Please enter a valid command \
               or enter 'help' for a list of commands. \n"

let names = "0 - Bill Nye\n1 - Toni Morrison\n2 - Carl Sagan\n\
             3 - Ruth Bader Ginsburg\n4 - Mae Jemison \n"

let player_exists = "Sorry. This player already exists. Please choose another name!"

let quit_help_msg = "Type 'quit' to end the game, type 'help' to for more commands."

(* [print_val str] prints [str] and then terminates the program *)
let print_val str = print_endline str; ();;

let player_quits st =
  let curr = List.assoc st.current_player st.players in
  let _ = print_endline (curr.id^" has quit the game. Here are the final rankings!\n") in
  let _ = print_endline (print_rankings st) in
  print_val "Thanks for playing!"; st

let player_exit st =
  let curr = List.assoc st.current_player st.players in
  let _ = print_endline (curr.id^" has lost the game because they are out of money. \
                                Here are the final rankings!\n") in
  let _ = print_endline (print_rankings st) in
print_val "Thanks for playing!"; st

(* [print_properties lst] prints a statement containing a list of the current
 * player's properties. *)
let rec print_properties lst =
  match lst with
  | [] -> ""
  | tl::[] -> tl.pname
  | h::t -> (h.pname)^", "^(print_properties t)

(* [print_transports lst] prints a statement containing a list of the current
 * player's transports. *)
let rec print_transports lst =
  match lst with
  | [] -> ""
  | tl::[] -> tl.tname
  | h::t -> (h.tname)^", "^(print_transports t)

(* [print_utilities lst] prints a statement containing a list of the current
 * player's utilities. *)
let rec print_utilities lst =
  match lst with
  | [] -> ""
  | tl::[] -> tl.uname
  | h::t -> (h.uname)^", "^(print_utilities t)

(* [check_selling_utilities player st] checks whether [player] can
 * sell any utilities. If the player can, and chooses to do so, the function
 * creates a new state to reflect these changes. The function returns a print
 * statement depending on the outcome. *)
let rec check_selling_utilities (player : player) (st: state) : state =
  let cur_num = player.num in
  if (player.utilities <> 0) then
    let _ = print_endline "Would you like to sell any utilities? If so type \
                           'sell utility' followed by the name of the utility. \
                           For example: If you wanted to sell the Eduroam Wifi Network, \
                           type 'sell utility eduroam wifi network'. If you don't want \
                           to sell any utilities, type 'No'. Here is your current \
                           list of utilities:\n"
     in let _ = print_endline (print_utilities (List.filter (fun x -> x.uowner = cur_num) st.utilities))
     in match read_line() with
     | "No" | "n" | "no" -> let _ = print_endline "You are not selling any utilities.\n" in st
     | s -> begin
           match (parse s) with
           | SellUtility(p) -> begin
               if (can_sell_utility st p) then
                 let ns = do' (SellUtility(p)) st
                 in let _ = print_endline ("You have sold "^p^".\n")
                 in check_selling_utilities player ns
               else let _ = print_endline invalid in check_selling_utilities player st
             end
           | Quit -> player_quits st
           | Help -> let _ = print_endline help_string in check_selling_utilities player st
           | Locations -> let _ = print_endline "Printing locations"
             in check_selling_utilities player st
           | Turns -> let _ = print_endline ("Round: "^(string_of_int st.round))
             in check_selling_utilities player st
           | Inventory -> let _ = print_endline (inv st) in check_selling_utilities player st
           | _ -> let _ = print_endline invalid in check_selling_utilities player st
         end
  else let _ = print_endline "You have no utilities to sell.\n" in st

(* [check_selling_transports player st] checks whether [player] can
 * sell any transports. If the player can, and chooses to do so, the function
 * creates a new state to reflect these changes. The function returns a print
 * statement depending on the outcome, and proceeds to call [check_selling_utilities]
 * to check if [player] can sell any utilities. *)
let rec check_selling_transports (player : player) (st: state) : state =
  let cur_num = player.num in
  if (player.transports <> 0) then
    let _ = print_endline "Would you like to sell any transports? If so type \
                           'sell transport' followed by the name of the transport. \
                           For example: If you wanted to sell the TCAT Bus, \
                           type 'sell transport tcat bus'. If you don't want \
                           to sell any transports, type 'No'. Here is your current \
                           list of transports:\n"
    in let _ = print_endline (print_transports (List.filter (fun x -> x.towner = cur_num) st.transports))
    in match read_line() with
    | "No" | "n" | "no" -> begin
        let _ = print_endline "You are not selling any transports.\n"
        in check_selling_utilities player st
      end
    | s -> begin
          match (parse s) with
          | SellTransport(p) -> begin
              if (can_sell_transport st p) then
                let ns = do' (SellTransport(p)) st
                in let _ = print_endline ("You have sold "^p^".\n")
                in check_selling_transports player ns
              else let _ = print_endline invalid in check_selling_transports player st
            end
          | Quit -> player_quits st
          | Help -> begin
              let _ = print_endline help_string
              in check_selling_transports player st
            end
          | Locations -> let _ = print_endline "Printing locations"
            in check_selling_transports player st
          | Turns -> let _ = print_endline ("Round: "^(string_of_int st.round))
            in check_selling_transports player st
          | Inventory -> begin
              let _ = print_endline (inv st)
              in check_selling_transports player st
            end
          | _ -> let _ = print_endline invalid in check_selling_transports player st
        end
    else let _ = print_endline "You have no transports to sell.\n"
      in check_selling_utilities player st

(* [check_selling_houses player st] checks whether [player] can
* sell any houses. If the player can, and chooses to do so, the function
* creates a new state to reflect these changes. The function returns a print
* statement depending on the outcome. *)
let rec check_selling_houses (player : player) st =
  let cur_num = player.num in
  if (has_houses st) then (* If you have houses to sell *)
    let _ = print_endline "Would you like to sell any houses? If so type \
                          'sell house at' followed by the name of the \
                          property. For example: If you wanted to sell a \
                          house on Mann Library, you would type 'sell house \
                          at mann library'. If you don't want to sell any \
                          houses, type 'No'. Here is your current list of \
                          properties:\n"
    in let _ = print_endline (print_properties (List.filter (fun x -> x.powner = cur_num) st.properties))
    in match read_line() with
    | "No" | "no" | "n" -> begin
        let _ = print_endline "You are not selling any houses.\n"
        in st
      end
    | s -> begin
        match (parse s) with
        | SellHouse(h) -> begin
            if (can_sell_house st h) then
              let ns = do' (SellHouse(h)) st
              in let _ = print_endline ("You have sold a house at "^h^".\n")
              in check_selling_houses player ns
            else let _ = print_endline invalid in check_selling_houses player st
          end
        | Quit -> player_quits st
        | Help -> let _ = print_endline help_string in check_selling_houses player st
        | Locations -> let _ = print_endline "Printing locations" in check_selling_houses player st
        | Turns -> let _ = print_endline ("Round: "^(string_of_int st.round))
                in check_selling_houses player st
        | Inventory -> let _ = print_endline (inv st) in check_selling_houses player st
        | _ -> let _ = print_endline invalid in check_selling_houses player st
      end
  else let _ = print_endline "You have no houses to sell.\n" in st

(* [check_selling_properties player st] checks whether [player] can
 * sell any properties. If the player can, and chooses to do so, the function
 * creates a new state to reflect these changes. The function returns a print
 * statement depending on the outcome, and proceeds to call [check_selling_houses]
 * to check if [player] can sell any houses. *)
let rec check_selling_properties (player : player) st =
  let cur_num = player.num in
  if (player.properties <> []) then
    let _ = print_endline "Would you like to sell any properties? If so type \
                           'sell' followed by the name of the property. \
                           For example: If you wanted to sell Mann Library, \
                           you would type 'sell mann library'. If you don't \
                           want to sell any properties, type 'No'. Here is your \
                           current list of properties: \n"
    in let _ = print_endline (print_properties (List.filter (fun x -> x.powner = cur_num) st.properties))
    in match read_line() with
    | "No" | "n" | "no" -> begin
        let _ = print_endline "You are not selling any properties.\n"
        in check_selling_houses player st
      end
    | s -> begin
        match (parse s) with
        | SellProperty(p) -> begin
            if (can_sell_property st p) then
              let ns = do' (SellProperty(p)) st
              in let _ = print_endline ("You have sold "^p^".\n")
              in check_selling_properties player ns
            else let _ = print_endline invalid in check_selling_properties player st
          end
        | Quit -> player_quits st
        | Help -> begin
            let _ = print_endline help_string
            in check_selling_properties player st
          end
        | Locations -> let _ = print_endline "Printing locations"
          in check_selling_properties player st
        | Turns -> let _ = print_endline ("Round: "^(string_of_int st.round))
          in check_selling_properties player st
        | Inventory -> begin
            let _ = print_endline (inv st)
            in check_selling_properties player st
          end
        | _ -> let _ = print_endline invalid in check_selling_properties player st
      end
  else let _ = print_endline "You have no properties to sell.\n"
    in check_selling_houses player st

(* [check_double die_list] returns whether or not the current player has rolled
 * 2 die with the same numbers. *)
let check_double die_list =
  if List.length die_list = 2 then
    if ([List.hd die_list] = List.tl die_list) then true else false
  else false

let rec buy_prop_prompt st prop =
  let _ = print_endline ("Would you like to buy the " ^ prop.pcolor ^ " property " ^ prop.pname
          ^ " for $" ^ (string_of_int prop.pprice) ^
          "? \n - Enter 'y' or 'n', or 'help' for a list of other valid commands.\n") in
  match String.lowercase_ascii (read_line()) with
  | "y" | "yes" -> let _ = print_endline ("You've bought the property " ^ prop.pname ^
                                  " for $" ^ (string_of_int prop.pprice) ^ "!\n") in
    do' (BuyProperty prop.pname) st
  | "n" | "no" -> let _ = print_endline "Okay!\n" in st
  | "quit" -> player_quits st
  | "turns" -> let _ = print_endline ("Round: "^(string_of_int st.round)) in
    buy_prop_prompt st prop
  | "inventory" | "inv" -> let _ = print_endline (inv st) in buy_prop_prompt st prop
  | "inventory all" | "inv all" -> let _ = print_endline (inv_all st) in buy_prop_prompt st prop
  | "locations" -> let _ = print_endline (locations st) in buy_prop_prompt st prop
  | "help" -> let _ = print_endline help_string in buy_prop_prompt st prop
  | _ -> let _ = print_endline "That's an invalid entry. Please enter Y or N, \
                                or 'help' for a list of other valid commands.\n" in
    buy_prop_prompt st prop

let rec buy_tport_prompt st tport =
  let _ = print_endline ("Would you like to buy the transport " ^ tport.tname
        ^ " for $" ^ (string_of_int tport.tprice) ^
        "? \n - Enter 'y' or 'n', or 'help' for a list of other valid commands.\n") in
  match String.lowercase_ascii (read_line()) with
  | "y" | "yes" -> let _ = print_endline ("You've bought the transport " ^ tport.tname ^
                                " for $" ^ (string_of_int tport.tprice) ^ "!\n") in
    do' (BuyTransport tport.tname) st
  | "n" | "no" -> let _ = print_endline "Okay!\n" in st
  | "quit" -> player_quits st
  | "turns" -> let _ = print_endline ("Round: "^(string_of_int st.round)) in
    buy_tport_prompt st tport
  | "inventory" | "inv" -> let _ = print_endline (inv st) in buy_tport_prompt st tport
  | "inventory all" | "inv all" -> let _ = print_endline (inv_all st) in buy_tport_prompt st tport
  | "locations" -> let _ = print_endline (locations st) in buy_tport_prompt st tport
  | "help" -> let _ = print_endline help_string in buy_tport_prompt st tport
  | _ -> let _ = print_endline "That's an invalid entry. Please enter Y or N, \
                                or 'help' for a list of other valid commands.\n" in
    buy_tport_prompt st tport

let rec buy_util_prompt st util =
  let _ = print_endline ("Would you like to buy the utility " ^ util.uname
      ^ " for $" ^ (string_of_int util.uprice) ^
      "? \n - Enter 'y' or 'n', or 'help' for a list of other valid commands.\n") in
  match String.lowercase_ascii (read_line()) with
  | "y" | "yes" -> let _ = print_endline ("You've bought the utility " ^ util.uname ^
                              " for $" ^ (string_of_int util.uprice) ^ "!\n") in
    do' (BuyUtility util.uname) st
  | "n" | "no" -> let _ = print_endline "Okay!\n" in st
  | "quit" -> player_quits st
  | "turns" -> let _ = print_endline ("Round: "^(string_of_int st.round)) in
    buy_util_prompt st util
  | "inventory" | "inv" -> let _ = print_endline (inv st) in buy_util_prompt st util
  | "inventory all" | "inv all" -> let _ = print_endline (inv_all st) in buy_util_prompt st util
  | "locations" -> let _ = print_endline (locations st) in buy_util_prompt st util
  | "help" -> let _ = print_endline help_string in buy_util_prompt st util
  | _ -> let _ = print_endline "That's an invalid entry. Please enter Y or N, \
                                or 'help' for a list of other valid commands.\n" in
    buy_util_prompt st util

(*************************************************************** CPU *)

let rec ai_helper_move_twocolor state =
  if (in_jail state) then
    if (List.assoc state.current_player state.players).cards > 0
    then ai_helper_move_twocolor (do' (UpdateMoney(-200)) (do' (MoveTo(10)) state))
    else ai_helper_move_twocolor (do' (UpdateMoney(-250)) (do' (MoveTo(10)) state))
  else
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  (* let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(List.nth dice_roll 1 |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in *)
  let move_state = if (is_double) then
      let new_state = do' IncrementDouble state in
      if (new_state.double_counter > 2) then do' SendToJA new_state
      else do' (MoveForward num_steps) new_state
    else do' (MoveForward num_steps) {state with double_counter = 0}
  in

  let current_player = List.assoc move_state.current_player move_state.players in
  let current_location = List.assoc current_player.location move_state.board in
  match current_location with
  | Enroll -> let _ = print_endline "You've landed on Enroll! Collect $200\n" in
    do' (UpdateMoney 200) move_state
  | CommunityChest -> let _ = print_endline "You've landed on Community Chest! Draw a card" in
    let top_card = List.hd move_state.cc_cards in begin
      match top_card with
      | (str, AddCard true) -> let _ = print_endline str in
        do' (AddCard true) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard true) move_state)
    end
  | Chance -> let _ = print_endline "You've landed on Chance! Draw a card" in
    let top_card = List.hd move_state.chance_cards in begin
      match top_card with
      | (str, AddCard false) -> let _ = print_endline str in
        do' (AddCard false) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard false) move_state)
    end
  | Property init_p ->
    let p = List.hd (List.filter (fun x -> x.pname = init_p.pname) move_state.properties) in
    let _ = print_endline ("You've landed on the property " ^ p.pname ^ "!") in
    if (p.powned && p.powner <> current_player.num)
    then let _ = print_endline ((List.assoc current_player.num move_state.players).id ^
                                " pays " ^ (List.assoc p.powner move_state.players).id ^
                                " $" ^ (string_of_int p.pcurrent_rent) ^
                                " for rent on " ^ p.pname ^ ".\n") in
      do' (PayRent (p.powner, p.pcurrent_rent)) move_state
    else if (p.powned && p.powner = current_player.num) then
      let _ = print_endline "This is one of your own properties. Enjoy your stay!\n" in
      move_state
    else if (twocolor_helper move_state init_p.pname) then
      do' (BuyProperty p.pname) move_state
    else move_state
  | Transport init_t ->
    let t = List.hd (List.filter (fun x -> x.tname = init_t.tname) move_state.transports) in
    let _ = print_endline ("You've landed on the transport " ^ t.tname ^ "!") in
    if (t.towned && t.towner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int t.towner) ^
                                " $" ^ (string_of_int t.tcurrent_rent) ^
                                " for rent on " ^ t.tname ^ ".\n") in
      do' (PayRent (t.towner, t.tcurrent_rent)) move_state
    else move_state
  | Utility init_u ->
    let u = List.hd (List.filter (fun x -> x.uname = init_u.uname) move_state.utilities) in
    let _ = print_endline ("You've landed on the utility " ^ u.uname ^ "!") in
    if (u.uowned && u.uowner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int u.uowner) ^
                                " $" ^ (string_of_int u.ucurrent_rent) ^
                                " for rent on " ^ u.uname ^ ".\n") in
      do' (PayRent (u.uowner, u.ucurrent_rent)) move_state
    else move_state
  | Visiting ->
    let _ = print_endline "You are visiting the JA's office! No need to worry, \
                           you're not in trouble yet!" in
    move_state
  | GoToJA -> let _ = print_endline "You've landed on 'Go to the JA's Office! \n
                                         You've been sent to the JA's office and \
                                     will be able to attempt to get out on your \
                                     next turn.\n" in
    do' SendToJA move_state
  | Tax (name, amount) ->
    let _ = print_endline ("You've landed on " ^ name ^ " and are charged $" ^
                           (string_of_int amount) ^ " by the Bursar's Office.\n") in
    do' (UpdateMoney (-amount)) move_state
  | FreeParking -> let _ = print_endline "You've landed in Cornell's mythical free parking! \
                                          Enjoy your stay!\n" in
    move_state
  | JAsOffice -> let _ = print_endline "You're in the JA's Office!\n" in
    move_state

let rec ai_helper_move_smart state =
  if (in_jail state) then
    if (List.assoc state.current_player state.players).cards > 0
    then ai_helper_move_smart (do' (UpdateMoney(-200)) (do' (MoveTo(10)) state))
    else ai_helper_move_smart (do' (UpdateMoney(-250)) (do' (MoveTo(10)) state))
  else
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  (* let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(List.nth dice_roll 1 |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in *)
  let move_state = if (is_double) then
      let new_state = do' IncrementDouble state in
      if (new_state.double_counter > 2) then do' SendToJA new_state
      else do' (MoveForward num_steps) new_state
    else do' (MoveForward num_steps) {state with double_counter = 0}
  in

  let current_player = List.assoc move_state.current_player move_state.players in
  let current_location = List.assoc current_player.location move_state.board in
  match current_location with
  | Enroll -> let _ = print_endline "You've landed on Enroll! Collect $200\n" in
    do' (UpdateMoney 200) move_state
  | CommunityChest -> let _ = print_endline "You've landed on Community Chest! Draw a card" in
    let top_card = List.hd move_state.cc_cards in begin
      match top_card with
      | (str, AddCard true) -> let _ = print_endline str in
        do' (AddCard true) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard true) move_state)
    end
  | Chance -> let _ = print_endline "You've landed on Chance! Draw a card" in
    let top_card = List.hd move_state.chance_cards in begin
      match top_card with
      | (str, AddCard false) -> let _ = print_endline str in
        do' (AddCard false) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard false) move_state)
    end
  | Property init_p ->
    let p = List.hd (List.filter (fun x -> x.pname = init_p.pname) move_state.properties) in
    let _ = print_endline ("You've landed on the property " ^ p.pname ^ "!") in
    if (p.powned && p.powner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int p.powner) ^
                                " $" ^ (string_of_int p.pcurrent_rent) ^
                                " for rent on " ^ p.pname ^ ".\n") in
      do' (PayRent (p.powner, p.pcurrent_rent)) move_state
    else if (p.powned && p.powner = current_player.num) then
      let _ = print_endline "This is one of your own properties. Enjoy your stay!\n" in
      move_state
    else if (smart_helper move_state init_p.pname) then
      do' (BuyProperty p.pname) move_state
    else move_state
  | Transport init_t ->
    let t = List.hd (List.filter (fun x -> x.tname = init_t.tname) move_state.transports) in
    let _ = print_endline ("You've landed on the transport " ^ t.tname ^ "!") in
    if (t.towned && t.towner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int t.towner) ^
                                " $" ^ (string_of_int t.tcurrent_rent) ^
                                " for rent on " ^ t.tname ^ ".\n") in
      do' (PayRent (t.towner, t.tcurrent_rent)) move_state
    else if (t.towned && t.towner = current_player.num) then
      let _ = print_endline "This is one of your own transports. Enjoy your ride!\n" in
      move_state
    else if (has_enough_money move_state current_player.num t.tprice)
    then do' (BuyTransport t.tname) move_state
    else
      let _ = print_endline "You have landed on an unowned transportation, but you \
                             do not have enough to buy it. Sorry!\n" in
      move_state
  | Utility init_u ->
    let u = List.hd (List.filter (fun x -> x.uname = init_u.uname) move_state.utilities) in
    let _ = print_endline ("You've landed on the utility " ^ u.uname ^ "!") in
    if (u.uowned && u.uowner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int u.uowner) ^
                                " $" ^ (string_of_int u.ucurrent_rent) ^
                                " for rent on " ^ u.uname ^ ".\n") in
      do' (PayRent (u.uowner, u.ucurrent_rent)) move_state
    else move_state
  | Visiting ->
    let _ = print_endline "You are visiting the JA's office! No need to worry, \
                           you're not in trouble yet!" in
    move_state
  | GoToJA -> let _ = print_endline "You've landed on 'Go to the JA's Office! \n
                                          You've been sent to the JA's office and \
                                     will be able to attempt to get out on your \
                                     next turn.\n" in
    do' SendToJA move_state
  | Tax (name, amount) ->
    let _ = print_endline ("You've landed on " ^ name ^ " and are charged $" ^
                           (string_of_int amount) ^ " by the Bursar's Office.\n") in
    do' (UpdateMoney (-amount)) move_state
  | FreeParking -> let _ = print_endline "You've landed in Cornell's mythical free parking! \
                                          Enjoy your stay!\n" in
    move_state
  | JAsOffice -> let _ = print_endline "You're in the JA's Office!\n" in
    move_state

let rec ai_helper_move_nuisance state =
  if (in_jail state) then
    if (List.assoc state.current_player state.players).cards > 0
    then ai_helper_move_nuisance (do' (UpdateMoney(-200)) (do' (MoveTo(10)) state))
    else ai_helper_move_nuisance (do' (UpdateMoney(-250)) (do' (MoveTo(10)) state))
  else
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  (* let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(List.nth dice_roll 1 |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in *)
  let move_state = if (is_double) then
      let new_state = do' IncrementDouble state in
      if (new_state.double_counter > 2) then do' SendToJA new_state
      else do' (MoveForward num_steps) new_state
    else do' (MoveForward num_steps) {state with double_counter = 0}
  in

  let current_player = List.assoc move_state.current_player move_state.players in
  let current_location = List.assoc current_player.location move_state.board in
  match current_location with
  | Enroll -> let _ = print_endline "You've landed on Enroll! Collect $200\n" in
    do' (UpdateMoney 200) move_state
  | CommunityChest -> let _ = print_endline "You've landed on Community Chest! Draw a card" in
    let top_card = List.hd move_state.cc_cards in begin
      match top_card with
      | (str, AddCard true) -> let _ = print_endline str in
        do' (AddCard true) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard true) move_state)
    end
  | Chance -> let _ = print_endline "You've landed on Chance! Draw a card" in
    let top_card = List.hd move_state.chance_cards in begin
      match top_card with
      | (str, AddCard false) -> let _ = print_endline str in
        do' (AddCard false) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard false) move_state)
    end
  | Property init_p ->
    let p = List.hd (List.filter (fun x -> x.pname = init_p.pname) move_state.properties) in
    let _ = print_endline ("You've landed on the property " ^ p.pname ^ "!") in
    if (p.powned && p.powner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int p.powner) ^
                                " $" ^ (string_of_int p.pcurrent_rent) ^
                                " for rent on " ^ p.pname ^ ".\n") in
      do' (PayRent (p.powner, p.pcurrent_rent)) move_state
    else if (p.powned && p.powner = current_player.num) then
      let _ = print_endline "This is one of your own properties. Enjoy your stay!\n" in
      move_state
    else if (nuisance_helper move_state init_p.pname) then
      do' (BuyProperty p.pname) move_state
    else move_state
  | Transport init_t ->
    let t = List.hd (List.filter (fun x -> x.tname = init_t.tname) move_state.transports) in
    let _ = print_endline ("You've landed on the transport " ^ t.tname ^ "!") in
    if (t.towned && t.towner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int t.towner) ^
                                " $" ^ (string_of_int t.tcurrent_rent) ^
                                " for rent on " ^ t.tname ^ ".\n") in
      do' (PayRent (t.towner, t.tcurrent_rent)) move_state
    else
      move_state
  | Utility init_u ->
    let u = List.hd (List.filter (fun x -> x.uname = init_u.uname) move_state.utilities) in
    let _ = print_endline ("You've landed on the utility " ^ u.uname ^ "!") in
    if (u.uowned && u.uowner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int u.uowner) ^
                                " $" ^ (string_of_int u.ucurrent_rent) ^
                                " for rent on " ^ u.uname ^ ".\n") in
      do' (PayRent (u.uowner, u.ucurrent_rent)) move_state
    else move_state
  | Visiting ->
    let _ = print_endline "You are visiting the JA's office! No need to worry, \
                           you're not in trouble yet!" in
    move_state
  | GoToJA -> let _ = print_endline "You've landed on 'Go to the JA's Office! \n
                                     You've been sent to the JA's office and \
                                     will be able to attempt to get out on your \
                                     next turn.\n" in
    do' SendToJA move_state
  | Tax (name, amount) ->
    let _ = print_endline ("You've landed on " ^ name ^ " and are charged $" ^
                           (string_of_int amount) ^ " by the Bursar's Office.\n") in
    do' (UpdateMoney (-amount)) move_state
  | FreeParking -> let _ = print_endline "You've landed in Cornell's mythical free parking! \
                                          Enjoy your stay!\n" in
    move_state
  | JAsOffice -> let _ = print_endline "You're in the JA's Office!\n" in
    move_state

let rec ai_buy_sell_helper_twocolor st =
  let cur_num = st.current_player in
  let player_obj = List.assoc cur_num st.players in
  (* not enough money to buy, must sell *)
  if (player_obj.money < 500) then
    (* let diff = 500 - player_obj.money in *)
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_houses = List.filter
        (fun x -> (x.powner = cur_num) && (x.phouses > 0)) prop_list in
    if possible_houses <> [] then
      let hd_house = List.hd possible_houses in
      let ns = do' (SellHouse hd_house.pname) st in
      ai_buy_sell_helper_twocolor ns
    else
    if list_properties <> [] then
      let hd_property = List.hd list_properties in
      let ns = do' (SellProperty hd_property) st in
      ai_buy_sell_helper_twocolor ns
    else
      ai_helper_move_smart st
  else if (player_obj.money > 1000) then
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_buys = List.filter (fun x -> (can_buy_house st (String.lowercase_ascii x.pname))&&(x.phouses<=4)) prop_list in
    if (possible_buys <> []) then
      let p_head = List.hd possible_buys in
      let ns = do'(BuyHouse p_head.pname) st in
      ai_buy_sell_helper_twocolor ns
    else
      ai_helper_move_twocolor st
  else
    ai_helper_move_twocolor st

let rec ai_buy_sell_helper_nuisance st =
  let cur_num = st.current_player in
  let player_obj = List.assoc cur_num st.players in
  (* not enough money to buy, must sell *)
  if (player_obj.money < 500) then
    (* let diff = 500 - player_obj.money in *)
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_houses = List.filter
        (fun x -> (x.powner = cur_num) && (x.phouses > 0)) prop_list in
    if possible_houses <> [] then
      let hd_house = List.hd possible_houses in
      let ns =do'  (SellHouse(hd_house.pname)) st in
      ai_buy_sell_helper_nuisance ns
    else
    if list_properties <> [] then
      let hd_property = List.hd list_properties in
      let ns = sell_property st hd_property in
      ai_buy_sell_helper_nuisance ns
    else
      ai_helper_move_nuisance st
  else
    ai_helper_move_nuisance st

let rec ai_buy_sell_helper_smart st =
  let cur_num = st.current_player in
  let player_obj = List.assoc cur_num st.players in
  (* not enough money to buy, must sell *)
  if (player_obj.money < 500) then
    (* let diff = 500 - player_obj.money in *)
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_houses = List.filter
        (fun x -> (x.powner = cur_num) && (x.phouses > 0)) prop_list in
    if possible_houses <> [] then
      let hd_house = List.hd possible_houses in
      let ns = sell_house st hd_house.pname in
      ai_buy_sell_helper_smart ns
    else
    if list_properties <> [] then
      let hd_property = List.hd list_properties in
      let ns = sell_property st hd_property in
      ai_buy_sell_helper_smart ns
    else
      ai_helper_move_smart st
  else if (player_obj.money > 1000) then
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_buys = List.filter (fun x -> (can_buy_house st (String.lowercase_ascii x.pname))&&(x.phouses<=2)) prop_list in
    if (possible_buys <> []) then
      let p_head = List.hd possible_buys in
      let ns = buy_house st p_head.pname in
      ai_buy_sell_helper_smart ns
    else
      ai_helper_move_smart st
  else
    ai_helper_move_smart st

(************************************************************ END OF CPU *)

(* [move_spaces state] allows the current player to roll the dice and proceed
 * forward an arbitrary amount of spaces. *)
let move_spaces state =
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(dice_roll |> List.rev |> List.hd |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in
  let str = if (is_double) then "Since you have rolled a double, you \
                                will be able to roll again after this turn.\n"
    else "" in let _ = print_endline str in
  let move_state = if (is_double) then
      let new_state = do' IncrementDouble state in
      if (new_state.double_counter > 2) then do' SendToJA new_state
      else do' (MoveForward num_steps) new_state
    else do' (MoveForward num_steps) {state with double_counter = 0}
  in

  let current_player = List.assoc move_state.current_player move_state.players in
  let current_location = List.assoc current_player.location move_state.board in
  match current_location with
  | Enroll -> let _ = print_endline "You've landed on Enroll! Collect $200\n" in
    do' (UpdateMoney 200) move_state
  | CommunityChest -> let _ = print_endline "You've landed on Community Chest! Draw a card" in
    let top_card = List.hd move_state.cc_cards in begin
      match top_card with
      | (str, AddCard true) -> let _ = print_endline str in
        do' (AddCard true) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard true) move_state)
  end
  | Chance -> let _ = print_endline "You've landed on Chance! Draw a card" in
    let top_card = List.hd move_state.chance_cards in begin
      match top_card with
      | (str, AddCard false) -> let _ = print_endline str in
        do' (AddCard false) move_state
      | (str, com) -> let _ = print_endline (str ^ "\n") in
        do' com (do' (DrawCard false) move_state)
    end
  | Property init_p ->
    let p = List.hd (List.filter (fun x -> x.pname = init_p.pname) move_state.properties) in
    let _ = print_endline ("You've landed on the property " ^ p.pname ^ "!") in
    if (p.powned && p.powner <> current_player.num)
    then let _ = print_endline (current_player.id^
                                " pays "^((List.assoc p.powner state.players).id) ^
                                " $" ^ (string_of_int p.pcurrent_rent) ^
                                " for rent on " ^ p.pname ^ ".\n") in
      do' (PayRent (p.powner, p.pcurrent_rent)) move_state
    else if (p.powned && p.powner = current_player.num) then
      let _ = print_endline "This is one of your own properties. Enjoy your stay!\n" in
      move_state
    else if (can_buy_prop move_state p.pname)
    then buy_prop_prompt move_state p
    else
    let _ = print_endline "This property is unowned, but you \
                           do not have enough to buy it. Sorry!\n" in
      move_state
  | Transport init_t ->
    let t = List.hd (List.filter (fun x -> x.tname = init_t.tname) move_state.transports) in
    let _ = print_endline ("You've landed on the transport " ^ t.tname ^ "!") in
    if (t.towned && t.towner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int t.towner) ^
                                " $" ^ (string_of_int t.tcurrent_rent) ^
                                " for rent on " ^ t.tname ^ ".\n") in
      do' (PayRent (t.towner, t.tcurrent_rent)) move_state
    else if (t.towned && t.towner = current_player.num) then
      let _ = print_endline "This is one of your own transports. Enjoy your ride!\n" in
      move_state
    else if (has_enough_money move_state current_player.num t.tprice)
    then buy_tport_prompt move_state t
    else
      let _ = print_endline "You have landed on an unowned transportation, but you \
                             do not have enough to buy it. Sorry!\n" in
      move_state
  | Utility init_u ->
    let u = List.hd (List.filter (fun x -> x.uname = init_u.uname) move_state.utilities) in
    let _ = print_endline ("You've landed on the utility " ^ u.uname ^ "!") in
    if (u.uowned && u.uowner <> current_player.num)
    then let _ = print_endline ("Player " ^ (string_of_int current_player.num) ^
                                " pays player " ^ (string_of_int u.uowner) ^
                                " $" ^ (string_of_int u.ucurrent_rent) ^
                                " for rent on " ^ u.uname ^ ".\n") in
      do' (PayRent (u.uowner, u.ucurrent_rent)) move_state
    else if (u.uowned && u.uowner = current_player.num) then
      let _ = print_endline "This is one of your own wifi networks. \
                             Browse Make Cornell Meme Again while you're here!\n" in
      move_state
    else if (has_enough_money move_state current_player.num u.uprice)
    then buy_util_prompt move_state u
    else
      let _ = print_endline "Though this is an unowned utility, but you \
                             do not have enough to buy it. Sorry!\n" in
      move_state
  | Visiting ->
    let _ = print_endline "You are visiting the JA's office! No need to worry, \
                                       you're not in trouble yet!" in
    move_state
  | GoToJA -> let _ = print_endline "You've landed on 'Go to the JA's Office! \n \
                                     You've been sent to the JA's office and \
                                     will be able to attempt to get out on your \
                                     next turn.\n" in
    do' SendToJA move_state
  | Tax (name, amount) ->
    let _ = print_endline ("You've landed on " ^ name ^ " and are charged $" ^
              (string_of_int amount) ^ " by the Bursar's Office.\n") in
    do' (UpdateMoney (-amount)) move_state
  | FreeParking -> let _ = print_endline "You've landed in Cornell's mythical free parking! \
                                           Enjoy your stay!\n" in
    move_state
  | JAsOffice -> let _ = print_endline "You're in the JA's Office!\n" in
    move_state
  (* if move_state.double_counter <> 0 then  *)

(* [check_jail player ns] checks whether [player] is currently on the JA Office
 * square. It then allows [player] to use a Card to get out of the Office, pay
 * $50, or attempt to roll doubles. The function returns a print statement
 * depending on the outcome. *)
let rec check_jail player ns =
  let _ = print_endline "OH NO! You're in the JA's office! \n"
  in if (player.cards > 0) then
    let _ = print_endline "It looks like you have a 'Get Out Of JA's Office \
                           Free' Card. If you would like to use it, type \
                           'use'. If you want to enroll in a B.A.S.I.C.S \
                           course instead for $50, then type 'pay'. \
                           If you want to try to roll a double instead to \
                           get out of jail, type 'roll'.\n"
    in match String.lowercase_ascii (read_line()) with
    | "roll" -> begin
        let die_result = roll_die 2
        in if (List.hd die_result = List.nth die_result 1) then
          let _ = print_endline "Congratulations! You've rolled a double!" in
          move_spaces (do' (UpdateMoney(-200)) (do' (MoveTo(10)) ns))
        else let _ = print_endline "Sorry! You didn't roll a double! Maybe next time! \n" in ns
      end
    | s -> begin
        match parse s with
        | UseCard -> begin
            let _ = print_endline "You have used your card to get out of the JA's office."
            in move_spaces (do' (UpdateMoney(-200)) (do' (MoveTo(10)) ns))
          end
        | UpdateMoney(v) -> begin
            let _ = print_endline "You have enrolled in a B.A.S.I.C.S course for \
                                   $50. Congratulations on learning about alcohol safety! You have exited \
                                   the JA's office."
            in move_spaces (do' (UpdateMoney(-250)) (do' (MoveTo(10)) ns))
          end
        | Quit -> player_quits ns
        | Help -> begin
            let _ = print_endline help_string
            in check_jail player ns
          end
        | Locations -> let _ = print_endline "Printing locations"
          in check_jail player ns
        | Turns -> let _ = print_endline ("Round: "^(string_of_int ns.round))
          in check_jail player ns
        | Inventory -> begin
            let _ = print_endline (inv ns)
            in check_jail player ns
          end
        | _ -> let _ = print_endline invalid in check_jail player ns
      end
  else let _ = print_endline "It looks like you don't have a 'Get Out Of JA's Office Free' \
                Card. If you want to enroll in a B.A.S.I.C.S course instead for \
                $50, then type 'pay'. If you want to try to roll a double \
                instead to get out of jail, type 'roll'.\n"
    in match String.lowercase_ascii (read_line()) with
    | "roll" -> begin
        let die_result = roll_die 2
        in if (List.hd die_result = List.nth die_result 1) then
          let _ = print_endline "Congratulations! You've rolled a double!" in
          move_spaces (do' (UpdateMoney(-200)) (do' (MoveTo(10)) ns))
        else let _ = print_endline "Sorry! You didn't roll a double! Maybe next time! \n" in ns
      end
    | s -> begin
        match parse s with
        | UpdateMoney(v) -> begin
            let _ = print_endline "You have enrolled in a B.A.S.I.C.S course for \
                                   $50. Congratulations on learning about alcohol safety! You have exited \
                                   the JA's office."
            in move_spaces (do' (UpdateMoney(-250)) (do' (MoveTo(10)) ns))
          end
        | Quit -> player_quits ns
        | Help -> begin
            let _ = print_endline help_string
            in check_jail player ns
          end
        | Locations -> let _ = print_endline "Printing locations"
          in check_jail player ns
        | Turns -> let _ = print_endline ("Round: "^(string_of_int ns.round))
          in check_jail player ns
        | Inventory -> begin
            let _ = print_endline (inv ns)
            in check_jail player ns
          end
        | _ -> let _ = print_endline invalid in check_jail player ns
      end

let buy_sell_properties_houses (curr : player) (st : state) : state =
  let sell_state = check_selling_properties curr st
  in let rec buy_houses house_state =
      let _ = print_endline "Would you like to buy any houses? If so, please \
                              type 'buy house at [property]' where [property] \
                              is the name of the property you want to buy a house \
                              on. For example, if you wanted to buy a house on \
                              the Dairy Bar, you would type 'buy house at dairy bar'. \
                              Remember that each property can only have up to 5 houses \
                              at a time. If you need more information, enter 'help'.\n"
      in match String.lowercase_ascii (read_line()) with
      | "no" | "n" -> house_state
      | s -> begin
          match (parse s) with
          | BuyHouse(h) -> begin
              if (can_buy_house house_state h) then let ns = do' (BuyHouse(h)) house_state
                in let _ = print_endline "You have bought a house at this property.\n\n"
                       in buy_houses ns
              else let _ = print_endline "Sorry. That didn't work. Would you like to \
                                          try again (y) or continue and roll the dice (n)?\n\n"
                in match String.lowercase_ascii (read_line()) with
                | "n" -> house_state
                | "y" -> buy_houses house_state
                | _ -> let _ = print_endline invalid in buy_houses house_state
            end
          | Quit -> player_quits house_state
          | Help -> begin
              let _ = print_endline help_string
              in buy_houses house_state
            end
          | Locations -> let _ = print_endline "Printing locations"
              in buy_houses house_state
          | Turns -> let _ = print_endline ("Round: "^(string_of_int house_state.round))
              in buy_houses house_state
          | Inventory -> begin
              let _ = print_endline (inv house_state)
              in buy_houses house_state
            end
          | _ -> let _ = print_endline invalid in buy_houses house_state
        end
  in let util_trans_state =
       if ((List.assoc sell_state.current_player sell_state.players).properties <> [])
       then buy_houses sell_state else sell_state
  in check_selling_transports curr util_trans_state

(* [repl state] handles whether the user is stuck in jail, or is allowed to
 * proceed in the game like normal. *)
let rec repl state =
  let curr = List.assoc state.current_player state.players in
  let _ = print_endline ("It is "^curr.id^"'s turn:\n") in
  let rec ready_helper st =
    let _ = print_endline ("Are you ready to roll? Enter 'y', otherwise enter \
                            'n' if you want to sell any properties, houses, \
                            tranports, utilities or if you want to buy any houses.\n \
                            * Remember, if you're in debt, and are not able to \
                            get out of debt before rolling, you will lose! So sad!\
                             \n * Enter 'help' for a list of other valid commands. \
                             Below is a current look at your inventory. \n") in
    let _ = print_endline (inv st) in
    match String.lowercase_ascii (read_line()) with
    | "y" |"yes" ->
      if (curr.money < 0) then player_exit st
      (* if the player is in jail... *)
      else if (in_jail st) then
        let ns = check_jail curr st in repl (do' IncrementPlayer ns)
        (* not in jail *)
      else let ns = move_spaces st in
        if (ns.double_counter > 0 && ns.double_counter < 3) then repl ns
        else repl (do' IncrementPlayer ns)
    | "n" | "no" -> let ns = (buy_sell_properties_houses curr st) in ready_helper ns
    | "quit" -> player_quits st
    | "turns" -> let _ = print_endline ("Round: "^(string_of_int st.round)) in
      ready_helper st
    | "inventory" | "inv" -> let _ = print_endline (inv st) in ready_helper st
    | "inventory all" | "inv all" -> let _ = print_endline (inv_all st) in ready_helper st
    | "locations" -> let _ = print_endline (locations st) in ready_helper st
    | "help" -> let _ = print_endline help_string in ready_helper st
    | _ -> let _ = print_endline "That's an invalid entry. Please enter Y or N" in
      ready_helper st
  in
  match curr.strategy with
  | Human -> ready_helper state
  | Smart -> let ns = ai_buy_sell_helper_smart state in repl (do' IncrementPlayer ns)
  | Nuisance -> let ns = ai_buy_sell_helper_nuisance state in repl (do' IncrementPlayer ns)
  | TwoColor -> let ns = ai_buy_sell_helper_twocolor state in repl (do' IncrementPlayer ns)

(* print_order prints out the list of players in the order that they will play
   in after the die has been rolled *)
let rec print_order lst count ns =
  try match List.assoc count lst with
    | p -> begin
        let _ = print_endline ((string_of_int count)^" : "^p.id^"\n")
        in print_order lst (count+1) ns
      end
  with Not_found -> let _ = print_endline "" in
    let _ = print_endline "Each player starts at 0 - Enroll, and with $1500. \n" in
    repl ns

(* initial state outputs the intial state of the game *)
let initial_state player_list =
  let ns = init_state player_list in
  let _ = print_endline "Each player has rolled a die, and the order has been \
                         determined as follows: \n"
  in print_order ns.players 1 ns


(* Can only create HUMAN players, not CPU players *)
let rec create_players list_of_names list_of_players counter =
  if counter<4 then
    let _ = print_endline "Please choose another name to add to the game. If \
                           you don't want to add any more players, enter
                          'N' and we will fill up the game with CPU players!\n"

    in let _ = print_endline names
    in match read_line() with
    | "0" -> begin
        if (List.mem "Bill Nye" list_of_names) then
          let _ = print_endline "Sorry, this player is already taken!. \n"
          in create_players list_of_names list_of_players counter
        else
          let _ = print_endline "You have chosen Bill Nye as your next player. \n" in
          let new_player = instantiate_player 0 "Bill Nye" Human "Bill Nye"
          in create_players ("Bill Nye"::list_of_names) (new_player::list_of_players) (counter+1)
      end
    | "1" -> begin
        if (List.mem "Toni Morrison" list_of_names) then
          let _ = print_endline "Sorry, this player is already taken!. \n"
          in create_players list_of_names list_of_players counter
        else
          let _ = print_endline "You have chosen Toni Morrison as your next player. \n" in
          let new_player = instantiate_player 0 "Toni Morrison" Human "Toni Morrison"
          in create_players ("Toni Morrison"::list_of_names) (new_player::list_of_players) (counter+1)
      end
    | "2" -> begin
        if (List.mem "Carl Sagan" list_of_names) then
          let _ = print_endline "Sorry, this player is already taken!. \n"
          in create_players list_of_names list_of_players counter
        else
          let _ = print_endline "You have chosen Carl Sagan as your next player. \n" in
          let new_player = instantiate_player 0 "Carl Sagan" Human "Carl Sagan"
          in create_players ("Carl Sagan"::list_of_names) (new_player::list_of_players) (counter+1)
      end
    | "3" -> begin
        if (List.mem "Ruth Bader Ginsburg" list_of_names) then
          let _ = print_endline "Sorry, this player is already taken!. \n"
          in create_players list_of_names list_of_players counter
        else
          let _ = print_endline "You have chosen Ruth Bader Ginsburg as your next player. \n" in
          let new_player = instantiate_player 0 "Ruth Bader Ginsburg" Human "Ruth Bader Ginsburg"
          in create_players ("Ruth Bader Ginsburg"::list_of_names) (new_player::list_of_players) (counter+1)
      end
    | "4" -> begin
        if (List.mem "Mae Jemison" list_of_names) then
          let _ = print_endline "Sorry, this player is already taken!. \n"
          in create_players list_of_names list_of_players counter
        else
          let _ = print_endline "You have chosen Mae Jemison as your next player. \n" in
          let new_player = instantiate_player 0 "Mae Jemison" Human "Mae Jemison"
          in create_players ("Mae Jemison"::list_of_names) (new_player::list_of_players) (counter+1)
      end
    | "N" | "n" -> begin
        if counter = 1 then
          let ai1 = instantiate_player 0 "AI 1" Smart "AI 1"
          in let ai2 = instantiate_player 0 "AI 2" Nuisance "AI 2"
          in let ai3 = instantiate_player 0 "AI 3" TwoColor "AI 3"
          in initial_state (ai1::ai2::ai3::list_of_players)
        else if counter = 2 then
          let ai1 = instantiate_player 0 "AI 1" Smart "AI 1"
          in let ai2 = instantiate_player 0 "AI 2" Nuisance "AI 2"
          in initial_state (ai1::ai2::list_of_players)
        else let ai1 = instantiate_player 0 "AI 1" Smart "AI 1" in initial_state (ai1::list_of_players)
      end
    | _ -> let _ = print_endline invalid in create_players list_of_names list_of_players counter
  else let _ = print_endline "Looks like our game is filled up! Let's begin. \n"
    in initial_state list_of_players

let rec create_first_player list_of_players counter =
  let _ = print_endline "To start, please choose a number that corresponds \
                         to one of following names. This will be the name \
                         of your human player! Press any other key to default \
                         to Bill Nye. He's a pretty cool guy. \n"
  in
  let _ = print_endline names
  in
  let p_name = match read_line() with
    | "1" -> "Toni Morrison"
    | "2" -> "Carl Sagan"
    | "3" -> "Ruth Bader Ginsburg"
    | "4" -> "Mae Jemison"
    | _ -> "Bill Nye"
  in let _ = print_endline ("You have chosen "^p_name^" as your first player. \n")
  in create_players (p_name::[]) ((instantiate_player 0 p_name Human p_name)::list_of_players) (counter+1)

let main () =
  let _ = print_endline start_string
  in create_first_player [] 0

let x = main () (* Should probably change back to unit to end game *)
