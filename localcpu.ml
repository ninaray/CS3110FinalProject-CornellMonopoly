open State
open Command

let smart_helper st prop =
  let myp_list = st.current_player.properties in
  if List.length myp_list =0 then (can_buy_prop st prop)
  else if (List.length myp_list =1) then
  let p_list = st.properties in
  let owned_list = List.filter (fun x -> (x.pname = st.current_player)) p_list in
  let color_list = "orange"::"light blue"::"pink"::List.map (fun x -> x.pcolor) owned_list in
  let match_list = List.filter (fun x -> x.pname = prop) p_list in
  let prop_to_buy = List.hd match_list in
  List.mem prop_to_buy.pcolor color_list
  else
    false

let twocolor_helper st prop =
  let myp_list = st.current_player.properties in
  if (List.length myp_list <=1)
  then (can_buy_prop st prop)
  else
    let p_list = st.properties in
    let owned_list = List.filter (fun x -> (x.pname = st.current_player)) p_list in
    let color_list = List.map (fun x -> x.pcolor) owned_list in
    let match_list = List.filter (fun x -> x.pname = prop) p_list in
    let prop_to_buy = List.hd match_list in
    List.mem prop_to_buy.pcolor color_list


let rec ai_buy_sell_helper_twocolor st =
  let cur_num = st.current_player in
  let player_obj = List.assoc cur_num st.players in
  (* not enough money to buy, must sell *)
  if (player_obj.money < 500) then
    (* let diff = 500 - player_obj.money in *)
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_houses = List.filter
        (fun x -> (x.powner = curr_num) && (x.phouses > 0)) prop_list in
    if possible_houses <> [] then
      let hd_house = List.hd possible_houses in
      let ns = sell_house st hd_house.pname in
      ai_buy_sell_helper_twocolor ns
    else
    if list_properties <> [] then
      let hd_property = List.hd list_properties in
      let ns = sell_property st hd_property in
      ai_buy_sell_helper_twocolor ns
    else
      ai_helper_move_smart st
  else if (player_obj.money > 1000) then
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_buys = List.filter (fun x -> (can_buy_house st x.pname)&&(x.phouses<=4)) prop_list in
    if (possible_buys <> []) then
      let p_head = List.hd possible_buys in
      let ns = buy_house st p_head.pname in
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
        (fun x -> (x.powner = curr_num) && (x.phouses > 0)) prop_list in
    if possible_houses <> [] then
      let hd_house = List.hd possible_houses in
      let ns = sell_house st hd_house.pname in
      ai_buy_sell_helper_nuisance ns
    else
    if list_properties <> [] then
      let hd_property = List.hd list_properties in
      let ns = sell_property st hd_property in
      ai_buy_sell_helper_nuisance ns
    else
      ai_helper_move_nuisance st
  else
    ai_helper_move_nuisance

let rec ai_buy_sell_helper_smart st =
  let cur_num = st.current_player in
  let player_obj = List.assoc cur_num st.players in
  (* not enough money to buy, must sell *)
  if (player_obj.money < 500) then
    (* let diff = 500 - player_obj.money in *)
    let list_properties = player_obj.properties in
    let prop_list = st.properties in
    let possible_houses = List.filter
        (fun x -> (x.powner = curr_num) && (x.phouses > 0)) prop_list in
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
    let possible_buys = List.filter (fun x -> (can_buy_house st x.pname)&&(x.phouses<=2)) prop_list in
    if (possible_buys <> []) then
      let p_head = List.hd possible_buys in
      let ns = buy_house st p_head.pname in
      ai_buy_sell_helper_smart ns
    else
      ai_helper_move_smart st
  else
    ai_helper_move_smart st *)

let ai_helper_move_nuisance state =
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(dice_roll |> List.rev |> List.hd |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in
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
      do' (BuyProperty p.name) move_state
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

let ai_helper_move_twocolor state =
  let dice_roll = roll_die 2 in
  let is_double = check_double dice_roll in
  let num_steps = List.fold_left (+) 0 dice_roll in
  let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                         " and a " ^(dice_roll |> List.rev |> List.hd |> string_of_int) ^
                         ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in
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
    else if (twocolor_helper move_state init_p.pname) then
      do' (BuyProperty p.name) move_state
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

let ai_helper_move_smart state =
  let dice_roll = roll_die 2 in
   let is_double = check_double dice_roll in
   let num_steps = List.fold_left (+) 0 dice_roll in
   let _ = print_endline ("You've rolled a " ^ (dice_roll |> List.hd |> string_of_int) ^
                          " and a " ^(dice_roll |> List.rev |> List.hd |> string_of_int) ^
                          ". You will move " ^ (string_of_int num_steps) ^ " spaces.\n") in
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
       do' (BuyProperty p.name) move_state
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
