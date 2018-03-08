open Command

(*********************************************************)

(* [strategy] is what kind of gameplay strategy the player uses *)
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
   players, which is their number in the order and name list, the current player,
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
  cc_cards: (string * command) list;
  chance_cards: (string * command) list;
}

let game_board = [
  (0, Enroll);
  (1, Property ({
       pname = "Law Library";
       powned = false;
       powner = 0;
       pcolor = "purple";
       phouses = 0;
       pcurrent_rent = 2;
       prent_list = [2; 10; 30; 90; 160; 250];
       pprice = 60;
       pbuild_cost = 50 ;
     }));
  (2, CommunityChest);
  (3, Property ({
       pname = "Myron Taylor Hall";
       powned = false;
       powner = 0;
       pcolor = "purple";
       phouses = 0;
       pcurrent_rent = 4;
       prent_list = [4; 20; 60; 180; 320; 450];
       pprice = 60;
       pbuild_cost = 50;
     }));
  (4, Tax("Tuition", 200));
  (5, Transport ({
       tname = "TCAT Bus";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (6, Property ({
       pname = "Catherwood Library";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 6;
       prent_list = [6; 30; 90; 270; 400; 550];
       pprice = 100;
       pbuild_cost = 50;
     }));
  (7, Chance);
  (8, Property ({
       pname = "Ives Hall";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 6;
       prent_list = [6; 20; 90; 270; 400; 550];
       pprice = 100;
       pbuild_cost = 50;
     }));
  (9, Property ({
       pname = "ILR Building";
       powned = false;
       powner = 0;
       pcolor = "light blue";
       phouses = 0;
       pcurrent_rent = 8;
       prent_list = [9; 40; 100; 300; 450; 600];
       pprice = 120;
       pbuild_cost = 50;
     }));
  (10, Visiting); (* Visiting JA's Office *)
  (11, Property ({
       pname = "Martha Van Rensselaer Hall";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 10;
       prent_list = [10; 50; 150; 450; 625; 750];
       pprice = 140;
       pbuild_cost = 100;
     }));
  (12, Utility ({
       uname = "Eduroam Wifi Network";
       uowned = false;
       uowner = 0;
       ucurrent_rent = 25;
       uprice = 150;
     }));
  (13, Property ({
       pname = "Mann Library";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 10;
       prent_list = [10; 50; 150; 450; 625; 750];
       pprice = 140;
       pbuild_cost = 100;
     }));
  (14, Property ({
       pname = "Human Ecology Building";
       powned = false;
       powner = 0;
       pcolor = "pink";
       phouses = 0;
       pcurrent_rent = 12;
       prent_list = [12; 60; 180; 500; 700; 900];
       pprice = 160;
       pbuild_cost = 100;
     }));
  (15, Transport ({
       tname = "Coach Shortline Bus";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (16, Property ({
       pname = "Dairy Bar";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 14;
       prent_list = [14; 70; 200; 500; 750];
       pprice = 180;
       pbuild_cost = 100;
     }));
  (17, CommunityChest);
  (18, Property ({
       pname = "Plant Sciences Building";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 14;
       prent_list = [14; 70; 200; 550; 750];
       pprice = 180;
       pbuild_cost = 100;
     }));
  (19, Property ({
       pname = "Kennedy Hall";
       powned = false;
       powner = 0;
       pcolor = "orange";
       phouses = 0;
       pcurrent_rent = 16;
       prent_list = [16; 80; 220; 600; 800; 1000];
       pprice = 200;
       pbuild_cost = 100;
     }));
  (20, FreeParking); (* Free Parking *)
  (21, Property ({
       pname = "Green Dragon";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 18;
       prent_list = [18; 90; 250; 700; 875; 1050];
       pprice = 220;
       pbuild_cost = 150;
     }));
  (22, Chance);
  (23, Property ({
       pname = "Milstein Hall";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 18;
       prent_list = [18; 90; 250; 700; 875; 1050];
       pprice = 220;
       pbuild_cost = 150;
     }));
  (24, Property ({
       pname = "Lincoln Hall";
       powned = false;
       powner = 0;
       pcolor = "red";
       phouses = 0;
       pcurrent_rent = 20;
       prent_list = [20; 100; 300; 750; 925; 1100];
       pprice = 240;
       pbuild_cost = 150;
     }));
  (25, Transport ({
       tname = "Big Red Shuttle";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (26, Property ({
       pname = "Klarman Hall";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 22;
       prent_list = [22; 110; 330; 800; 975; 1150];
       pprice = 260;
       pbuild_cost = 150;
     }));
  (27, Property ({
       pname = "Physical Sciences Building";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 22;
       prent_list = [22; 110; 330; 800; 975; 1150];
       pprice = 260;
       pbuild_cost = 150;
     }));
  (28, Utility ({
       uname = "Red Rover Wifi Network";
       uowned = false;
       uowner = 0;
       ucurrent_rent = 25;
       uprice = 150;
     }));
  (29, Property ({
       pname = "Rockefeller Hall";
       powned = false;
       powner = 0;
       pcolor = "yellow";
       phouses = 0;
       pcurrent_rent = 24;
       prent_list = [24; 120; 360; 850; 1025; 1200];
       pprice = 280;
       pbuild_cost = 150;
     }));
  (30, GoToJA);
  (31, Property ({
       pname = "Duffield Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 26;
       prent_list = [26; 130; 390; 900; 1100; 1275];
       pprice = 300;
       pbuild_cost = 200;
     }));
  (32, Property ({
       pname = "Carpenter Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 26;
       prent_list = [26; 130; 390; 900; 1100; 1275];
       pprice = 300;
       pbuild_cost = 200;
     }));
  (33, CommunityChest);
  (34, Property ({
       pname = "Gates Hall";
       powned = false;
       powner = 0;
       pcolor = "green";
       phouses = 0;
       pcurrent_rent = 28;
       prent_list = [28; 150; 450; 1000; 1200; 1400];
       pprice = 320;
       pbuild_cost = 200;
     }));
  (35, Transport ({
       tname = "Lyft";
       towned = false;
       towner = 0;
       tcurrent_rent = 25;
       tprice = 200;
     }));
  (36, Chance);
  (37, Property ({
       pname = "Sage Hall";
       powned = false;
       powner = 0;
       pcolor = "dark blue";
       phouses = 0;
       pcurrent_rent = 35;
       prent_list = [35; 175; 500; 1100; 1300; 1500];
       pprice = 350;
       pbuild_cost = 200;
     }));
  (38, Tax("Room and Board", 200));
  (39, Property({
       pname = "Statler Hall";
       powned = false;
       powner = 0;
       pcolor = "dark blue";
       phouses = 0;
       pcurrent_rent = 50;
       prent_list = [50; 200; 600; 1400; 1700];
       pprice = 400;
       pbuild_cost = 200;
     }));
  (40, JAsOffice);
]

(* Community chest cards as a (string * command) list *)
let cc_lst = [
  ("Advance to Enroll (Collect $200)", MoveTo 0);
  ("Bursar error in your favor - Collect $200", UpdateMoney 200);
  ("Cornell Health fees - Pay $50", UpdateMoney (-50));
  ("From sale of cryptocurrency you get $50", UpdateMoney 50);
  ("Get Out of JA's Office Free - This card will be kept until needed", AddCard true);
  ("Go directly to JA's Office - Do not pass Enroll - Do not collect $200", SendToJA);
  ("A Capella Concert - Collect $50 from every player for opening night seats", CollectFromAll 50);
  ("Grandma sends you holiday money - Receive $100", UpdateMoney 100);
  ("Tuition refund - Collect $20", UpdateMoney 20);
  ("It is your birthday - Collect $10 from each player", CollectFromAll 10);
  ("Bitcoin soars - Collect $100", UpdateMoney 100);
  ("Pay Cornell Health fees of $100", UpdateMoney (-100));
  ("Pay $150 for 'required' textbook you couldn't find online", UpdateMoney (-150));
  ("You find  $25 walking in Collegetown", UpdateMoney 25);
  ("Ithaca assesses you for street repairs - $40 per house - $115 per hotel", ForEachBuilding (40, 115));
  ("You have won second prize in an essay contest - Collect $10", UpdateMoney 10);
  ("You inherit $100", UpdateMoney 100);
]

(* Chance chest cards as a (string * command) list *)
let chance_lst = [
  ("Advance to Enroll (Collect $200)", MoveTo 0);
  ("Advance to Lincoln Hall - If you pass Enroll, collect $200", MoveTo 24);
  ("Advance to Martha Van Rensselaer Hall - If you pass Enroll, collect $200", MoveTo 11);
  ("Advance to Eduroam Wifi Network. If unowned, you may buy it from the Bank. If owned, pay rent", MoveTo 12);
  ("Advance to Red Rover Wifi Network. If unowned, you may buy it from the Bank. If owned, pay rent", MoveTo 28);
  ("Advance to Lyft. If unowned, you may buy it from the Bank. If owned, pay the owner rent", MoveTo 35);
  ("Bursar makes a mistake and pays you $50", UpdateMoney 50);
  ("Get Out of JA's Office Free - This card will be kept until needed", AddCard false);
  ("Go Back 3 Spaces", MoveForward (-3));
  ("Go directly to JA's Office - Do not pass Enroll - Do not collect $200", SendToJA);
  ("Make general repairs on all your properties - For each house pay $25 - For each hotel pay $100", ForEachBuilding (25, 100));
  ("Order takeout - Pay $15", UpdateMoney (-15));
  ("Take a trip on the TCAT Bus - If you pass Enroll, collect $200", MoveTo 5);
  ("Have a luxurious stay at the Statler Hotel - Advance to the Statler Hotel", MoveTo 39);
  ("You have been elected Chairman of the Board - Pay each player $50", CollectFromAll (-50));
  ("You were awarded a scholarship - Collect $150", UpdateMoney 150);
  ("You won a crossword competition - Collect $100", UpdateMoney 100);
]

(* [instantiate_player] takes in int for id, a string for name, and string
   for token, and the rest of fields in the record begin at zero *)

(* let instantiate_player p_id p_name p_token = { *)
  (* We took out p_name and p_token because we are not sure about its relevance *)

let instantiate_player p_num p_id p_strategy p_token =
  {
    num = p_num;
    id = p_id;
    strategy = p_strategy;
    token = p_token;
    money = 1500;
    properties = [];
    cards = 0;
    location = 0;
    transports = 0;
    utilities = 0;
  }

(* [roll_die] takes in how many dice it rolls and outputs the list of results *)
let roll_die num =
  Random.self_init ();
  let rec roll_helper num acc =
    if num = 0 then acc
    else
      roll_helper (num-1) ((1 + Random.int 5)::acc) in
  roll_helper num []


(* [is_bought] takes in a property, and returns whether or not it has been
   purchased by another user *)
let is_bought p = failwith "Unimplimented"

(* [turns s] is the number of turns the players have taken so far.
 * not used currently, but might be used in the future *)
(* val turns : state -> int *)

(* [shuffle_card] takes in a deck of cards are returned it in a new order *)
let shuffle_card c_list =
  Random.self_init ();
  let mapped = List.map (fun n -> (Random.bits(), n)) c_list in
  let sorted = List.sort compare mapped in
  List.map snd sorted

(* Helpers for inv and inv_all*)
let rec prop_string cur prop_lst acc =
  match prop_lst with
  | [] -> acc
  | h::t -> let new_acc = acc ^ "  - " ^ h.pname ^ "\n" ^
                      "    + Color: " ^ h.pcolor ^ "\n" ^
            "    + Houses: " ^ (string_of_int h.phouses) ^ "\n" ^
            "    + Current rent: $" ^ (string_of_int h.pcurrent_rent) ^ "\n" in
            prop_string cur t new_acc

let rec tport_string cur tport_lst acc =
  match tport_lst with
  | [] -> acc
  | h::t -> let new_acc = acc ^ "  - " ^ h.tname ^ "\n" ^
            "    + Current rent: $" ^ (string_of_int h.tcurrent_rent) ^ "\n" in
            tport_string cur t new_acc

let rec util_string cur u_lst acc =
  match u_lst with
  | [] -> acc
  | h::t -> let new_acc = acc ^ "  - " ^ h.uname ^ "\n" ^
            "    + Current rent: $" ^ (string_of_int h.ucurrent_rent) ^ "\n" in
          util_string cur t new_acc


(* [inv s] is thestring representation of the player's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant.
 * Items that appear in inventory are properties, transports, utilities, special
 * cards such as 'Get out of the JA's office free', and money *)
let inv st =
  let cur_player_num = st.current_player in
  let player_list = st.players in
  let cur_player = List.assoc cur_player_num player_list in
  let props = List.filter (fun x -> x.powner = cur_player_num) st.properties in
  let tports = List.filter (fun x -> x.towner = cur_player_num) st.transports in
  let utils = List.filter (fun x -> x.uowner = cur_player_num) st.utilities in
  "The inventory of the current player (" ^ cur_player.id ^ ") is:\n" ^
  "Money: $" ^ (string_of_int cur_player.money) ^ "\n" ^
  "Get Out of JA's Office Free Cards: " ^(string_of_int cur_player.cards)^"\n"^
  "Properties: " ^ (if cur_player.properties = [] then "0"
                    else "\n" ^ prop_string cur_player_num props "") ^ "\n" ^
  "Transports: " ^ (if cur_player.transports = 0 then "0"
                    else "\n" ^ tport_string cur_player_num tports "") ^ "\n" ^
  "Utilities: " ^ (if cur_player.utilities = 0 then "0"
                   else "\n" ^ util_string cur_player_num utils "") ^ "\n"

let inv_all s =
  let rec inv_all_helper st int_list acc =
    match int_list with
    | [] -> acc
    | h::t ->
    let player_list = st.players in
    let cur_player = List.assoc h player_list in
    let props = List.filter (fun x -> x.powner = h) st.properties in
    let tports = List.filter (fun x -> x.towner = h) st.transports in
    let utils = List.filter (fun x -> x.uowner = h) st.utilities in
    let new_acc = acc ^ "The inventory of player " ^ (string_of_int h) ^
                  " (" ^ (cur_player.id) ^ ") is:\n" ^
    "Money: $" ^ (string_of_int cur_player.money) ^ "\n" ^
    "Get Out of JA's Office Free Cards: " ^(string_of_int cur_player.cards)^"\n"^
    "Properties: " ^ (if cur_player.properties = [] then "0"
                      else "\n" ^ prop_string h props "") ^ "\n" ^
    "Transports: " ^ (if cur_player.transports = 0 then "0"
                      else "\n" ^ tport_string h tports "") ^ "\n" ^
    "Utilities: " ^ (if cur_player.utilities = 0 then "0\n"
                     else "\n" ^ util_string h utils "") ^ "\n" in
    inv_all_helper st t new_acc
  in
  inv_all_helper s [1;2;3;4] ""

  (* [print_rankings s] is the string representation of the ranking of all
   * players by liquid assests.*)
let print_rankings st =
  let rec sum_props prop_lst =
    match prop_lst with
    | [] -> 0
    | h::t -> (h.pprice + h.phouses*h.pbuild_cost) + sum_props t
  in
  let rec sum_tports tport_lst =
    match tport_lst with
    | [] -> 0
    | h::t -> h.tprice + sum_tports t
  in
  let rec sum_utils util_lst =
    match util_lst with
    | [] -> 0
    | h::t -> h.uprice + sum_utils t
  in
  let get_val s player_num =
    let props = List.filter (fun x -> x.powner = player_num) s.properties in
    let tports = List.filter (fun x -> x.towner = player_num) s.transports in
    let utils = List.filter (fun x -> x.uowner = player_num) s.utilities in
    (sum_props props) + (sum_tports tports) + (sum_utils utils) + (List.assoc player_num s.players).money
  in
  let sorted_list = List.sort (fun x y -> if (fst x = fst y) then 0 else if (fst x < fst y) then 1 else -1)
      [(get_val st 1, 1);(get_val st 2, 2);(get_val st 3, 3);(get_val st 4, 4)] in
  let first = List.nth sorted_list 0 in
  let second = List.nth sorted_list 1 in
  let third = List.nth sorted_list 2 in
  let fourth = List.nth sorted_list 3 in
  let players = st.players in
  "Final Game Rankings! (by liquid assets)\n" ^
  "  First place: Player " ^ (string_of_int (snd first)) ^ " - " ^
  (List.assoc (snd first) players).id ^ " with $" ^ (string_of_int (fst first)) ^ "\n"  ^
  "  Second place: Player " ^ (string_of_int (snd second)) ^ " - " ^
  (List.assoc (snd second) players).id ^  " with $" ^ (string_of_int (fst second)) ^ "\n"  ^
  "  Third place: Player " ^ (string_of_int (snd third)) ^ " - " ^
  (List.assoc (snd third) players).id ^  " with $" ^ (string_of_int (fst third)) ^ "\n"  ^
  "  Fourth place: Player " ^ (string_of_int (snd fourth)) ^ " - " ^
  (List.assoc (snd fourth) players).id ^  " with $" ^ (string_of_int (fst fourth)) ^ "\n"

(* [init_state lst] is the initial state of the game given
 * that lst are the names of the players in order of play.
 * requires: [lst] contains 2 to 4 distinct alpha-numeric strings. *)
let init_state player_list =
  let die_rolls = roll_die 4 in
  let sorted_rolls = List.sort compare die_rolls in
  let rec matcher_fn die_list p_list acc =
    match die_list, p_list with
    | [], [] -> acc
    | h1::t1, h2::t2 -> matcher_fn t1 t2 ((h1,h2)::acc)
    | _, _ -> failwith "ERROR"
  in
  let matched = matcher_fn die_rolls player_list [] in
  let rec orderer o_list p_list acc =
    match p_list, o_list with
    | [], [] -> acc
    | h1::t1, h2::t2 ->
      begin
        (* let i = List.length acc in *)
        let o_player = List.assoc h2 p_list in
        let new_player = {o_player with num = ((List.length acc)+1)} in
        orderer t2 (List.remove_assoc h2 p_list) (((List.length acc)+1, new_player) :: acc)
      end
    | _, _ -> failwith "ERROR"
  in
  let final_player_list = orderer sorted_rolls matched [] in
  let rec get_properties spaces acc =
    match spaces with
    | [] -> acc
    | (_, h)::t -> match h with
      | Property p -> get_properties t (p::acc)
      | _ -> get_properties t acc
  in
  let rec get_utilities spaces acc =
    match spaces with
    | [] -> acc
    | (_, h)::t -> match h with
      | Utility u -> get_utilities t (u::acc)
      | _ -> get_utilities t acc
  in
  let rec get_transports spaces acc =
    match spaces with
    | [] -> acc
    | (_, h)::t -> match h with
      | Transport p -> get_transports t (p::acc)
      | _ -> get_transports t acc
  in
  {
    players = final_player_list;
    current_player = 1;
    double_counter = 0;
    round = 0;
    properties = get_properties game_board [];
    utilities = get_utilities game_board [];
    transports = get_transports game_board [];
    board = game_board;
    cc_cards = shuffle_card cc_lst;
    chance_cards = shuffle_card chance_lst;
  }

let in_jail st =
  let p = List.assoc (st.current_player) st.players in
  p.location = 40

(* [locations s] is a string representation of the players' locations *)
let locations st =
  let rec loc_helper player_list board acc =
    match player_list with
    | [] -> acc
    | (hint, hobj)::t -> let loc_string = begin
        match List.assoc hobj.location st.board with
        | Enroll -> "Enroll"
        | Property p -> p.pname
        | Transport t -> t.tname
        | Utility u -> u.uname
        | FreeParking -> "Free Parking"
        | Chance -> "Chance"
        | CommunityChest -> "Community Chest"
        | Visiting -> "Visiting the JA's Office"
        | GoToJA -> "Go to JA's Office"
        | JAsOffice -> "JA's Office"
        | Tax (name, _) -> name
    end in
      let new_acc = acc ^ "Player " ^ (string_of_int hint) ^ " : " ^ hobj.id ^
                    " is at location " ^ (string_of_int hobj.location) ^ " - " ^ loc_string ^ ".\n" in
      loc_helper t board new_acc
  in
  loc_helper st.players st.board ""

(* [update_money] changes cur_player's money by num (positive if giving,
   negative if taking) and returns the updated player list.
   val update_money : player -> player list -> int -> player_list *)
let update_money st num =
  let rec money_helper cur p_lst mon acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur
      then money_helper cur t mon ((hint, hp)::acc)
      else let update = {hp with money = hp.money + mon} in
        money_helper cur t mon ((hint, update)::acc)
  in
  {st with players = (money_helper st.current_player st.players num [])}

(* [send_to_ja] updates a player's location to the JA's office and returns
   updated list of players.
   val send_to_ja : player -> player list -> player list *)
let send_to_ja st =
  let rec ja_helper cur p_lst acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur then ja_helper cur t ((hint, hp)::acc)
      else let update = {hp with location = 40} in
        ja_helper cur t ((hint, update)::acc)
  in
  {st with double_counter = 0; players = (ja_helper st.current_player st.players [])}


(* [can_buy_house st prop_str] checks whether a player can buy a house on the
   property they've just landed on . It returns a tuple of three booleans.
   the first is whether the player owns all properties of the given color,
    because players can't buy houses otherwise.
   The second is whether the player has enough money to buy a house on the property.
   the third is whether the player can build a house without exceeding the 5-house limit.
   val can_buy_house : state -> string -> (bool) *)
let can_buy_house st prop_str =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  if match_list = [] then false
  else
    let prop = List.hd match_list in
    let color = prop.pcolor in
    let color_list = List.filter (fun x -> x.pcolor = color) p_list in
      let owns = List.fold_left (fun x y ->
        x||(y.powner=st.current_player)
      ) false color_list in
    let player_int = st.current_player in
    let player_obj = List.assoc player_int (st.players) in
    let enough_money = prop.pbuild_cost <= player_obj.money
    in (owns&&enough_money&&(prop.phouses<=4))

(* [can_buy_house st prop_str] checks whether a player can buy a house on the
   property they've just landed on . It returns a tuple of three booleans.
   the first is whether the player owns all properties of the given color,
    because players can't buy houses otherwise.
   The second is whether the player has enough money to buy a house on the property.
   the third is whether the player can build a house without exceeding the 5-house limit.
   val can_buy_house : state -> string -> (bool) *)
let can_buy_n_houses st prop_str n =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  let prop = List.hd match_list in
  let color = prop.pcolor in
  let color_list = List.filter (fun x -> x.pcolor = color) p_list in
  let owns = List.fold_left (fun x y -> x&&(y.powner=st.current_player)) true color_list in
  let player_int = st.current_player in
  let player_obj = List.assoc player_int (st.players) in
  let enough_money = n*prop.pbuild_cost <= player_obj.money in
  (owns&&enough_money&&prop.phouses+n<=5)


(* [buy_house st prop_str] buys a house at the location of the current player,
    provided that the current player already owns all the properties of the
   current location's color, and building another house would not exceed the
   limit of 5 houses per property.
   val buy_house : state -> string -> state *)
let buy_house st prop_str =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  let prop = List.hd match_list in
  let new_prop = {prop with phouses = prop.phouses+1; pcurrent_rent = List.nth prop.prent_list prop.phouses+1} in
  let player_int = st.current_player in
  let player_obj = List.assoc player_int (st.players) in
  let new_player = {player_obj with money = player_obj.money-prop.pbuild_cost} in
  let new_prop_list = new_prop::(List.filter (fun x -> (String.lowercase_ascii x.pname) <> (String.lowercase_ascii prop_str)) p_list) in
  let new_player_list = (player_int, new_player)::(List.remove_assoc player_int st.players)in
  {st with players = new_player_list; properties = new_prop_list}


(* [can_sell_house st prop_str] checks whether a player can sell a house on the
   property they've just landed on. It returns a boolean.
   val can_sell_house : state -> string -> bool *)
let can_sell_house st prop_str =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  if match_list = [] then false else
  let prop = List.hd match_list in
  let owns = prop.powner=st.current_player in
  owns&&prop.phouses>=1


let has_houses st =
  List.fold_left (fun acc p -> acc || p.powner=st.current_player) false st.properties

(* [sell_house st prop_str] sells a house at the location of the current player,
    provided that the current player already owns houses there.
   val sell_house : state -> string -> state *)
let sell_house st prop_str =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  let prop = List.hd match_list in
  let new_prop = {prop with phouses = prop.phouses-1; pcurrent_rent = List.nth prop.prent_list prop.phouses-1} in
  let player_int = st.current_player in
  let player_obj = List.assoc player_int (st.players) in
  let new_player = {player_obj with money = player_obj.money+(prop.pbuild_cost/2)} in
  let new_prop_list = new_prop::(List.filter (fun x -> x.pname <> prop_str) p_list) in
  let new_player_list = (player_int, new_player)::(List.remove_assoc player_int st.players)in
  {st with players = new_player_list; properties = new_prop_list}


(* [can_buy_prop st prop_str] checks whether a player can buy the
   property they've just landed on . It returns a tuple of two booleans.
   the first is whether the property is not yet sold.
   The second is whether the player has enough money to buy a house on the property.

   val can_buy_prop : state -> string -> (bool) *)
let can_buy_prop st prop_str =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop_str)) p_list in
  if match_list = [] then false else
  let prop = List.hd match_list in
  let price = prop.pprice in
  let player_int = st.current_player in
  let player_obj = List.assoc player_int (st.players) in
  (not prop.powned&&price <= player_obj.money)

(* [buy_property st prop_str] buys the property at the location of the current player,
    provided that no other player already owns that property.
   val buy_property : state -> string -> state *)
let buy_property st prop =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop)) p_list in
  let prop_to_buy = List.hd match_list in
  let player_int = st.current_player in
  let player_obj = List.assoc player_int (st.players) in
  let new_property = {prop_to_buy with powned = true; powner = player_int; pcurrent_rent = List.hd prop_to_buy.prent_list} in
  let new_prop_list = new_property::(List.filter (fun x -> (String.lowercase_ascii x.pname) <> (String.lowercase_ascii prop)) p_list) in
  let new_player = {player_obj with properties = prop::player_obj.properties; money = player_obj.money - prop_to_buy.pprice } in
  let new_player_list = (player_int, new_player)::(List.remove_assoc player_int st.players)in
  {st with players = new_player_list; properties = new_prop_list}

(* [sell_property st prop_str] sells the property at the location of the current player,
    provided that the current player already that property.
   val sell_property : state -> string -> state *)
let sell_property st prop =
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = (String.lowercase_ascii prop)) p_list in

  let prop_to_sell = List.hd match_list in
    let player_int = st.current_player in
    let player_obj = List.assoc player_int (st.players) in
    let new_property = {prop_to_sell with powned = false; powner = 0; pcurrent_rent = (List.hd prop_to_sell.prent_list)} in
  let new_prop_list = new_property::(List.filter (fun x -> (String.lowercase_ascii x.pname) <> (String.lowercase_ascii prop)) p_list) in
    let new_player =
      {player_obj with properties = (List.filter (fun x -> (String.lowercase_ascii x) <> (String.lowercase_ascii prop)) player_obj.properties);
                     money = player_obj.money + (prop_to_sell.pprice/2) + (prop_to_sell.phouses * (prop_to_sell.pbuild_cost/2))} in
    let new_player_list = (player_int, new_player)::(List.remove_assoc player_int st.players) in
      {st with players = new_player_list; properties = new_prop_list}

let buy_transport st tport =
  let cur = st.current_player in
  let tport_lst = st.transports in
  let match_lst = List.filter (fun x -> (String.lowercase_ascii x.tname) = (String.lowercase_ascii tport)) tport_lst in
  let tport_to_buy = List.hd match_lst in
  let player_obj = List.assoc cur (st.players) in
  let new_tport = {tport_to_buy with towned = true; towner = cur; tcurrent_rent = 25*(player_obj.transports + 1)} in
  let new_tport_lst = new_tport::(List.filter (fun x -> (String.lowercase_ascii x.tname) <> (String.lowercase_ascii tport)) tport_lst) in
  let new_player = {player_obj with transports = player_obj.transports + 1; money = player_obj.money - tport_to_buy.tprice} in
  let new_player_lst = (cur, new_player)::(List.remove_assoc cur st.players) in
  {st with players = new_player_lst; transports = new_tport_lst}

let sell_transport st tport =
  let cur = st.current_player in
  let tport_lst = st.transports in
  let match_lst = List.filter (fun x -> (String.lowercase_ascii x.tname) = (String.lowercase_ascii tport)) tport_lst in
  let tport_to_sell = List.hd match_lst in
  let player_obj = List.assoc cur (st.players) in
  let new_tport = {tport_to_sell with towned = false; towner = 0; tcurrent_rent = 25} in
  let new_tport_lst = new_tport::(List.filter (fun x -> (String.lowercase_ascii x.tname) <> (String.lowercase_ascii tport)) tport_lst) in
  let new_player = {player_obj with transports = player_obj.transports - 1; money = player_obj.money + (tport_to_sell.tprice/2)} in
  let new_player_lst = (cur, new_player)::(List.remove_assoc cur st.players) in
  {st with players = new_player_lst; transports = new_tport_lst}

let buy_utility st util =
  let cur = st.current_player in
  let util_lst = st.utilities in
  let match_lst = List.filter (fun x -> (String.lowercase_ascii x.uname) = (String.lowercase_ascii util)) util_lst in
  let util_to_buy = List.hd match_lst in
  let player_obj = List.assoc cur (st.players) in
  let new_util = {util_to_buy with uowned = true; uowner = cur; ucurrent_rent = 25*(player_obj.utilities + 1)} in
  let new_util_lst = new_util::(List.filter (fun x -> (String.lowercase_ascii x.uname) <> (String.lowercase_ascii util)) util_lst) in
  let new_player = {player_obj with utilities = player_obj.utilities + 1; money = player_obj.money - util_to_buy.uprice} in
  let new_player_lst = (cur, new_player)::(List.remove_assoc cur st.players) in
  {st with players = new_player_lst; utilities = new_util_lst}

let sell_utility st util =
  let cur = st.current_player in
  let util_lst = st.utilities in
  let match_lst = List.filter (fun x -> (String.lowercase_ascii x.uname) = (String.lowercase_ascii util)) util_lst in
  let util_to_sell = List.hd match_lst in
  let player_obj = List.assoc cur (st.players) in
  let new_util = {util_to_sell with uowned = false; uowner = 0; ucurrent_rent = 25} in
  let new_util_lst = new_util::(List.filter (fun x ->(String.lowercase_ascii x.uname) <> (String.lowercase_ascii util)) util_lst) in
  let new_player = {player_obj with utilities = player_obj.utilities - 1; money = player_obj.money + (util_to_sell.uprice/2)} in
  let new_player_lst = (cur, new_player)::(List.remove_assoc cur st.players) in
  {st with players = new_player_lst; utilities = new_util_lst}

(* [ nuisance_helper st prop] returns a boolean indicating whether the nuisance
   AI player should buy the property they just landed on
   val nuisance_helper : state -> string -> bool *)
let nuisance_helper st prop =
  if can_buy_prop st prop then
    begin
      let p_list = st.properties in
      let match_list = List.filter (fun x -> x.pname = prop) p_list in
      let prop_to_buy = List.hd match_list in
      let color = prop_to_buy.pcolor in
      let color_list = List.filter (fun x -> x.pcolor = color) p_list in
      let owned = List.fold_left (fun x y -> x||(y.powner=0)) false color_list in
      owned
    end
  else false

  let smart_helper st prop =
    let pl = List.assoc st.current_player st.players in
    let myp_list = (pl).properties in
    if List.length myp_list =0 then (can_buy_prop st prop)
    else if (List.length myp_list =1) then
    let p_list = st.properties in
    let owned_list = List.filter (fun x -> (x.powner = pl.num)) p_list in
    let color_list = "orange"::"light blue"::"pink"::List.map (fun x -> x.pcolor) owned_list in
    let match_list = List.filter (fun x -> x.pname = prop) p_list in
    let prop_to_buy = List.hd match_list in
    List.mem prop_to_buy.pcolor color_list
    else
      false

  let twocolor_helper st prop =
    let pl = List.assoc st.current_player st.players in
    let myp_list = pl.properties in
    if (List.length myp_list <=1)
    then (can_buy_prop st prop)
    else
      let p_list = st.properties in
      let owned_list = List.filter (fun x -> (x.powner = pl.num)) p_list in
      let color_list = List.map (fun x -> x.pcolor) owned_list in
      let match_list = List.filter (fun x -> x.pname = prop) p_list in
      let prop_to_buy = List.hd match_list in
      List.mem prop_to_buy.pcolor color_list

(* [can_sell_utility st util] returns a boolean indicating whether the current player
     can sell the utility [util] *)
let can_sell_utility (st : state) util =
    (* let _ = print_endline util in *)
    let u_list = st.utilities in
    let match_list = List.filter (fun x -> (String.lowercase_ascii x.uname) = util) u_list in
    let rec check lst =
      match match_list with
      | [] -> false
      | h::t -> if h.uowner=st.current_player then true else check t
    in check match_list

(* [can_sell_transport st trans_str] returns a boolean indicating whether the current player
   can sell the transport [trans_str] *)
let can_sell_transport (st : state) trans_str =
  (* let _ = print_endline trans_str in *)
  let t_list = st.transports in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.tname) = trans_str) t_list in
  let rec check lst =
    match match_list with
    | [] -> false
    | h::t -> if h.towner=st.current_player then true else check t
  in check match_list

(* [ can_sell_property st prop_str] returns a boolean indicating whether the current player
   can buy the property they just landed on
   val can_sell_property : state -> string -> bool *)
let can_sell_property (st : state) prop_str =
  (* let _ = print_endline prop_str in *)
  let p_list = st.properties in
  let match_list = List.filter (fun x -> (String.lowercase_ascii x.pname) = prop_str) p_list in
  (* if match_list=[] then false
  else let prop = List.hd match_list in
     prop.powner=st.current_player *)
  let rec check lst =
    match match_list with
    | [] -> false
    | h::t -> if h.powner=st.current_player then true else check t
  in check match_list

let use_card st =
  let rec use_helper cur p_lst acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur then use_helper cur t ((hint, hp)::acc)
      else let update = {hp with location = 10; cards = (hp.cards - 1)} in
        use_helper cur t ((hint, update)::acc)
  in
  if (List.length st.cc_cards < List.length st.chance_cards) then
    let get_out_card =
      ("Get Out of JA's Office Free - This card will be kept until needed", AddCard true) in
    let new_cc = st.cc_cards@[get_out_card] in
    {st with players = (use_helper st.current_player st.players []);
             cc_cards = new_cc}
  else
    let get_out_card =
      ("Get Out of JA's Office Free - This card will be kept until needed", AddCard false) in
    let new_chance = st.chance_cards@[get_out_card] in
    {st with players = (use_helper st.current_player st.players []);
             chance_cards = new_chance}

let has_enough_money state p_payer min_money =
  let p = List.assoc p_payer (state.players) in
  p.money >= min_money


let add_card st deck =
  let rec add_helper cur p_lst acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur then add_helper cur t ((hint, hp)::acc)
      else let update = {hp with cards = (hp.cards + 1)} in
        add_helper cur t ((hint, update)::acc)
  in
  if deck then
    {st with players = (add_helper st.current_player st.players []);
             cc_cards = (List.tl st.cc_cards)}
  else
    {st with players = (add_helper st.current_player st.players []);
             chance_cards = (List.tl st.chance_cards)}

let move_to st num =
  let rec move_helper cur p_lst loc acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur
      then move_helper cur t loc ((hint, hp)::acc)
      else let pass_go = if loc < hp.location then 200 else 0 in
        let update = {hp with location = loc;
                              money = hp.money + pass_go} in
        move_helper cur t loc ((hint, update)::acc)
  in
  {st with players = (move_helper st.current_player st.players num [])}

let move_forward st num =
  let rec forward_helper cur p_lst loc acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur
      then forward_helper cur t loc ((hint, hp)::acc)
      else let pass_go = if ((loc + hp.location)/40 = 1) then 200 else 0 in
        let update = {hp with location = ((loc + hp.location) mod 40);
                              money = hp.money + pass_go} in
        forward_helper cur t loc ((hint, update)::acc)
  in
  {st with players = (forward_helper st.current_player st.players num [])}

let collect_from_all st num =
  let rec collect_helper cur p_lst n acc =
    match p_lst with
    | [] -> acc
    | (hint, hp)::t -> if hint <> cur
      then let update_other =  {hp with money = (hp.money - n)} in
        collect_helper cur t n ((hint, update_other)::acc)
      else
        let update_cur = {hp with money = (hp.money + (3*n))} in
        collect_helper cur t n ((hint, update_cur)::acc)
  in
  {st with players = (collect_helper st.current_player st.players num [])}

let for_each_building st num1 num2 =
  let rec for_each_helper cur_p prop_list i1 i2 acc =
    match prop_list with
    | [] -> acc
    | h::t -> if h.powner <> cur_p then for_each_helper cur_p t i1 i2 acc
      else if h.phouses = 5 then for_each_helper cur_p t i1 i2 (acc + i2)
      else for_each_helper cur_p t i1 i2 (acc + (h.phouses * i1))
  in
  let cur = st.current_player in
  let pay_amount = for_each_helper cur st.properties num1 num2 0 in
  update_money st (-pay_amount)

let send_to_ja st =
  let cur = st.current_player in
  let cur_player = List.assoc cur st.players in
  let update_player = {cur_player with location = 40} in
  {st with players = (cur, update_player)::(List.remove_assoc cur st.players)}

(* [get_top_card st deck] returns a (string, command) pair; the bool 'deck'
 * refers to Community Chest when true and Chance when false
 * DOESN'T REALLY NEED TO BE A FUNCTION *)
let get_top_card st deck =
  if deck then List.hd st.cc_cards else List.hd st.chance_cards

(* [draw_card st deck] does NOT return a card; takes card from start of card
   list and puts it at end;
   deck is a bool -> true = Community Chest; false = Chance *)
let draw_card st deck =
  if deck then let cards = st.cc_cards in
    {st with cc_cards = (List.tl cards)@[List.hd cards]}
  else let cards = st.chance_cards in
    {st with chance_cards = (List.tl cards)@[List.hd cards]}

let pay_rent st owner rent =
  let decrement_current = update_money st (-rent) in
  let dec_players = decrement_current.players in
  let p_owner = List.assoc owner dec_players in
  let update_owner = {p_owner with money = p_owner.money + rent} in
  {decrement_current with
   players = (owner, update_owner)::(List.remove_assoc owner dec_players)}

let increment_player st =
  let cur = st.current_player in
  if cur = 4 then {st with round = st.round + 1; current_player = 1; double_counter = 0}
  else {st with current_player = cur + 1; double_counter = 0}

(* [do' com st] is [st'] if doing command [com] in state [st] results
 * in a new state [st'].
 * requires: the input state is the predefined starting state given the number
 * of players, or by repeated applications of [do'] to such a state.
*)
let do' com st =
  match com with
  | Quit -> st
  | Help -> st
  | Locations -> st
  | Inventory -> st
  | Turns -> st
  | BuyProperty str -> buy_property st str
  | BuyHouse str -> buy_house st str
  | SellProperty str -> sell_property st str
  | SellHouse str -> sell_house st str
  | BuyTransport str -> buy_transport st str
  | SellTransport str -> sell_transport st str
  | BuyUtility str -> buy_utility st str
  | SellUtility str -> sell_utility st str
  | UseCard -> use_card st
  | UpdateMoney num -> update_money st num
  | PayRent (owner, rent) -> pay_rent st owner rent
  | MoveTo loc -> move_to st loc
  | MoveForward num -> move_forward st num
  | AddCard deck -> add_card st deck
  | CollectFromAll num -> collect_from_all st num
  | ForEachBuilding (num1, num2) -> for_each_building st num1 num2
  | SendToJA -> send_to_ja st
  | IncrementDouble -> {st with double_counter = st.double_counter +1}
  | IncrementRound -> {st with round = st.round + 1}
  | IncrementPlayer -> increment_player st
  | DrawCard deck -> draw_card st deck
  | Invalid -> st

(* [win_state] returns when the game has ended, which is when one player exits
   the game, aka they have no money left or properties to sell, and winners
   are in order of highest to lowers amount of total net worth *)
let win_state st = failwith "Unimplimented"

(*********************************************************)

(* [description s] is the description of the current game state, including
 * player locations, inventory, money, and which player's turn it is*)
let description st = failwith "Unimplimented"
