type color = Red | Green | Yellow | Blue
type special = Reverse | Skip | PlusTwo

type card =
  | Regular of color * int
  | Special of color * special
  | Wild
  | PlacedWild of color

type directions = Clockwise | Counterclockwise

type player = {
  name : string;
  mutable numcards : int;
  mutable cards : card list;
  curr_card_player : int option;
  win : bool option;
}

module type Game = sig
  type 'a t

  val empty : bool -> card list -> 'a t
  val get_player_number : 'a t -> int
  val get_player_num_of_cards : 'a t -> int -> int
  val get_player_cards : 'a t -> int -> card list
  val create_players : 'a t -> int -> bool -> 'a t
  val players_to_string : 'a t -> string
  val card_to_string : card -> string
  val cards_to_string : 'a t -> string
  val card_list_to_string : card list -> int -> string
  val edit_player_cards : 'a t -> int -> card list -> 'a t
  val make_curr_card : 'a t -> 'a t
  val print_curr_card : 'a t -> unit
  val chance_curr_card : 'a t -> card option -> unit
  val add_curr_card_to_cards : 'a t -> card list -> card list
  val add_cards_to_hand : 'a t -> int -> int -> card list
  val check_if_win : player list -> bool
  val get_players : 'a t -> player list
  val get_curr_card : 'a t -> card
  val get_avail_cards : 'a t -> card list
  val change_direction : 'a t -> unit
  val get_direction : 'a t -> directions
  val next_player : 'a t -> 'a t
  val get_curr_player : 'a t -> int
  val get_curr_player_name : 'a t -> string
  val handle_wild : card option -> string -> card option
  val select_card : unit -> int
  val save_player_name : unit -> string
  val save_wild_input : unit -> string
end

let red_cards =
  [
    Regular (Red, 0);
    Regular (Red, 1);
    Regular (Red, 2);
    Regular (Red, 3);
    Regular (Red, 4);
    Regular (Red, 5);
    Regular (Red, 6);
    Regular (Red, 7);
    Regular (Red, 8);
    Regular (Red, 9);
    Special (Red, Reverse);
    Special (Red, Skip);
    Special (Red, PlusTwo);
  ]

let yellow_cards =
  [
    Regular (Yellow, 0);
    Regular (Yellow, 1);
    Regular (Yellow, 2);
    Regular (Yellow, 3);
    Regular (Yellow, 4);
    Regular (Yellow, 5);
    Regular (Yellow, 6);
    Regular (Yellow, 7);
    Regular (Yellow, 8);
    Regular (Yellow, 9);
    Special (Yellow, Reverse);
    Special (Yellow, Skip);
    Special (Yellow, PlusTwo);
  ]

let blue_cards =
  [
    Regular (Blue, 0);
    Regular (Blue, 1);
    Regular (Blue, 2);
    Regular (Blue, 3);
    Regular (Blue, 4);
    Regular (Blue, 5);
    Regular (Blue, 6);
    Regular (Blue, 7);
    Regular (Blue, 8);
    Regular (Blue, 9);
    Special (Blue, Reverse);
    Special (Blue, Skip);
    Special (Blue, PlusTwo);
  ]

let green_cards =
  [
    Regular (Green, 0);
    Regular (Green, 1);
    Regular (Green, 2);
    Regular (Green, 3);
    Regular (Green, 4);
    Regular (Green, 5);
    Regular (Green, 6);
    Regular (Green, 7);
    Regular (Green, 8);
    Regular (Green, 9);
    Special (Green, Reverse);
    Special (Green, Skip);
    Special (Green, PlusTwo);
  ]

let wild_cards = [ Wild ]

(*SHUFFLE CODE IS COPY AND PASTED FROM STACK OVERFLOW
    https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
let shuffle d =
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let all_cards =
  shuffle
    (red_cards @ red_cards @ blue_cards @ blue_cards @ green_cards @ green_cards
   @ yellow_cards @ yellow_cards @ wild_cards)

(* Potentially implement a functor that takes in the game instance to play the game*)

module GameInstance : Game = struct
  type 'a t = {
    players : player list;
    mutable available_cards : card list;
    mutable curr_card : card option;
    mutable curr_player : int;
    mutable direction : directions;
  }

  let empty test cards =
    if not test then
      {
        players = [];
        available_cards = all_cards;
        curr_card = None;
        curr_player = 0;
        direction = Clockwise;
      }
    else
      {
        players = [];
        available_cards = cards;
        curr_card = None;
        curr_player = 0;
        direction = Clockwise;
      }

  let save_user_input (print_func : unit) : string =
    print_func;
    read_line ()

  let get_player_number (game : 'a t) : int = List.length game.players

  let get_player_num_of_cards (game : 'a t) (p_num : int) : int =
    let player = List.nth game.players (p_num - 1) in
    List.length player.cards

  let get_player_cards (game : 'a t) (p_num : int) : card list =
    let player = List.nth game.players p_num in
    player.cards

  let edit_player_cards (game : 'a t) (p_num : int) (card_list : card list) :
      'a t =
    let player = List.nth game.players p_num in
    player.cards <- card_list;
    player.numcards <- List.length card_list;
    game

  let get_players (game : 'a t) : player list = game.players

  let get_curr_card (game : 'a t) : card =
    match game.curr_card with Some v -> v | None -> failwith "No card placed"

  let get_avail_cards (game : 'a t) : card list = game.available_cards

  let get_direction (game : 'a t) : directions =
    match game.direction with
    | Clockwise -> Clockwise
    | Counterclockwise -> Counterclockwise

  let rec check_if_win player_list : bool =
    match player_list with
    | [] -> false
    | h :: t -> if h.numcards = 0 then true else check_if_win t

  let check_if_skip card : bool =
    match card with
    | Special (_, v) -> if v = Skip then true else false
    | _ -> false

  let check_if_reverse card : bool =
    match card with
    | Special (_, v) -> if v = Reverse then true else false
    | _ -> false

  let check_if_plus card : bool =
    match card with
    | Special (_, v) -> if v = PlusTwo then true else false
    | _ -> false

  let rec get_n_cards pool receive n =
    match (n, pool) with
    | 0, _ -> (receive, pool)
    | x, h :: t -> get_n_cards t (receive @ [ h ]) (x - 1)
    | _ -> raise (Invalid_argument "Not enough cards")

  let save_player_name () : string =
    save_user_input
      (print_endline "Enter Player name: ";
       print_string ">")

  let create_player (card_list : card list) (input_name : string) :
      player * card list =
    let play_cards, remain_cards = get_n_cards card_list [] 7 in
    ( {
        name = input_name;
        numcards = 7;
        cards = play_cards;
        curr_card_player = None;
        win = None;
      },
      remain_cards )

  let rec create_players (game : 'a t) (player_num : int) (test : bool) : 'a t =
    match (player_num, test) with
    | 0, _ -> game
    | _, false ->
        let player, cards =
          create_player game.available_cards (save_player_name ())
        in
        create_players
          {
            available_cards = cards;
            players = game.players @ [ player ];
            curr_card = game.curr_card;
            curr_player = game.curr_player;
            direction = game.direction;
          }
          (player_num - 1) false
    | _, true ->
        let player, cards =
          create_player game.available_cards (string_of_int player_num)
        in
        create_players
          {
            available_cards = cards;
            players = game.players @ [ player ];
            curr_card = game.curr_card;
            curr_player = game.curr_player;
            direction = game.direction;
          }
          (player_num - 1) true

  let make_curr_card (game : 'a t) : 'a t =
    let curr_card, remain =
      match game.available_cards with [] -> (None, []) | h :: t -> (Some h, t)
    in
    {
      players = game.players;
      available_cards = remain;
      curr_card;
      curr_player = game.curr_player;
      direction = game.direction;
    }

  let rec get_player_names lst result =
    match lst with
    | [] -> result
    | h :: t -> get_player_names t (h.name :: result)

  let players_to_string (game : 'a t) : string =
    let player_list = game.players in
    String.concat " " (get_player_names player_list [])

  let color_to_string (col : color) : string =
    match col with
    | Red -> "Red"
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Blue -> "Blue"

  let special_to_string (spec : special) : string =
    match spec with
    | Reverse -> "Reverse"
    | Skip -> "Skip"
    | PlusTwo -> "Plus Two"

  let card_to_string (c : card) : string =
    match c with
    | Regular (c, v) -> "(" ^ color_to_string c ^ ", " ^ string_of_int v ^ ")"
    | Special (c, v) ->
        "(" ^ color_to_string c ^ ", " ^ special_to_string v ^ ")"
    | Wild -> "(Wild)"
    | PlacedWild c -> "(" ^ color_to_string c ^ ")"

  let rec card_list_to_string (c : card list) (i : int) : string =
    match c with
    | [] -> ""
    | h :: [] -> string_of_int i ^ ": " ^ card_to_string h
    | h :: t ->
        string_of_int i ^ ": " ^ card_to_string h ^ "; "
        ^ card_list_to_string t (i + 1)

  let rec players_cards_to_list (pl : player list) : string =
    match pl with
    | [] -> ""
    | h :: t ->
        (h.name ^ ": " ^ "[" ^ card_list_to_string h.cards 0)
        ^ "]" ^ "\n" ^ players_cards_to_list t

  let cards_to_string (game : 'a t) : string =
    players_cards_to_list game.players

  let print_curr_card (game : 'a t) : unit =
    match game.curr_card with
    | None -> print_string "\n"
    | Some x -> print_string (card_to_string x ^ "\n")

  let chance_curr_card game card : unit = game.curr_card <- card

  let add_curr_card_to_cards game cards : card list =
    match game.available_cards with
    | [] -> cards
    | h :: t ->
        game.available_cards <- t;
        h :: cards

  let add_cards_to_hand game p_num num : card list =
    let player = List.nth game.players p_num in
    let returnval, cardsleft =
      get_n_cards game.available_cards player.cards num
    in
    game.available_cards <- cardsleft;
    returnval

  let change_direction (game : 'a t) : unit =
    match game.direction with
    | Clockwise -> game.direction <- Counterclockwise
    | Counterclockwise -> game.direction <- Clockwise

  let save_wild_input () : string =
    String.lowercase_ascii
      (save_user_input
         (print_endline "Select a color";
          print_string "> "))

  let rec handle_wild (card : card option) (user_input : string) : card option =
    match card with
    | Some Wild -> (
        match String.lowercase_ascii user_input with
        | "red" -> Some (PlacedWild Red)
        | "yellow" -> Some (PlacedWild Yellow)
        | "blue" -> Some (PlacedWild Blue)
        | "green" -> Some (PlacedWild Green)
        | _ ->
            print_endline "Please select a valid color";
            handle_wild card (save_wild_input ()))
    | Some x -> Some x
    | None -> None

  let select_card () : int =
    int_of_string
      (save_user_input
         (print_endline "Select a card to place down";
          print_string "> "))

  let next_player (game : 'a t) : 'a t =
    (if check_if_reverse (get_curr_card game) then change_direction game;
     if check_if_plus (get_curr_card game) then
       let next_player =
         let increm =
           if get_direction game = Clockwise then 1
           else get_player_number game - 1
         in
         (game.curr_player + increm) mod get_player_number game
       in
       ignore
         (edit_player_cards game next_player
            (add_cards_to_hand game next_player 2)));

    match game.direction with
    | Clockwise ->
        if check_if_skip (get_curr_card game) then
          game.curr_player <- (game.curr_player + 2) mod get_player_number game
        else
          game.curr_player <- (game.curr_player + 1) mod get_player_number game;
        game
    | Counterclockwise ->
        if check_if_skip (get_curr_card game) then
          game.curr_player <-
            (game.curr_player + get_player_number game - 2)
            mod get_player_number game
        else
          game.curr_player <-
            (game.curr_player + get_player_number game - 1)
            mod get_player_number game;
        game

  let get_curr_player (game : 'a t) : int = game.curr_player

  let get_curr_player_name (game : 'a t) : string =
    (List.nth game.players game.curr_player).name
end

module GameInterface = GameInstance

(* let create_game players =
   GameInterface.players_to_string
     (GameInterface.create_players GameInterface.empty (int_of_string players)) *)
(* let rec card_selected cards input =
   match (cards, input) with
   | h :: t, 0 -> (h, t)
   | _ :: t, x -> card_selected t (x - 1)
   | [], _ -> raise (Invalid_argument "Invalid card") *)

(* Given a game, prints out the current player's name and their cards *)
let display_player_cards game =
  print_endline "";
  print_endline (GameInterface.get_curr_player_name game ^ "'s turn");
  print_endline "";
  print_endline
    (GameInterface.card_list_to_string
       (GameInterface.get_player_cards game
          (GameInterface.get_curr_player game))
       0)

(* need to change this so that the current card is the users selection *)
let rec remove_card idx lst =
  match (idx, lst) with
  | 0, _ :: t -> t
  | x, h :: t -> h :: remove_card (x - 1) t
  | _ -> raise (Invalid_argument "Invalid card")

let rec get_selected_card idx lst =
  match (idx, lst) with
  | 0, x :: _ -> x
  | x, _ :: t -> get_selected_card (x - 1) t
  | _ -> raise (Invalid_argument "Invalid card")

let validate_card (prev : card) (selected : card) : bool =
  match prev with
  | Regular (oldColor, oldNum) -> (
      match selected with
      | Regular (nColor, nNum) ->
          if oldColor = nColor || oldNum = nNum then true else false
      | Special (nColor, _) -> if oldColor = nColor then true else false
      | Wild -> true
      | _ -> false)
  | Special (oldColor, oldSpec) -> (
      match selected with
      | Regular (nColor, _) -> if oldColor = nColor then true else false
      | Special (nColor, nSpec) ->
          if oldColor = nColor || oldSpec = nSpec then true else false
      | Wild -> true
      | _ -> false)
  | PlacedWild oldColor -> (
      match selected with
      | Regular (nColor, _) -> if oldColor = nColor then true else false
      | Special (nColor, _) -> if oldColor = nColor then true else false
      | Wild -> true
      | _ -> false)
  | _ -> false

let check_forced_pickup game =
  let playerHand =
    GameInterface.get_player_cards game (GameInterface.get_curr_player game)
  in
  let rec check_hand hand =
    match hand with
    | [] -> true
    | h :: t ->
        if validate_card (GameInterface.get_curr_card game) h = false then
          check_hand t
        else false
  in
  check_hand playerHand

let rec let_player_select game =
  if check_forced_pickup game then (
    print_endline
      "You have no valid cards to play. You must pick up a card, click enter \
       to continue: ";
    ignore (read_line ());
    let add_card_to_player_cards =
      GameInterface.add_cards_to_hand game
        (GameInterface.get_curr_player game)
        1
    in
    print_endline
      ("Your new hand is: "
      ^ GameInterface.card_list_to_string add_card_to_player_cards 0);
    GameInterface.edit_player_cards game
      (GameInterface.get_curr_player game)
      add_card_to_player_cards)
  else
    let input = GameInterface.select_card () in

    let cards_post_remove =
      remove_card input
        (GameInterface.get_player_cards game
           (GameInterface.get_curr_player game))
    in
    let card_selected =
      get_selected_card input
        (GameInterface.get_player_cards game
           (GameInterface.get_curr_player game))
    in
    let prev_card = GameInterface.get_curr_card game in
    let card_validated = validate_card prev_card card_selected in
    if card_validated then (
      (match card_selected with
      | Wild ->
          GameInterface.chance_curr_card game
            (GameInterface.handle_wild (Some card_selected)
               (GameInterface.save_wild_input ()))
      | _ ->
          GameInterface.chance_curr_card game (Some card_selected);
          print_endline (GameInterface.card_list_to_string cards_post_remove 0);
          print_endline "");

      GameInterface.edit_player_cards game
        (GameInterface.get_curr_player game)
        cards_post_remove)
    else (
      print_endline
        (GameInterface.card_to_string card_selected
        ^ " is an invalid card, please pick a valid card");
      let_player_select game)

let player_turn game =
  display_player_cards game;
  GameInterface.next_player (let_player_select game)

(* let card_to_string (c : card) : string =
     "(" ^ color_to_string (fst c) ^ ", " ^ string_of_int (snd c) ^ ")"

   let print_curr_card game =
     match game.curr_card with
     | None -> print_string ""
     | Some x -> print_string card_to_string x *)

let rec play_game game =
  print_endline "";
  GameInterface.print_curr_card game;
  if GameInterface.check_if_win (GameInterface.get_players game) then
    print_endline "Congratulations, you won! \n Press enter again to end game"
  else play_game (player_turn game)

let create_game players =
  play_game
    (GameInterface.make_curr_card
       (GameInterface.create_players
          (GameInterface.empty false [])
          (int_of_string players) false))
