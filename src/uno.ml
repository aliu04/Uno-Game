type color = Red | Green | Yellow | Blue
type card = color * int

module type Game = sig
  type 'a t

  val empty : 'a t
  val get_player_number : 'a t -> int
  val get_player_num_of_cards : 'a t -> int -> int
  val get_player_cards : 'a t -> int -> card list
  val create_players : 'a t -> int -> 'a t
  val players_to_string : 'a t -> string
  val cards_to_string : 'a t -> string
  val card_list_to_string : card list -> string
  val edit_player_cards : 'a t -> int -> card list -> 'a t
  val make_curr_card : 'a t -> 'a t
  val print_curr_card : 'a t -> unit
  val chance_curr_card : 'a t -> card option -> unit
  val add_curr_card_to_cards : 'a t -> card list -> card list
end

type player = {
  name : string;
  mutable cards : card list;
  curr_card_player : int option;
  win : bool option;
}

let red_cards =
  [
    (Red, 0);
    (Red, 1);
    (Red, 2);
    (Red, 3);
    (Red, 4);
    (Red, 5);
    (Red, 6);
    (Red, 7);
    (Red, 8);
    (Red, 9);
  ]

let yellow_cards =
  [
    (Yellow, 0);
    (Yellow, 1);
    (Yellow, 2);
    (Yellow, 3);
    (Yellow, 4);
    (Yellow, 5);
    (Yellow, 6);
    (Yellow, 7);
    (Yellow, 8);
    (Yellow, 9);
  ]

let blue_cards =
  [
    (Blue, 0);
    (Blue, 1);
    (Blue, 2);
    (Blue, 3);
    (Blue, 4);
    (Blue, 5);
    (Blue, 6);
    (Blue, 7);
    (Blue, 8);
    (Blue, 9);
  ]

let green_cards =
  [
    (Green, 0);
    (Green, 1);
    (Green, 2);
    (Green, 3);
    (Green, 4);
    (Green, 5);
    (Green, 6);
    (Green, 7);
    (Green, 8);
    (Green, 9);
  ]

(*SHUFFLE CODE IS COPY AND PASTED FROM STACK OVERFLOW
    https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let all_cards =
  shuffle
    (red_cards @ red_cards @ blue_cards @ blue_cards @ green_cards @ green_cards
   @ yellow_cards @ yellow_cards)

(* Potentially implement a functor that takes in the game instance to play the game*)

module GameInstance : Game = struct
  type 'a t = {
    players : player list;
    mutable available_cards : card list;
    mutable curr_card : card option;
  }

  let empty = { players = []; available_cards = all_cards; curr_card = None }
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
    game

  let rec get_n_cards pool receive n =
    match (n, pool) with
    | 0, _ -> (receive, pool)
    | x, h :: t -> get_n_cards t (receive @ [ h ]) (x - 1)
    | _ -> raise (Invalid_argument "Not enough cards")

  let create_player (card_list : card list) (name : int) : player * card list =
    let play_cards, remain_cards = get_n_cards card_list [] 7 in
    ( {
        name = string_of_int name;
        cards = play_cards;
        curr_card_player = None;
        win = None;
      },
      remain_cards )

  let rec create_players (game : 'a t) (player_num : int) : 'a t =
    match player_num with
    | 0 -> game
    | x ->
        let player, cards = create_player game.available_cards x in
        create_players
          {
            available_cards = cards;
            players = game.players @ [ player ];
            curr_card = game.curr_card;
          }
          (player_num - 1)

  let make_curr_card (game : 'a t) : 'a t =
    let curr_card, remain =
      match game.available_cards with [] -> (None, []) | h :: t -> (Some h, t)
    in
    { players = game.players; available_cards = remain; curr_card }

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

  let card_to_string (c : card) : string =
    "(" ^ color_to_string (fst c) ^ ", " ^ string_of_int (snd c) ^ ")"

  let rec card_list_to_string (c : card list) : string =
    match c with
    | [] -> ""
    | h :: [] -> card_to_string h
    | h :: t -> card_to_string h ^ "; " ^ card_list_to_string t

  let rec players_cards_to_list (pl : player list) : string =
    match pl with
    | [] -> ""
    | h :: t ->
        (h.name ^ ": " ^ "[" ^ card_list_to_string h.cards)
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

let display_player_cards game round_num =
  print_endline
    ("Player "
    ^ string_of_int (round_num mod GameInterface.get_player_number game)
    ^ " turn");
  print_endline
    (GameInterface.card_list_to_string
       (GameInterface.get_player_cards game
          (round_num mod GameInterface.get_player_number game)))

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

let let_player_select game round_num =
  print_endline "Select a card to place down";
  print_string "> ";
  let input = read_line () in
  match input with
  | "" ->
      let add_card_to_player_cards =
        GameInterface.add_curr_card_to_cards game
          (GameInterface.get_player_cards game
             (round_num mod GameInterface.get_player_number game))
      in
      print_endline (GameInterface.card_list_to_string add_card_to_player_cards);
      GameInterface.edit_player_cards game
        (round_num mod GameInterface.get_player_number game)
        add_card_to_player_cards
  | _ ->
      let cards_post_remove =
        remove_card (int_of_string input)
          (GameInterface.get_player_cards game
             (round_num mod GameInterface.get_player_number game))
      in
      let card_selected =
        get_selected_card (int_of_string input)
          (GameInterface.get_player_cards game
             (round_num mod GameInterface.get_player_number game))
      in
      GameInterface.chance_curr_card game (Some card_selected);
      print_endline (GameInterface.card_list_to_string cards_post_remove);
      print_endline "";
      GameInterface.edit_player_cards game
        (round_num mod GameInterface.get_player_number game)
        cards_post_remove

let player_turn game round_num =
  display_player_cards game round_num;
  let_player_select game round_num

(* let card_to_string (c : card) : string =
     "(" ^ color_to_string (fst c) ^ ", " ^ string_of_int (snd c) ^ ")"

   let print_curr_card game =
     match game.curr_card with
     | None -> print_string ""
     | Some x -> print_string card_to_string x *)

let rec play_game num_rounds game =
  GameInterface.print_curr_card game;
  match num_rounds with
  | 0 -> print_endline "bye"
  | x -> play_game (x - 1) (player_turn game x)

let create_game players =
  play_game
    (7 * int_of_string players)
    (GameInterface.make_curr_card
       (GameInterface.create_players GameInterface.empty (int_of_string players)))
