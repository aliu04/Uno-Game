type color = Red | Green | Yellow | Blue
type card = color * int

module type Game = sig
  type 'a t

  val empty : 'a t
  val get_player_number : 'a t -> int
  val get_player_num_of_cards : 'a t -> int -> int
  val create_players : 'a t -> int -> 'a t
  val players_to_string : 'a t -> string
  val cards_to_string : 'a t -> string
end

type player = {
  name : string;
  cards : card list;
  curr_card : int option;
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
    available_cards : card list;
    curr_card : card option;
  }

  let empty = { players = []; available_cards = all_cards; curr_card = None }
  let get_player_number (game : 'a t) : int = List.length game.players

  let get_player_num_of_cards (game : 'a t) (p_num : int) : int =
    let player = List.nth game.players (p_num - 1) in
    List.length player.cards

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
        curr_card = None;
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
end

module GameInterface = GameInstance

(* let create_game players =
   GameInterface.players_to_string
     (GameInterface.create_players GameInterface.empty (int_of_string players)) *)

let create_game players =
  GameInterface.cards_to_string
    (GameInterface.create_players GameInterface.empty (int_of_string players))
