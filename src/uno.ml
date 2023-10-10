type color = Red | Green | Yellow | Blue
type card = color * int

module type Game = sig
  type 'a t

  val empty : 'a t
  val create_players : 'a t -> int -> 'a t
  val players_to_string : 'a t -> string
end

type player = {
  name : string;
  cards : card list;
  curr_card : int option;
  win : bool option;
}

let all_cards = [ (Red, 1); (Red, 2); (Red, 3); (Red, 4) ]

module GameInstance : Game = struct
  type 'a t = {
    players : player list;
    available_cards : card list;
    curr_card : card option;
  }

  let empty = { players = []; available_cards = all_cards; curr_card = None }

  let rec get_n_cards pool receive n =
    match (n, pool) with
    | 0, _ -> (receive, pool)
    | x, h :: t -> get_n_cards t (receive @ [ h ]) (x - 1)
    | _ -> raise (Invalid_argument "Not enough cards")

  let create_player (card_list : card list) : player * card list =
    let play_cards, remain_cards = get_n_cards card_list [] 2 in
    ( { name = "Player 1"; cards = play_cards; curr_card = None; win = None },
      remain_cards )

  let rec create_players (game : 'a t) (player_num : int) : 'a t =
    match player_num with
    | 0 -> game
    | _ ->
        let player, cards = create_player game.available_cards in
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
end

module GameInterface = GameInstance

let create_game players =
  GameInterface.players_to_string
    (GameInterface.create_players GameInterface.empty (int_of_string players))
