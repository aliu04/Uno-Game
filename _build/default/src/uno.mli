(** The variant to represent the different colors of the cards *)
type color = Red | Green | Yellow | Blue

(** The variant to represent the different kinds of special cards *)
type special = Reverse | Skip | PlusTwo

(** The variant for the card type*)
type card =
  | Regular of color * int
  | Special of color * special
  | Wild
  | PlacedWild of color

(** The variant for the direction the game will proceed in*)
type directions = Clockwise | Counterclockwise

type player = {
  name : string;
  mutable numcards : int;
  mutable cards : card list;
  curr_card_player : int option;
  win : bool option;
}
(** The record type for player*)

(*Each instance of a game will be of type Game*)
module type Game = sig
  (* Representation type of the game *)
  type 'a t

  (* Initializes the empty game *)
  val empty : 'a t

  (* Returns the number of players in the game *)
  val get_player_number : 'a t -> int

  (* Given game g and player index p, get_player_num_of_cards g p
     returns the number of cards player p has in game g *)
  val get_player_num_of_cards : 'a t -> int -> int

  (* Given game g and player index p, get_player_cards g p
     returns the cards player p has in game g *)
  val get_player_cards : 'a t -> int -> card list

  (* Given game g and n as the number of players, hands out 7 cards
     to each player using the available cards left in the game g *)
  val create_players : 'a t -> int -> 'a t
  val players_to_string : 'a t -> string
  val card_to_string : card -> string
  val cards_to_string : 'a t -> string
  val card_list_to_string : card list -> string
  val edit_player_cards : 'a t -> int -> card list -> 'a t
  val make_curr_card : 'a t -> 'a t
  val print_curr_card : 'a t -> unit
  val chance_curr_card : 'a t -> card option -> unit
  val add_curr_card_to_cards : 'a t -> card list -> card list
  val add_cards_to_hand : 'a t -> int -> int -> card list
  val check_if_win : player list -> bool
  val get_players : 'a t -> player list
  val get_curr_card : 'a t -> card
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

module GameInterface : Game

val create_game : string -> unit
