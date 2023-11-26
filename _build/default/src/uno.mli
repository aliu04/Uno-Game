(** The signature of sampleable bags (multisets). *)
type color = Red | Green | Yellow | Blue

type special = Reverse | Skip
type card = Regular of color * int | Special of color * special
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
  val check_if_win : player list -> bool
  val check_if_skip : card -> bool
  val get_players : 'a t -> player list
  val get_curr_card : 'a t -> card
  val change_direction : 'a t -> unit
  val get_direction : 'a t -> directions
  val check_if_reverse : card -> bool
  val next_player : 'a t -> 'a t
  val get_curr_player : 'a t -> int
end

module GameInterface : Game

val create_game : string -> unit
