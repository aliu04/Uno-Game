(** The signature of sampleable bags (multisets). *)
type color = Red | Green | Yellow | Blue

type card = color * int

type player = {
  name : string;
  mutable cards : card list;
  curr_card : int option;
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
end

module GameInterface : Game

val create_game : string -> unit
