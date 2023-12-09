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

(** The record type for player*)

type player = {
  name : string;
  mutable numcards : int;
  mutable cards : card list;
  curr_card_player : int option;
  win : bool option;
}

(** Each instance of a game will be of type Game*)

module type Game = sig
  (*** Representation type of the game *)

  type 'a t

  (** Initializes the empty game
     If the game is a normal game instance, will simply use the available
     cards by default. If it is a test game, will use the card list
     passed in. *)

  val empty : bool -> card list -> 'a t

  (** Returns the number of players in the game *)

  val get_player_number : 'a t -> int

  (** Given game g and player index p, get_player_num_of_cards g p
     returns the number of cards player p has in game g *)

  val get_player_num_of_cards : 'a t -> int -> int

  (** Given game g and player index p, get_player_cards g p
     returns the cards player p has in game g *)

  val get_player_cards : 'a t -> int -> card list

  (** Given game g and n as the number of players, hands out 7 cards
     to each player using the available cards left in the game g
     Returns an updated game instance with players having appropriate
     cards.
     Takes in an extra boolean input, test, which indicates if this is
      a game running from the test suite or actual game.
  *)

  val create_players : 'a t -> int -> bool -> 'a t

  (** Given game g returns a string including all the player names *)

  val players_to_string : 'a t -> string

  (** Simply converts a card type to its string counterpart *)

  val card_to_string : card -> string

  (**Returns a string of all the players in the game and their cards *)

  val cards_to_string : 'a t -> string

  (** Given a list of cards, concatenates each card value to a string *)

  val card_list_to_string : card list -> string

  (** Given game g, a player at index p and a new card list c
        edit_player_cards g p c will update the player's cards at index p
        with the new cards c*)

  val edit_player_cards : 'a t -> int -> card list -> 'a t

  (** Given game g, initializes the curr_card of the game with the card
     at the top of the available cards list *)

  val make_curr_card : 'a t -> 'a t

  (** Given game g, prints the current card of the game*)

  val print_curr_card : 'a t -> unit

  (** Given game g and card c, changes the current card of the game to c*)

  val chance_curr_card : 'a t -> card option -> unit

  val add_curr_card_to_cards : 'a t -> card list -> card list
  (** NOT SURE WHAT THIS DOES *)

  (** Given game g, adds num random cards to the cards held by player
     represented by p_num *)

  val add_cards_to_hand : 'a t -> int -> int -> card list

  (** Given a list of players p, checks if any of the players have
     0 cards left, if so then a player has won and the function will
       return true*)

  val check_if_win : player list -> bool

  (** Returns the player list from a game*)

  val get_players : 'a t -> player list

  (** Given game g, returns the current card in the game*)

  val get_curr_card : 'a t -> card

  (** Given game g, the available cards in the game*)

  val get_avail_cards : 'a t -> card list

  (** Given game g, changes its direction to counter clockwise if
     previous directoin was clockwise and viceversa*)

  val change_direction : 'a t -> unit

  (** Given game g, gets the direction of the game g*)

  val get_direction : 'a t -> directions

  (** Given game g, gets the next_player in the game
     by checking the direction the game is in and based on
     that direction computes a modulo value which indicates
     the next player that should have a turn*)

  val next_player : 'a t -> 'a t

  (** Given game g, returns the index of the current player*)

  val get_curr_player : 'a t -> int

  (** Given game g, returns the name of the current player*)

  val get_curr_player_name : 'a t -> string

  (** Given card c, handles the wild card where the
     user can change the color of the current card*)

  val handle_wild : card option -> string -> card option

  (** The function which allows users to select a card to place down*)

  val select_card : unit -> int

  (** The function which allows users to name themsleves*)

  val save_player_name : unit -> string

  (** The function which allows users to recolor cards*)

  val save_wild_input : unit -> string
end

(** An instance of the module for the Game*)

module GameInterface : Game

(** The function that runs the game, it is called from bin/main.ml *)

val create_game : string -> unit
