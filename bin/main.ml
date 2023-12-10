open Uno

(* read-eval-print loop *)
let rec repl (eval : string -> unit) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      print_endline "";
      print_endline "Starting game...";
      input |> eval;
      print_endline "";
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Uno.\n";
  print_endline "";
  print_endline
    "Press [Enter] to read over instructions otherwise press any other key to \
     play";
  let input = read_line () in
  match input with
  | "" ->
      print_endline
        "The objective of Uno is to be the first one to clear your hand. \n\
        \ Every player will start out with 7 cards. \n\
        \ Given your turn, you are allowed to place down a card that is of the \
         same color or number of the current card of the game. \n\
        \ \n\
        \ There are specialty cards in the game, which are Wild, Reverse, \
         Skip, and Plus two. \n\
        \ For wild cards, you are allowed to change the color of the\n\
        \ current card. \n\
        \ For skip, you would skip the next player's turn. \n\
        \ For reverse, you would reverse the direction in the game will go in. \n\
        \ For plus 2, it would add 2 cards to the next players hand. \n\
        \ The game ends when one player has no cards left. \n\
        \ \n\
        \ Have fun!";
      print_endline "";
      print_endline "Starting game...";
      print_endline "Please enter the number of players (4-10):";
      repl create_game
  | _ ->
      print_endline "";
      print_endline "Starting game...";

      print_endline "";
      print_endline "Please enter the number of players (4-10):";
      repl create_game
