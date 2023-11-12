open Uno

(* read-eval-print loop *)
let rec repl (eval : string -> unit) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      print_endline "Starting game...";
      input |> eval;
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Uno.\n";
  print_endline "Please enter the number of players (4-10):";
  repl create_game
