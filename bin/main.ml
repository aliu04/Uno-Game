open Uno

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Uno.\n";
  print_endline "Please enter the number of players (4-10):";
  repl create_game
