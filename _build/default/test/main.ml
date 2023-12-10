open OUnit2
open Uno
module Test_Game = Uno.GameInterface

(*
   Testing Plan
   -----
   Almost every function within the module type Game was tested using OUnit 
   tests. Functions that were manually tested included those that requireed user
   inputs such as select_card, save_player_name, and save_wild_input. 
   To test these functions, we ran make uno to check that the game behaves
   as we would expect it to with manual use inputs. 

   Tests were developed using glass box testing. Several tests include iterations
   of the same function but testing various branches of the code. For example
   for the function next_player, test cases are written for when the
   current card is reverse, plus two, or skip. 

   The game module stores the essential logic behind our whole game. Thus, being
   able to write a test cases for all the essential functions within our module
   ensures correctness of our logic. Further, by using glass box testing, 
   we ensure that the tests touch upon all possible combinations our game could
   go through. Although our test suite is limited by the fact that we can't 
   test instances of manual user input, we manually incremented next_player
   features to act as if we are iterating through turns in the game. 


*)
let test_cards =
  [
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
  ]

let card_list_for_test =
  [
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
  ]

let rec test_initial_number_of_cards (p_num : int) : bool =
  let game =
    Test_Game.create_players (Test_Game.empty true test_cards) p_num true
  in
  match p_num with
  | 0 -> true
  | _ ->
      if Test_Game.get_player_num_of_cards game p_num = 7 then
        test_initial_number_of_cards (p_num - 1)
      else false

(* pretty printing functions are sourced from Cindy's A3*)
let pp_string s = "\"" ^ s ^ "\""

let test_helper_string out in1 _ =
  assert_equal ~printer:pp_string
    ~msg:
      ("function: Retruning a string output for the test\ninput: "
     ^ pp_string in1)
    out in1

let test_helper_int out in1 _ =
  assert_equal ~printer:pp_string
    ~msg:
      ("function: Retruning a string output for the test\ninput: "
      ^ pp_string (string_of_int in1))
    (string_of_int out) (string_of_int in1)

(* Test suite for basic functions of the game that are not
   dependent to the game state *)
let game_tests =
  [
    ( "test if create_players makes the correct number of players for a 4 \
       player game"
    >:: fun _ ->
      assert_equal 4
        (let created_game =
           Test_Game.create_players (Test_Game.empty true test_cards) 4 true
         in
         Test_Game.get_player_number created_game) );
    ( "test if create_players makes the correct number of players for a 10 \
       player game"
    >:: fun _ ->
      assert_equal 10
        (let created_game =
           Test_Game.create_players (Test_Game.empty true test_cards) 10 true
         in
         Test_Game.get_player_number created_game) );
    ( "test number of players in empty_game" >:: fun _ ->
      assert_equal 0
        (let created_game = Test_Game.empty true test_cards in
         Test_Game.get_player_number created_game) );
    ( "test create_players gives 4 players 7 cards each" >:: fun _ ->
      assert_equal true (test_initial_number_of_cards 4) );
    ( "test create_players gives 10 players 7 cards each" >:: fun _ ->
      assert_equal true (test_initial_number_of_cards 10) );
    ( "test players_to_string when 4 players" >:: fun _ ->
      assert_equal "1 2 3 4"
        (Test_Game.players_to_string
           (Test_Game.create_players (Test_Game.empty true test_cards) 4 true))
    );
    ( "test players_to_string when 10 players" >:: fun _ ->
      assert_equal "1 2 3 4 5 6 7 8 9 10"
        (Test_Game.players_to_string
           (Test_Game.create_players (Test_Game.empty true test_cards) 10 true))
    );
    ( "test players_to_string on empty game" >:: fun _ ->
      assert_equal ""
        (Test_Game.players_to_string (Test_Game.empty true test_cards)) );
    ( "test cards_to_string on empty game" >:: fun _ ->
      assert_equal ""
        (Test_Game.cards_to_string (Test_Game.empty true test_cards)) );
    ( "test next_player" >:: fun _ ->
      assert_equal 1
        (Test_Game.get_curr_player
           (Test_Game.next_player
              (Test_Game.make_curr_card
                 (Test_Game.create_players
                    (Test_Game.empty true test_cards)
                    4 true)))) );
    ( "test card_list_to_string" >:: fun _ ->
      assert_equal
        "(Blue, 4); (Red, 1); (Green, 3); (Yellow, 7); (Green, Reverse); (Red, \
         Skip); (Wild)"
        (Test_Game.card_list_to_string card_list_for_test) );
    ( "test handle_wild, changing the color to red" >:: fun _ ->
      assert_equal (Some (PlacedWild Red))
        (Test_Game.handle_wild (Some Wild) "red") );
    ( "test handle_wild, changing the color to red\n\
      \      checks for case insensitivity"
    >:: fun _ ->
      assert_equal (Some (PlacedWild Red))
        (Test_Game.handle_wild (Some Wild) "Red") );
    ( "test handle_wild, changing the color to blue" >:: fun _ ->
      assert_equal (Some (PlacedWild Blue))
        (Test_Game.handle_wild (Some Wild) "blue") );
    ( "test handle_wild, changing the color to yellow" >:: fun _ ->
      assert_equal (Some (PlacedWild Yellow))
        (Test_Game.handle_wild (Some Wild) "yellow") );
    ( "test handle_wild, changing the color to green" >:: fun _ ->
      assert_equal (Some (PlacedWild Green))
        (Test_Game.handle_wild (Some Wild) "green") );
    ( "test handle_wild, for a non wild card" >:: fun _ ->
      assert_equal
        (Some (Regular (Blue, 4)))
        (Test_Game.handle_wild (Some (Regular (Blue, 4))) "green") );
    ( "test handle_wild, for a none card" >:: fun _ ->
      assert_equal None (Test_Game.handle_wild None "green") );
  ]

let four_person_cards =
  [
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
  ]

let four_person_game =
  Test_Game.create_players (Test_Game.empty true four_person_cards) 4 true

let edit_cards_test =
  [
    Regular (Yellow, 7);
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
  ]

(* maybe make smth that toggles the total number of cards
   each player has to check ending the game

   add none check unchanged
   adding cards and reversing directions*)

(* Test suite for a four person game.
   Tests for functions that are dependent on each other*)

let four_player_test =
  [
    ( "Sets the current card of the game and tests if it matches" >:: fun _ ->
      assert_equal
        (Regular (Blue, 4))
        (Test_Game.chance_curr_card four_person_game (Some (Regular (Blue, 4)));
         Test_Game.get_curr_card four_person_game) );
    ( "Tests if adding 2 cards to a player works with add_cards_to_hand\n\
      \      Based on the hard coded test cards we pass to the Test_Game \
       function\n\
      \    we are able to check that the 2 cards given to the player are the \
       top \n\
      \    2 cards of the remaining deck"
    >:: fun _ ->
      assert_equal
        [
          Regular (Blue, 4);
          Regular (Red, 1);
          Regular (Green, 3);
          Regular (Yellow, 7);
          Special (Green, Reverse);
          Special (Red, Skip);
          Wild;
          Regular (Blue, 4);
          Regular (Red, 1);
        ]
        (Test_Game.add_cards_to_hand
           (Test_Game.create_players
              (Test_Game.empty true four_person_cards)
              4 true)
           0 2) );
    ( "Tests if adding 0 cards to a player works with add_cards_to_hand"
    >:: fun _ ->
      assert_equal
        [
          Regular (Blue, 4);
          Regular (Red, 1);
          Regular (Green, 3);
          Regular (Yellow, 7);
          Special (Green, Reverse);
          Special (Red, Skip);
          Wild;
        ]
        (Test_Game.add_cards_to_hand
           (Test_Game.create_players
              (Test_Game.empty true four_person_cards)
              4 true)
           0 0) );
    ( "Tests edit_player_cards and then get_player_cards for the first player"
    >:: fun _ ->
      assert_equal edit_cards_test
        (Test_Game.get_player_cards
           (Test_Game.edit_player_cards four_person_game 0 edit_cards_test)
           0) );
    ( "Tests edit_player_cards and then get_player_cards for the fourth player"
    >:: fun _ ->
      assert_equal edit_cards_test
        (Test_Game.get_player_cards
           (Test_Game.edit_player_cards four_person_game 3 edit_cards_test)
           3) );
    "Utilizes add_cards_to_hand to produce cards\n\
    \    for  edit_player_cards and then runs get_player_cards for the first \
     player"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Blue, 4);
               Regular (Red, 1);
             ])
          (let temp_game =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards
                (Test_Game.edit_player_cards temp_game 0
                   (Test_Game.add_cards_to_hand temp_game 0 2))
                0));
    "Checks available cards left after giving 2 cards to player 0"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Green, 3);
               Regular (Yellow, 7);
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
             ])
          (let temp_game =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.card_list_to_string
             (Test_Game.get_avail_cards
                (Test_Game.edit_player_cards temp_game 0
                   (Test_Game.add_cards_to_hand temp_game 0 2))));
    "Adds 2 cards to hand for the first player, then with those remaining cards\n\
    \    adds 2 cards to the next player"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Green, 3);
               Regular (Yellow, 7);
             ])
          (let temp_game =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards
                (Test_Game.edit_player_cards temp_game 1
                   (Test_Game.add_cards_to_hand
                      (Test_Game.edit_player_cards temp_game 0
                         (Test_Game.add_cards_to_hand temp_game 0 2))
                      1 2))
                1));
    "Adds cards to hand to first player but adds 0 cards, so\n\
    \    available cards should remain unchanged. Then adds 2 cards to the \
     next player."
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Blue, 4);
               Regular (Red, 1);
             ])
          (let temp_game =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards
                (Test_Game.edit_player_cards temp_game 1
                   (Test_Game.add_cards_to_hand
                      (Test_Game.edit_player_cards temp_game 0
                         (Test_Game.add_cards_to_hand temp_game 0 0))
                      1 2))
                1));
    "Checks for skipping a player in clockwise direction"
    >:: test_helper_int 2
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.chance_curr_card game_for_dir
             (Some (Special (Green, Skip)));
           Test_Game.get_curr_player (Test_Game.next_player game_for_dir));
    "Checks for skipping a player in clockwise direction"
    >:: test_helper_int 2
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.change_direction game_for_dir;
           Test_Game.chance_curr_card game_for_dir
             (Some (Special (Green, Skip)));
           Test_Game.get_curr_player (Test_Game.next_player game_for_dir));
    ( "Checks for the direction of the game when the currend card is reverse\n\
      \    and the game is initially clockwise"
    >:: fun _ ->
      assert_equal Counterclockwise
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir
           (Some (Special (Green, Reverse)));
         Test_Game.get_direction (Test_Game.next_player game_for_dir)) );
    ( "Checks for the direction of the game when the currend card is reverse\n\
      \    and the game is initially counterclockwise"
    >:: fun _ ->
      assert_equal Clockwise
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir
           (Some (Special (Green, Reverse)));
         Test_Game.change_direction game_for_dir;
         Test_Game.get_direction (Test_Game.next_player game_for_dir)) );
    ( "Checks for the direction of the game when the currend card is reverse\n\
      \    and the game is initially counterclockwise"
    >:: fun _ ->
      assert_equal Clockwise
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir
           (Some (Special (Green, Reverse)));
         Test_Game.change_direction game_for_dir;
         Test_Game.get_direction (Test_Game.next_player game_for_dir)) );
    "Checks that the next player's cards has + 2 when the current card is +2\n\
    \        in the initial clockwise direction"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Blue, 4);
               Regular (Red, 1);
             ])
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.chance_curr_card game_for_dir
             (Some (Special (Green, PlusTwo)));
           let updated_game = Test_Game.next_player game_for_dir in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards updated_game 1));
    "Checks that the next player's cards has + 2 when the current card is +2\n\
    \    but in the counterclockwise direction"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Blue, 4);
               Regular (Red, 1);
             ])
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.change_direction game_for_dir;
           Test_Game.chance_curr_card game_for_dir
             (Some (Special (Green, PlusTwo)));
           let updated_game = Test_Game.next_player game_for_dir in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards updated_game 3));
    ( "Tests get_direction for the initial game" >:: fun _ ->
      assert_equal Clockwise (Test_Game.get_direction four_person_game) );
    ( "Tests gets the next player for the initial game\n\
      \    which is expected to be the player at index 0"
    >:: fun _ -> assert_equal 0 (Test_Game.get_curr_player four_person_game) );
    ( "Given the initial game and clockwise direction, checks that the\n\
      \    next player should be at index 1"
    >:: fun _ ->
      assert_equal 1
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
         Test_Game.get_curr_player (Test_Game.next_player game_for_dir)) );
    ( "Given the initial game and clockwise direction, checks that the\n\
      \    next player of the next player should be at index 2"
    >:: fun _ ->
      assert_equal 2
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
         Test_Game.get_curr_player
           (Test_Game.next_player (Test_Game.next_player game_for_dir))) );
    ( "Given the initial game and counterclockwise direction, checks that the\n\
      \    next player should be at index 3"
    >:: fun _ ->
      assert_equal 3
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
         Test_Game.change_direction game_for_dir;
         Test_Game.get_curr_player (Test_Game.next_player game_for_dir)) );
    ( "Given the initial game and counterclockwise direction, checks that the\n\
      \    next player of the next player should be at index 2"
    >:: fun _ ->
      assert_equal 2
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
         Test_Game.change_direction game_for_dir;
         Test_Game.get_curr_player
           (Test_Game.next_player (Test_Game.next_player game_for_dir))) );
    ( "Given the initial game, sets the next player, then changes the game to ccw\n\
      \    gets that next player and the result should be the player at index 0"
    >:: fun _ ->
      assert_equal 0
        (let game_for_dir =
           Test_Game.create_players
             (Test_Game.empty true four_person_cards)
             4 true
         in
         Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
         let game_after_player = Test_Game.next_player game_for_dir in
         Test_Game.change_direction game_after_player;
         Test_Game.get_curr_player (Test_Game.next_player game_after_player)) );
    "Nests changing directions and adding cards to a player, uses the \
     get_curr_player\n\
    \    and edits that players cards"
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Blue, 4);
               Regular (Red, 1);
             ])
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
           let curr_player =
             Test_Game.get_curr_player (Test_Game.next_player game_for_dir)
           in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards
                (Test_Game.edit_player_cards game_for_dir curr_player
                   (Test_Game.add_cards_to_hand game_for_dir curr_player 2))
                curr_player));
    "Given the initial game, sets the next player, then changes the game to ccw\n\
    \    gets that next player and the result should be the player at index 0\n\
    \    In between each next player, will give 2 cards to each player\n\
    \    Thus the final player is expected to have the same initial card set\n\
    \    and Green and Yellow because those are the remaining cards in \
     available cards\n\
    \    "
    >:: test_helper_string
          (Test_Game.card_list_to_string
             [
               Regular (Blue, 4);
               Regular (Red, 1);
               Regular (Green, 3);
               Regular (Yellow, 7);
               Special (Green, Reverse);
               Special (Red, Skip);
               Wild;
               Regular (Green, 3);
               Regular (Yellow, 7);
             ])
          (let game_for_dir =
             Test_Game.create_players
               (Test_Game.empty true four_person_cards)
               4 true
           in
           Test_Game.chance_curr_card game_for_dir (Some (Regular (Blue, 4)));
           let game_after_player = Test_Game.next_player game_for_dir in
           let game_after_cards =
             Test_Game.edit_player_cards game_after_player
               (Test_Game.get_curr_player game_after_player)
               (Test_Game.add_cards_to_hand game_for_dir
                  (Test_Game.get_curr_player game_after_player)
                  2)
           in
           Test_Game.change_direction game_after_cards;
           let curr_player =
             Test_Game.get_curr_player (Test_Game.next_player game_after_cards)
           in
           Test_Game.card_list_to_string
             (Test_Game.get_player_cards
                (Test_Game.edit_player_cards game_for_dir curr_player
                   (Test_Game.add_cards_to_hand game_for_dir curr_player 2))
                curr_player));
    ( "Checks the winner function for a game by manually setting one of the\n\
      \    players cards to the empty list"
    >:: fun _ ->
      assert_equal true
        (Test_Game.check_if_win
           (Test_Game.get_players
              (Test_Game.edit_player_cards
                 (Test_Game.create_players
                    (Test_Game.empty true four_person_cards)
                    4 true)
                 3 []))) );
    ( "Checks the winner function for a game for a game where all the \n\
      \    players have cards, should return false"
    >:: fun _ ->
      assert_equal false
        (Test_Game.check_if_win
           (Test_Game.get_players
              (Test_Game.create_players
                 (Test_Game.empty true four_person_cards)
                 4 true))) );
  ]

let tests = "uno test suite" >::: List.flatten [ game_tests @ four_player_test ]
let _ = run_test_tt_main tests
