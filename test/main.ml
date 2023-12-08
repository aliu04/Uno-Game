open OUnit2
open Uno
module Test_Game = Uno.GameInterface

let rec test_initial_number_of_cards (p_num : int) : bool =
  let game = Test_Game.create_players Test_Game.empty p_num in
  match p_num with
  | 0 -> true
  | _ ->
      if Test_Game.get_player_num_of_cards game p_num = 7 then
        test_initial_number_of_cards (p_num - 1)
      else false

let test_cards =
  [
    Regular (Blue, 4);
    Regular (Red, 1);
    Regular (Green, 3);
    Regular (Yellow, 7);
    Special (Green, Reverse);
    Special (Red, Skip);
    Wild;
  ]

let game_tests =
  [
    ( "test if create_players makes the correct number of players for a 4 \
       player game"
    >:: fun _ ->
      assert_equal 4
        (let created_game = Test_Game.create_players Test_Game.empty 4 in
         Test_Game.get_player_number created_game) );
    ( "test if create_players makes the correct number of players for a 10 \
       player game"
    >:: fun _ ->
      assert_equal 10
        (let created_game = Test_Game.create_players Test_Game.empty 10 in
         Test_Game.get_player_number created_game) );
    ( "test number of players in empty_game" >:: fun _ ->
      assert_equal 0
        (let created_game = Test_Game.empty in
         Test_Game.get_player_number created_game) );
    ( "test create_players gives 4 players 7 cards each" >:: fun _ ->
      assert_equal true (test_initial_number_of_cards 4) );
    ( "test create_players gives 10 players 7 cards each" >:: fun _ ->
      assert_equal true (test_initial_number_of_cards 10) );
    ( "test players_to_string when 4 players" >:: fun _ ->
      assert_equal "1 2 3 4"
        (Test_Game.players_to_string
           (Test_Game.create_players Test_Game.empty 4)) );
    ( "test players_to_string when 10 players" >:: fun _ ->
      assert_equal "1 2 3 4 5 6 7 8 9 10"
        (Test_Game.players_to_string
           (Test_Game.create_players Test_Game.empty 10)) );
    ( "test players_to_string on empty game" >:: fun _ ->
      assert_equal "" (Test_Game.players_to_string Test_Game.empty) );
    ( "test cards_to_string on empty game" >:: fun _ ->
      assert_equal "" (Test_Game.cards_to_string Test_Game.empty) );
    ( "test next_player" >:: fun _ ->
      assert_equal 1
        (Test_Game.get_curr_player
           (Test_Game.next_player
              (Test_Game.make_curr_card
                 (Test_Game.create_players Test_Game.empty 4)))) );
    ( "test card_list_to_string" >:: fun _ ->
      assert_equal
        "(Blue, 4); (Red, 1); (Green, 3); (Yellow, 7); (Green, Reverse); (Red, \
         Skip); (Wild)"
        (Test_Game.card_list_to_string test_cards) );
  ]

let tests = "uno test suite" >::: List.flatten [ game_tests ]
let _ = run_test_tt_main tests
