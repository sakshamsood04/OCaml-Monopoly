open OUnit2
open Printf
open Monopoly
open Game

(*Testing Plan

  Functions tested automatically by OUnit Test suite: Color collection; String
  of color; String of owner; Roll Dice; getBoard; Game initialization;
  execute_go; check_collection; remove_owner; execute_jail; calculate_value;
  execute_income_tax; execute_luxury_tax; execute_rent; game_over Disclaimer:
  These functions were tested specifically for cases which did not involve user
  input from the terminal. The functions and cases which required user input
  were tested manually.

  Functions tested manually using the terminal: play; take_turn; game_loop;
  get_player_names; display_player_info; display_card; sell; sell_properties;
  check_bankruptcy

  All test cases will be developed using the glass box testing technique. This
  testing approach is effective in demonstrating correctness because we will be
  able to target specific edge cases which could possibly lead to errors, while
  the other tests will make sure that the usual cases are working normally too.
  This approach lets us cast a wide net to catch multiple possible issues in our
  code.

  The OUnit test cases will let us check individual functions and their
  behaviour for specific values which is useful in catching edge cases and non
  trivial erros. On the other hand, manual testing is also very important for
  our game because a lot of the functions require user input to process
  information and move ahead. We will run the game several times playing amongst
  ourselves to try out various situations. We will also modify starting
  conditions to force certain scenarios that we want to evaluate in the
  terminal. *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let test_player =
  {
    name = "TestPlayer";
    money = 0;
    properties = [];
    position = 0;
    jail_turns = 0;
  }

let test_card1 =
  {
    name = "TestCard 1";
    cost = 100;
    owner = None;
    color = Blue;
    rent = 50;
    typ = Property;
  }

let test_card2 =
  {
    name = "TestCard 2";
    cost = 100;
    owner = Some test_player;
    color = Blue;
    rent = 50;
    typ = Property;
  }

let player1 =
  {
    name = "Player1";
    money = 1500;
    properties = [];
    position = 0;
    jail_turns = 0;
  }

let player2 =
  {
    name = "Player2";
    money = 1500;
    properties = [];
    position = 0;
    jail_turns = 0;
  }

let create_test_game () =
  let players = [ player1; player2 ] in
  let board = getBoard in
  { board; players; current_player = 0 }

let utilityPlayer =
  {
    name = "UtilityPlayer";
    money = 1000;
    properties = [ List.nth getBoard 12; List.nth getBoard 28 ];
    position = 0;
    jail_turns = 0;
  }

let bluePlayer =
  {
    name = "BluePlayer";
    money = 1000;
    properties = [ List.nth getBoard 37; List.nth getBoard 39 ];
    position = 0;
    jail_turns = 0;
  }

let iAve =
  {
    name = "Illinois Avenue";
    cost = 240;
    owner = None;
    color = Red;
    rent = 20;
    typ = Property;
  }

let bRail =
  {
    name = "B&O Railroad";
    cost = 200;
    owner = None;
    color = Black;
    rent = 25;
    typ = Property;
  }

let aAve =
  {
    name = "Atlantic Avenue";
    cost = 260;
    owner = None;
    color = Yellow;
    rent = 22;
    typ = Property;
  }

let props = [ iAve; bRail; aAve ]

let propPlayer =
  {
    name = "PropPlayer";
    money = 1000;
    properties = props;
    position = 0;
    jail_turns = 0;
  }

let check_range (x : int) : bool = x >= 0 && x < 36

let individual_functions_test =
  [
    (* Color Collection *)
    ("Brown" >:: fun _ -> assert_equal 2 (color_collection Brown));
    ("Sky" >:: fun _ -> assert_equal 3 (color_collection Sky));
    ("Red" >:: fun _ -> assert_equal 3 (color_collection Red));
    ("Green" >:: fun _ -> assert_equal 3 (color_collection Green));
    ("Blue" >:: fun _ -> assert_equal 2 (color_collection Blue));
    ("White" >:: fun _ -> assert_equal 0 (color_collection White));
    ("Pink" >:: fun _ -> assert_equal 3 (color_collection Pink));
    ("Black" >:: fun _ -> assert_equal 4 (color_collection Black));
    ("Orange" >:: fun _ -> assert_equal 3 (color_collection Orange));
    ("Yellow" >:: fun _ -> assert_equal 3 (color_collection Yellow));
    ("Utility" >:: fun _ -> assert_equal 2 (color_collection Utility));
    (* String of color *)
    ("White" >:: fun _ -> assert_equal "White" (string_of_col White));
    ("Blue" >:: fun _ -> assert_equal "Blue" (string_of_col Blue));
    ("Green" >:: fun _ -> assert_equal "Green" (string_of_col Green));
    (* String of owner *)
    ("No owner" >:: fun _ -> assert_equal "None" (string_of_owner test_card1));
    ( "Some owner" >:: fun _ ->
      assert_equal "TestPlayer" (string_of_owner test_card2) );
    (* Roll Dice *)
    ( "Within range" >:: fun _ ->
      assert_bool "Out of range" (check_range (roll_dice ())) );
    (* Checking getBoard*)
    ( "Making sure our board has all the required tiles" >:: fun _ ->
      assert_equal 40 (List.length getBoard) );
    ( "Making sure our board has all the property tiles" >:: fun _ ->
      assert_equal 28
        (List.length (List.filter (fun x -> x.typ = Property) getBoard)) );
    ( "Making sure our board has all the Chance tiles" >:: fun _ ->
      assert_equal 3
        (List.length (List.filter (fun x -> x.typ = Chance) getBoard)) );
    ( "Making sure our board has all the Community Chest tiles" >:: fun _ ->
      assert_equal 3
        (List.length (List.filter (fun x -> x.typ = Community_chest) getBoard))
    );
    ( "Making sure our board has the Go tile" >:: fun _ ->
      assert_equal 1 (List.length (List.filter (fun x -> x.typ = Go) getBoard))
    );
    ( "Making sure our board has the Income Tax tile" >:: fun _ ->
      assert_equal 1
        (List.length (List.filter (fun x -> x.typ = ITax) getBoard)) );
    ( "Making sure our board has the Luxury Tax tile" >:: fun _ ->
      assert_equal 1
        (List.length (List.filter (fun x -> x.typ = LTax) getBoard)) );
    ( "Making sure our board has the Jail tile" >:: fun _ ->
      assert_equal 1
        (List.length (List.filter (fun x -> x.typ = Jail) getBoard)) );
    ( "Making sure our board has the Go to Jail tile" >:: fun _ ->
      assert_equal 1
        (List.length (List.filter (fun x -> x.typ = GotoJail) getBoard)) );
    ( "Making sure our board has the Free Parking tile" >:: fun _ ->
      assert_equal 1
        (List.length (List.filter (fun x -> x.typ = Free) getBoard)) );
    (* Testing game initialization *)
    ( "Making sure we have the right number of players" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        2 (List.length game.players) );
    ( "Making sure we start with the right player" >:: fun _ ->
      let game = create_test_game () in
      assert_equal ~printer:(fun x -> string_of_int x) 0 game.current_player );
    ( "Making sure the first player has the correct amount of starting money"
    >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        1500 (List.nth game.players 0).money );
    ( "Making sure the second player has the correct amount of starting money"
    >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        1500 (List.nth game.players 1).money );
    ( "Making sure the first player has 0 starting properties" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0
        (List.length (List.nth game.players 0).properties) );
    ( "Making sure the second player has 0 starting properties" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0
        (List.length (List.nth game.players 1).properties) );
    ( "Making sure the first player has 0 starting position" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0 (List.nth game.players 0).position );
    ( "Making sure the second player has 0 starting position" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0 (List.nth game.players 1).position );
    ( "Making sure the first player has 0 starting jail turns" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0 (List.nth game.players 0).jail_turns );
    ( "Making sure the second player has 0 starting jail turns" >:: fun _ ->
      let game = create_test_game () in
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0 (List.nth game.players 1).jail_turns );
    (* execute_go *)
    ( "Making sure the player is printed corrrectly" >:: fun _ ->
      let game = create_test_game () in
      execute_go (List.nth game.players 1);
      assert_equal
        ~printer:(fun x -> string_of_int x)
        1700 (List.nth game.players 1).money );
    (* check_collection *)
    ( "Checking if a player's collection is complete for Utility color"
    >:: fun _ ->
      assert_equal
        ~printer:(fun x -> string_of_bool x)
        true
        (check_collection utilityPlayer Utility) );
    ( "Checking if a player's collection is incomplete for Utility color"
    >:: fun _ ->
      utilityPlayer.properties <- [];
      assert_equal
        ~printer:(fun x -> string_of_bool x)
        false
        (check_collection utilityPlayer Utility) );
    ( "Checking if a player's collection is complete for Blue color" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> string_of_bool x)
        true
        (check_collection bluePlayer Blue) );
    ( "Checking if a player's collection is incomplete for Blue color"
    >:: fun _ ->
      bluePlayer.properties <- [ List.nth getBoard 37 ];
      assert_equal
        ~printer:(fun x -> string_of_bool x)
        false
        (check_collection bluePlayer Blue) );
    (* remove player *)
    ( "Checking first property after removing the owner of three properties"
    >:: fun _ ->
      iAve.owner <- Some propPlayer;
      bRail.owner <- Some propPlayer;
      aAve.owner <- Some propPlayer;
      remove_owner propPlayer;
      assert_equal None iAve.owner );
    ( "Checking second property after removing the owner of three properties"
    >:: fun _ -> assert_equal None bRail.owner );
    ( "Checking third property after removing the owner of three properties"
    >:: fun _ -> assert_equal None aAve.owner );
    (* jail *)
    ( "Checking a player's jail turns after putting them in jail" >:: fun _ ->
      execute_jail propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 3 propPlayer.jail_turns
    );
    ( "Checking a player's board position after putting them in jail"
    >:: fun _ ->
      execute_jail propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 10 propPlayer.position );
    ( "Checking if a player can pay for jail after being in jail for < 3 turns"
    >:: fun _ ->
      execute_jail propPlayer;
      pay_jail propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 800 propPlayer.money );
    ( "Checking if a player can pay for jail after being in jail for < 3 turns \
       (Insufficient funds case)"
    >:: fun _ ->
      execute_jail propPlayer;
      propPlayer.money <- 30;
      pay_jail propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 30 propPlayer.money );
    (* calculate value *)
    ( "Adding the value of all the properties owned by PropPlayer" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> string_of_int x)
        700 (calculate_value props) );
    ( "Adding the value of all the properties owned by a player when they own \
       nothing"
    >:: fun _ ->
      bluePlayer.properties <- [];
      assert_equal
        ~printer:(fun x -> string_of_int x)
        0
        (calculate_value bluePlayer.properties) );
    ( "Adding the value of all the properties owned by a player when they own \
       only 1 property"
    >:: fun _ ->
      bluePlayer.properties <- [ List.nth getBoard 37 ];
      assert_equal
        ~printer:(fun x -> string_of_int x)
        350
        (calculate_value bluePlayer.properties) );
    (* tax *)
    ( "Checking income tax for a propPlayer with 3 properties (Non-bankruptcy \
       case)"
    >:: fun _ ->
      let game = create_test_game () in
      propPlayer.money <- 1000;
      execute_income_tax propPlayer game;
      assert_equal ~printer:(fun x -> string_of_int x) 930 propPlayer.money );
    ( "Checking luxury tax for propPlayer (Non-bankruptcy case)" >:: fun _ ->
      let game = create_test_game () in
      propPlayer.money <- 1000;
      execute_luxury_tax propPlayer game;
      assert_equal ~printer:(fun x -> string_of_int x) 900 propPlayer.money );
    (* rent *)
    ( "Checking if a player pays another player rent (Non-bankruptcy case)"
    >:: fun _ ->
      let game = create_test_game () in
      propPlayer.money <- 1000;
      iAve.owner <- Some bluePlayer;
      execute_rent game iAve propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 980 propPlayer.money );
    ( "Checking if a player pays another player double rent (Non-bankruptcy \
       case)"
    >:: fun _ ->
      let game = create_test_game () in
      propPlayer.money <- 1000;
      bluePlayer.properties <- [ List.nth getBoard 37; List.nth getBoard 39 ];
      let a = List.nth getBoard 39 in
      let b = List.nth getBoard 37 in
      b.owner <- Some bluePlayer;
      a.owner <- Some bluePlayer;
      execute_rent game a propPlayer;
      assert_equal ~printer:(fun x -> string_of_int x) 900 propPlayer.money );
    (* game over *)
    ( "Checking game over for test game and Player 1" >:: fun _ ->
      let game = create_test_game () in
      game_over player1 game;
      assert_equal
        ~printer:(fun x -> string_of_int x)
        1 (List.length game.players) );
  ]

let suite = "Monopoly Tests" >::: List.flatten [ individual_functions_test ]
let _ = run_test_tt_main suite
