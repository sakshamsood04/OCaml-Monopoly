open Printf
open Monopoly

let main () =
  let () = printf "Welcome to Monopoly!\n" in
  let () = printf "Enter the number of players (2-4): " in
  let num_players = int_of_string (read_line ()) in
  if num_players < 2 || num_players > 4 then
    printf "Invalid number of players. Please choose 2 to 4 players.\n"
  else
    let player_names = Game.get_player_names num_players in
    let game = Game.initialize_game num_players player_names in
    Game.game_loop game

let () = main ()
