(** Type representing the different types of cards *)
type card_typ =
  | Go
  | Jail
  | Property
  | Chance
  | Community_chest
  | ITax
  | LTax
  | Free
  | GotoJail

(** Type representing the different colors of cards *)
type card_col =
  | Brown
  | Red
  | Green
  | Blue
  | White
  | Sky
  | Pink
  | Black
  | Orange
  | Yellow
  | Utility

type card = {
  name : string;  (** Name of card *)
  cost : int;  (** Cost of card *)
  mutable owner : player option;  (** Owner of card *)
  color : card_col;  (** Color of card *)
  mutable rent : int;  (** Rent of card *)
  typ : card_typ;  (** Type of card *)
}
(** Type representing a card *)

and player = {
  name : string;  (** Name of player *)
  mutable money : int;  (** Amount of money player has *)
  mutable properties : card list;  (** List of properties owned by player *)
  mutable position : int;  (** Current position of player on the board *)
  mutable jail_turns : int;  (** Number of turns player has spent in jail *)
}
(** Type representing a player *)

type game = {
  board : card list;  (** List of all cards on the Monopoly board *)
  mutable players : player list;  (** List of players in the game *)
  mutable current_player : int;  (** Index of current player *)
}
(** Type representing the game state *)

type chance_card = {
  message : string;  (** Message describing contents of the chance card *)
  change : int;  (** Amount of money change associated with the chance card *)
}
(** Type representing a chance card *)

type cc_card = {
  message : string;
      (** Message describing contents of the community chest card *)
  change : int;
      (** Amount of money change associated with the community chest card *)
}
(** Type representing a community chest card *)

val color_collection : card_col -> int
(** [color_collection c] returns the number of property cards of a given color
    [c]. *)

val string_of_col : card_col -> string
(** [string_of_col c] returns the string representation of a color [c]. *)

val string_of_owner : card -> string
(** [string_of_owner c] returns the name of the owner of the card [c]. *)

val roll_dice : unit -> int
(** [roll_dice ()] simulates rolling two six-sided dice and returns the sum,
    while incorporating Monopoly rules for appropriate behaviours when the same
    number is rolled on two dice. *)

val getBoard : card list
(** [getBoard] returns the list of cards on the board. *)

val initialize_game : int -> string list -> game
(** [initialize_game num_players player_names] initializes a new game with
    specified number of players and player names. *)

val get_player_names : int -> string list
(** [get_player_names num_players] prompts the user to enter names for each
    player and returns a list of player names. *)

val display_player_info : player -> unit
(** [display_player_info p] prints information about the player [p]. *)

val display_card : card -> unit
(** [display_card c] prints card [c] information. *)

val execute_go : player -> unit
(** [execute_go game cplayer] executes the "Go" card action for the player
    [cplayer] in the game [game]. *)

val check_collection : player -> card_col -> bool
(** [check_collection p game c] checks if the player [p] owns all the properties
    of a given color [c] in the game [game]. *)

val remove_owner : player -> unit
(** [remove_owner p] removes ownership of all properties owned by the player
    [p]. *)

val game_over : player -> game -> unit
(** [game_over p g] handles the game over condition for player [p] in the game
    [g]. *)

val sell : player -> unit
(** [sell p] is a helper for sell_properties and allows the player [p] to sell
    properties. The user is required to enter the exact name of the property
    when asked otherwise the game will end. The user is also required to input
    "done" when asked otherwise the game will end.*)

val sell_properties : player -> unit
(** [sell_properties p] prompts the player [p] to sell properties. *)

val check_bankruptcy : player -> game -> int -> unit
(** [check_bankruptcy player game amount] checks if the player [player] is
    bankrupt in the game [game] considering that the specified amount [amount]
    will be deducted. *)

val execute_rent : game -> card -> player -> unit
(** [execute_rent game property tenant] executes the rent payment for the player
    [tenant] on the property [property] in the game [game]. *)

val execute_jail : player -> unit
(** [execute_jail p] executes the jail action for the player [p]. *)

val chance_cards : chance_card list
(** [chance_cards] is a list of chance cards with associated messages and money
    changes. *)

val community_chest_cards : cc_card list
(** [community_chest_cards] is a list of community chest cards with associated
    messages and money changes. *)

val execute_chance : player -> game -> unit
(** [execute_chance p game] executes the chance card action for the player [p]
    in the game [game]. *)

val execute_community_chest : player -> game -> unit
(** [execute_community_chest p game] executes the community chest card action
    for the player [p] in the game [game]. *)

val calculate_value : card list -> int
(** [calculate_value lst] calculates the total cost of a list of cards. *)

val execute_income_tax : player -> game -> unit
(** [execute_income_tax p game] executes the income tax action for the player
    [p] in the game [game]. *)

val execute_luxury_tax : player -> game -> unit
(** [execute_luxury_tax p game] executes the luxury tax action for the player
    [p] in the game [game]. *)

val execute_purchase : game -> player -> card -> unit
(** [execute_purchase game cplayer current_card] handles the purchase of a
    property if unowned by the player [cplayer] in the game [game]. If the
    property is already owned then a message is printed.*)

val play : game -> player -> int -> unit
(** [play game cplayer steps] simulates a player [cplayer] taking a turn in the
    game [game] and move a specified number of [steps]. *)

val pay_jail : player -> unit
(** [pay_jail p] allows the player [p] to pay 200 to get out of jail. *)

val take_turn : game -> unit
(** [take_turn game] simulates a player taking their turn in the game [game].
    When prompted for input, if the mentioned commands are not used then the
    game will end, for instance, when asked to type roll to roll the dice, if
    anything else is given as input the game will end.*)

val game_loop : game -> unit
(** [game_loop game] simulates the main game loop for the game [game]. *)
