open Printf

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
  name : string;
  cost : int;
  mutable owner : player option;
  color : card_col;
  mutable rent : int;
  typ : card_typ;
}

and player = {
  name : string;
  mutable money : int;
  mutable properties : card list;
  mutable position : int;
  mutable jail_turns : int;
}

type game = {
  board : card list;
  mutable players : player list;
  mutable current_player : int;
}

type chance_card = {
  message : string;
  change : int;
}

type cc_card = {
  message : string;
  change : int;
}

let color_collection (c : card_col) =
  match c with
  | Brown -> 2
  | Sky -> 3
  | Red -> 3
  | Green -> 3
  | Blue -> 2
  | White -> 0
  | Pink -> 3
  | Black -> 4
  | Orange -> 3
  | Yellow -> 3
  | Utility -> 2

let string_of_col (c : card_col) =
  match c with
  | Brown -> "Brown"
  | Sky -> "Sky Blue"
  | Red -> "Red"
  | Green -> "Green"
  | Blue -> "Blue"
  | White -> "White"
  | Pink -> "Pink"
  | Black -> "Black"
  | Orange -> "Orange"
  | Yellow -> "Yellow"
  | Utility -> "Utility"

let string_of_owner (c : card) : string =
  match c.owner with
  | Some c -> c.name
  | None -> "None"

let roll_dice () =
  Random.self_init ();
  let dice = [ 1; 2; 3; 4; 5; 6 ] in
  let dice1 = List.nth dice (Random.int (List.length dice)) in
  let dice2 = List.nth dice (Random.int (List.length dice)) in
  let turn1 = dice1 + dice2 in
  if dice1 = dice2 then
    let dice3 = List.nth dice (Random.int (List.length dice)) in
    let dice4 = List.nth dice (Random.int (List.length dice)) in
    let turn2 = turn1 + dice3 + dice4 in
    if dice3 = dice4 then
      let dice5 = List.nth dice (Random.int (List.length dice)) in
      let dice6 = List.nth dice (Random.int (List.length dice)) in
      if dice5 = dice6 then 0 else turn2 + dice5 + dice6
    else turn2
  else turn1

let getBoard : card list =
  [
    { name = "Go"; cost = 0; owner = None; color = White; rent = 0; typ = Go };
    {
      name = "Mediterranean Avenue";
      cost = 60;
      owner = None;
      color = Brown;
      rent = 2;
      typ = Property;
    };
    {
      name = "Community Chest";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Community_chest;
    };
    {
      name = "Baltic Avenue";
      cost = 60;
      owner = None;
      color = Brown;
      rent = 4;
      typ = Property;
    };
    {
      name = "Income Tax";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = ITax;
    };
    {
      name = "Reading Railroad";
      cost = 200;
      owner = None;
      color = Black;
      rent = 25;
      typ = Property;
    };
    {
      name = "Oriental Avenue";
      cost = 100;
      owner = None;
      color = Sky;
      rent = 6;
      typ = Property;
    };
    {
      name = "Chance";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Chance;
    };
    {
      name = "Vermont Avenue";
      cost = 100;
      owner = None;
      color = Sky;
      rent = 6;
      typ = Property;
    };
    {
      name = "Connecticut Avenue";
      cost = 120;
      owner = None;
      color = Sky;
      rent = 8;
      typ = Property;
    };
    {
      name = "Jail";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Jail;
    };
    {
      name = "St. Charles Place";
      cost = 140;
      owner = None;
      color = Pink;
      rent = 10;
      typ = Property;
    };
    {
      name = "Electric Company";
      cost = 150;
      owner = None;
      color = Utility;
      rent = 30;
      typ = Property;
    };
    {
      name = "States Avenue";
      cost = 140;
      owner = None;
      color = Pink;
      rent = 10;
      typ = Property;
    };
    {
      name = "Virginia Avenue";
      cost = 160;
      owner = None;
      color = Pink;
      rent = 12;
      typ = Property;
    };
    {
      name = "Pennsylvania Railroad";
      cost = 200;
      owner = None;
      color = Black;
      rent = 25;
      typ = Property;
    };
    {
      name = "St. James Place";
      cost = 180;
      owner = None;
      color = Orange;
      rent = 14;
      typ = Property;
    };
    {
      name = "Community Chest";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Community_chest;
    };
    {
      name = "Tennessee Avenue";
      cost = 180;
      owner = None;
      color = Orange;
      rent = 14;
      typ = Property;
    };
    {
      name = "New York Avenue";
      cost = 200;
      owner = None;
      color = Orange;
      rent = 16;
      typ = Property;
    };
    {
      name = "Free Parking";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Free;
    };
    {
      name = "Kentucky Avenue";
      cost = 220;
      owner = None;
      color = Red;
      rent = 18;
      typ = Property;
    };
    {
      name = "Chance";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Chance;
    };
    {
      name = "Indiana Avenue";
      cost = 220;
      owner = None;
      color = Red;
      rent = 18;
      typ = Property;
    };
    {
      name = "Illinois Avenue";
      cost = 240;
      owner = None;
      color = Red;
      rent = 20;
      typ = Property;
    };
    {
      name = "B&O Railroad";
      cost = 200;
      owner = None;
      color = Black;
      rent = 25;
      typ = Property;
    };
    {
      name = "Atlantic Avenue";
      cost = 260;
      owner = None;
      color = Yellow;
      rent = 22;
      typ = Property;
    };
    {
      name = "Ventnor Avenue";
      cost = 260;
      owner = None;
      color = Yellow;
      rent = 22;
      typ = Property;
    };
    {
      name = "Water Works";
      cost = 150;
      owner = None;
      color = Utility;
      rent = 15;
      typ = Property;
    };
    {
      name = "Marvin Gardens";
      cost = 280;
      owner = None;
      color = Yellow;
      rent = 24;
      typ = Property;
    };
    {
      name = "Go To Jail";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = GotoJail;
    };
    {
      name = "Pacific Avenue";
      cost = 300;
      owner = None;
      color = Green;
      rent = 26;
      typ = Property;
    };
    {
      name = "North Carolina Avenue";
      cost = 300;
      owner = None;
      color = Green;
      rent = 26;
      typ = Property;
    };
    {
      name = "Community Chest";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Community_chest;
    };
    {
      name = "Pennsylvania Avenue";
      cost = 320;
      owner = None;
      color = Green;
      rent = 28;
      typ = Property;
    };
    {
      name = "Short Line";
      cost = 200;
      owner = None;
      color = Black;
      rent = 25;
      typ = Property;
    };
    {
      name = "Chance";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = Chance;
    };
    {
      name = "Park Place";
      cost = 350;
      owner = None;
      color = Blue;
      rent = 35;
      typ = Property;
    };
    {
      name = "Luxury Tax";
      cost = 0;
      owner = None;
      color = White;
      rent = 0;
      typ = LTax;
    };
    {
      name = "Boardwalk";
      cost = 400;
      owner = None;
      color = Blue;
      rent = 50;
      typ = Property;
    };
  ]

let initialize_game num_players player_names =
  let initial_player_info name =
    { name; money = 1500; properties = []; position = 0; jail_turns = 0 }
  in
  let players = List.map initial_player_info player_names in
  let board = getBoard in
  { board; players; current_player = 0 }

let rec get_player_names num_players =
  if num_players <= 0 then []
  else
    let () = printf "Enter the name for Player %d: " num_players in
    let name = read_line () in
    name :: get_player_names (num_players - 1)

let display_player_info (p : player) =
  print_endline "";
  printf "Player: %s\n" p.name;
  printf "Money in Bank: %d\n" p.money;
  printf "Properties Owned: %s\n"
    (String.concat ", " (List.map (fun property -> property.name) p.properties));
  print_endline ""

let display_card (c : card) =
  if c.typ = Go then (
    printf "Card: %s\n" c.name;
    printf "%s\n" "Collect 200";
    print_endline "")
  else if c.typ = GotoJail then (
    printf "Card: %s\n" c.name;
    printf "%s\n" "Go to Jail";
    print_endline "")
  else (
    printf "Card: %s\n" c.name;
    printf "Cost %d\n" c.cost;
    printf "Owner: %s\n" (string_of_owner c);
    printf "Color: %s\n" (string_of_col c.color);
    printf "Rent: %s\n" (string_of_int c.rent);
    print_endline "")

let execute_go cplayer =
  let cwallet = cplayer.money in
  cplayer.money <- cwallet + 200

let check_collection p c : bool =
  let rec check_cards_owned_by_player properties color acc =
    match properties with
    | [] -> acc
    | card :: rest ->
        if card.color = color then
          check_cards_owned_by_player rest color (acc + 1)
        else check_cards_owned_by_player rest color acc
  in
  let color_count = check_cards_owned_by_player p.properties c 0 in
  if color_count = color_collection c then true else false

let remove_owner (p : player) =
  List.iter (fun property -> property.owner <- None) p.properties;
  print_endline "";
  printf "Properties of %s have no owner now\n" p.name

let game_over (p : player) (g : game) =
  print_endline "";
  printf "%s is bankrupt. Game over for %s!\n" p.name p.name;
  remove_owner p;
  g.players <- List.filter (fun x -> x <> p) g.players;
  ()

let rec sell (p : player) =
  printf "Properties Owned: %s\n"
    (String.concat ", "
       (List.map
          (fun property -> sprintf "%s (Cost: %d)" property.name property.cost)
          p.properties));
  printf "Select a property to sell (type the name or 'done' to finish):";
  let prop_name = String.trim (read_line ()) in
  if String.equal prop_name "done" then ()
  else
    let property = List.find (fun pr -> pr.name = prop_name) p.properties in
    printf "Selling %s for %d." property.name property.cost;
    p.money <- p.money + property.cost;
    property.owner <- None;
    p.properties <- List.filter (fun pr -> pr <> property) p.properties;
    sell p

let sell_properties (p : player) =
  print_endline "";
  printf
    "Would you like to sell some properties? (Type 'yes' for yes or anything \
     else to skip)";
  let ans = String.trim (read_line ()) in
  if String.equal ans "yes" then
    if p.properties = [] then (
      print_endline "";
      printf "%s has no properties to sell" p.name)
    else sell p
  else ()

let check_bankruptcy player game amount =
  if player.money < amount then (
    print_endline "";
    printf "%s is bankrupt." player.name;
    sell_properties player;
    if player.money < amount then game_over player game else ())
  else ()

let execute_rent (game : game) (property : card) (tenant : player) : unit =
  match property.owner with
  | Some owner -> (
      match check_collection owner property.color with
      | true ->
          print_endline "";
          printf "You need to pay rent of %d to %s.\n" (2 * property.rent)
            owner.name;
          let rent_amount = 2 * property.rent in
          check_bankruptcy tenant game rent_amount;
          printf "%s pays rent of %d to %s.\n" tenant.name rent_amount
            owner.name;
          tenant.money <- tenant.money - rent_amount;
          print_endline ""
      | false ->
          print_endline "";
          printf "You need to pay rent of %d to %s.\n" property.rent owner.name;
          let rent_amount = property.rent in
          check_bankruptcy tenant game rent_amount;
          printf "%s pays rent of %d to %s.\n" tenant.name rent_amount
            owner.name;
          tenant.money <- tenant.money - rent_amount;
          owner.money <- owner.money + rent_amount;
          print_endline "")
  | None ->
      failwith
        "Property has no owner, rent cannot be executed on unowned property"

let execute_jail (p : player) =
  print_endline "";
  printf "%s is in jail for 3 turns" p.name;
  print_endline "";
  printf "%s has the option of paying to get out at their next turn" p.name;
  print_endline "";
  p.position <- 10;
  p.jail_turns <- 3

let chance_cards : chance_card list =
  [
    {
      message = "You won the NY Times lottery! You;re winnings are 300";
      change = -300;
    };
    {
      message = "Your car's wheels came off at high speed. You lose 50";
      change = 50;
    };
    {
      message =
        "Your grandmother passed away but you came across inheritance. You \
         gain 100";
      change = -100;
    };
    { message = "You had to go to the doctor! You lose 100"; change = 100 };
    {
      message = "You found a rare collectible and sold it! You gain 200";
      change = -200;
    };
    { message = "A surprise tax refund! You gain 150"; change = -150 };
    { message = "You lost a small court case. You lose 120"; change = 120 };
    {
      message = "You have to pay for emergency home repairs. You lose 200";
      change = 200;
    };
    {
      message = "You were caught for arson, and have been imprisoned";
      change = 0;
    };
    {
      message = "You gave away a lot of money in charity. You lose 175";
      change = 175;
    };
    {
      message = "A long-term investment paid off it's returns. You gain 225";
      change = -225;
    };
    {
      message = "You went all out on a birthday party. You lose 75";
      change = 75;
    };
    {
      message = "You returned your Louis Vuitton purse. You gain 50";
      change = -50;
    };
  ]

let community_chest_cards : cc_card list =
  [
    {
      message = "You were the victim of a house burglary. You lose 400";
      change = 400;
    };
    {
      message = "You found a $100 bill lying around. You win 100";
      change = -100;
    };
    {
      message = "You must pay a tax avoidance fine. You lose 150";
      change = 150;
    };
    {
      message = "You have received a salary increment. You win 250";
      change = -250;
    };
    {
      message =
        "You won a local General Knowledge Quiz competition! You win 200";
      change = -200;
    };
    {
      message =
        "You receive a small gift from a distant relative on Christmas. You \
         win 500";
      change = -500;
    };
    { message = "Your stock investments paid off! You win 350"; change = -350 };
    { message = "You have been fined for speeding. You lose 100"; change = 100 };
    {
      message = "You have been forced to withdraw your 401K. You gained 100";
      change = -100;
    };
    {
      message = "You got demoted at work due to bad performance. You lose 250";
      change = 250;
    };
    {
      message = "You just received your AT&T phone bill. You lose 28";
      change = 28;
    };
  ]

let execute_chance p game =
  let no = Random.int (List.length chance_cards) in
  let curr_card = List.nth chance_cards no in
  print_endline "";
  print_string curr_card.message;
  print_endline "";
  if curr_card.message = "You were caught for arson, and have been imprisoned"
  then execute_jail p
  else (
    check_bankruptcy p game curr_card.change;
    p.money <- p.money - curr_card.change)

let execute_community_chest p game =
  let no = Random.int (List.length community_chest_cards) in
  let curr_card = List.nth community_chest_cards no in
  print_endline "";
  print_string curr_card.message;
  print_endline "";
  check_bankruptcy p game curr_card.change;
  p.money <- p.money - curr_card.change

let rec calculate_value lst =
  match lst with
  | [] -> 0
  | h :: t -> h.cost + calculate_value t

let execute_income_tax (p : player) game =
  print_endline "";
  printf "%s has landed on Income Tax" p.name;
  print_endline "";
  printf "%s"
    (p.name ^ " must pay 10% of the total value for every property owned.");
  let total_value = calculate_value p.properties in
  let tax = int_of_float (0.1 *. float_of_int total_value) in
  print_endline "";
  printf "%n Tax is due" tax;
  check_bankruptcy p game tax;
  p.money <- p.money - tax;
  print_endline ""

let execute_luxury_tax (p : player) game =
  print_endline "";
  printf "%s has landed on Luxury Tax" p.name;
  print_endline "";
  printf "%s" (p.name ^ " must pay 100.");
  print_endline "";
  check_bankruptcy p game 100;
  p.money <- p.money - 100;
  print_endline ""

let execute_purchase game (cplayer : player) current_card =
  match current_card.owner with
  | Some owner ->
      printf "This property is owned by %s.\n" owner.name;
      if not (String.equal owner.name cplayer.name) then
        execute_rent game current_card cplayer
      else printf "You already own this property.\n"
  | None ->
      printf "This property is unowned.\n";
      printf
        "Do you want to buy it? (Type 'buy' to buy or anything else to skip): ";
      let ans = String.trim (read_line ()) in
      if String.equal ans "buy" then
        if cplayer.money >= current_card.cost then (
          printf "%s bought %s for $%d.\n" cplayer.name current_card.name
            current_card.cost;
          cplayer.money <- cplayer.money - current_card.cost;
          current_card.owner <- Some cplayer;
          cplayer.properties <- current_card :: cplayer.properties)
        else printf "Insufficient funds to buy %s.\n" current_card.name
      else printf "%s decided not to buy %s.\n" cplayer.name current_card.name

let play game cplayer steps =
  display_player_info cplayer;

  let old_pos = cplayer.position in
  if old_pos + steps > List.length game.board then (
    print_endline "You crossed the Go Card, earning 200";
    print_endline "";
    execute_go cplayer);
  let new_pos = (old_pos + steps) mod List.length game.board in
  cplayer.position <- new_pos;

  let current_card = List.nth game.board new_pos in
  display_card current_card;
  match current_card.typ with
  | Go -> execute_go cplayer
  | GotoJail -> execute_jail cplayer
  | Chance ->
      printf "%s has landed on a Chance card" cplayer.name;
      execute_chance cplayer game
  | Community_chest ->
      printf "%s has landed on a Community Chest card" cplayer.name;
      execute_community_chest cplayer game
  | ITax -> execute_income_tax cplayer game
  | LTax -> execute_luxury_tax cplayer game
  | Free ->
      printf "You are at Free Parking, no need to do anything!";
      print_endline ""
  | Jail -> print_endline "You are just visiting Jail, no need to do anything!"
  | Property -> execute_purchase game cplayer current_card

let pay_jail (p : player) =
  if p.money - 200 < 0 then printf "Insufficient funds to pay"
  else (
    p.jail_turns <- 0;
    p.money <- p.money - 200)

let take_turn game =
  let current_player = List.nth game.players game.current_player in
  print_endline "";
  printf "It's %s's turn.\n" current_player.name;

  if current_player.jail_turns > 0 then (
    printf
      "Do you want to pay 200 to get out of jail? (Type 'yes' to pay or \
       anything else to skip):";
    let ans = String.trim (read_line ()) in
    if String.equal ans "yes" then pay_jail current_player
    else (
      current_player.jail_turns <- current_player.jail_turns - 1;
      printf "%s in jail for %d more turns" current_player.name
        current_player.jail_turns);
    print_endline "")
  else
    let () =
      printf
        "Do you want to roll the dice (type roll) or exit the game (type \
         anything else)?"
    in
    let ans = String.trim (read_line ()) in
    if String.equal ans "roll" then (
      let dice = roll_dice () in
      printf "Rolling the dice... You got %d!\n" dice;
      play game current_player dice)
    else exit 0

let rec game_loop game =
  if List.length game.players <= 1 then
    printf "Game over. %s wins!\n" (List.hd game.players).name
  else
    let () = take_turn game in
    let next_player_index =
      (game.current_player + 1) mod List.length game.players
    in
    game.current_player <- next_player_index;
    game_loop game
