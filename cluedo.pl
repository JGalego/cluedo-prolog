/* RULES */

:- dynamic die_value/1, i_am/1, i_am_at/1, chosen/1, player/3, at/2.   /* Needed by SWI-Prolog. */
:- retractall(die_value(_)), 
   retractall(i_am(_)),
   retractall(i_am_at(_)), 
   retractall(chosen(_)), 
   retractall(player(_,_,_)),
   retractall(piece_player_pair(_,_)),
   retractall(picked_card(_)),
   retractall(picked_piece(_)),
   retractall(picked_player(_)),
   retractall(at(_,_)),
   retractall(murderer(_)), 
   retractall(murder_weapon(_)), 
   retractall(murder_location(_)).

/* Suspects */
suspect("Miss Scarlett"). 	/* red piece */
suspect("Professor Plum").	/* purple piece */
suspect("Mrs. Peacock").	/* blue piece */
suspect("Reverend Mr. Green").	/* green piece */
suspect("Colonel Mustard").	/* yellow piece */
suspect("Mrs. White").		/* white piece */

/* Weapons */
weapon("Candlestick").
weapon("Dagger").
weapon("Lead Pipe").
weapon("Revolver").
weapon("Rope").
weapon("Spanner").

/* Rooms */
room("Kitchen").
room("Ballroom").
room("Conservatory").
room("Dining Room").
room("Cellar"). /* where the envelope is */
room("Billiard Room").
room("Library").
room("Lounge").
room("Hall").
room("Study").

/* Secret Passages */
secret_passage("Kitchen", "Study").
secret_passage("Conversatory", "Lounge").
secret_passage(X,Y) :- secret_passage(Y,X).

/* Dice */
die_value(0).

who_am_i :-
           i_am(X),
           write("You are "),
           writeln(X).

where_am_i :-
           i_am_at(X),
           write("You are in the "),
           writeln(X).

where_am_i :-
           writeln("You are nowhere to be found!").

goto(X) :-
         room(X),
         i_am_at(X),
         write("You are already inside the "),
         writeln(X).

goto(X) :-
         room(X),
         \+ i_am_at(X),
         retract(i_am_at(_)),
         assert(i_am_at(X)),
         write("You are now inside the "),
         writeln(X).

goto(X) :-
         room(X),
         \+ i_am_at(X),
         assert(i_am_at(X)),
         write("You are now inside the "),
         writeln(X).

goto(X) :-
         \+ room(X),
         writeln("Invalid room!").

enter_secret_passage :-
                     i_am_at(X),
                     secret_passage(X,Y),
                     writeln("Entering the secret passage"),
                     goto(Y).

enter_secret_passage :-
                     writeln("This room has no secret passage").

look_around :-
           i_am_at(X),
           findall(Y, at(Y,X), Found),
           write("This is what's in the "),
           write(X),
           write(": "),
           writeln(Found).

roll_die :-
           Value is random(5)+1,
           write("The die landed on "),
           writeln(Value),
           retract(die_value(_)),
           assert(die_value(Value)).

random_player :-
              findall(X, (suspect(X), \+ picked_card(X)), Suspects),
              findall(Y, (weapon(Y), \+ picked_card(Y)), Weapons),
              findall(Z, (room(Z), \+ picked_card(Z)), Rooms),
              random_member(SelectedSuspect, Suspects),
              random_member(SelectedWeapon, Weapons),
              random_member(SelectedRoom, Rooms),
              assert(picked_card(SelectedSuspect)),
              assert(picked_card(SelectedWeapon)),
              assert(picked_card(SelectedRoom)),
              assert(player(SelectedSuspect, SelectedWeapon, SelectedRoom)).

random_player_configuration :-
              random_player,
              random_player_configuration;
              true.

set_user :-
         \+ i_am(_),
         findall(X, suspect(X), Suspects),
         random_member(SelectedSuspect, Suspects),
         assert(i_am(SelectedSuspect)),
         who_am_i.

set_envelope :-
              findall(X, (suspect(X), \+ picked_card(X)), Suspects),
              findall(Y, (weapon(Y), \+ picked_card(Y)), Weapons),
              findall(Z, (room(Z), \+ picked_card(Z)), Rooms),
              writeln("Selecting murderer"),
              random_member(Murderer, Suspects),
              writeln("Selecting murder weapon"),
              random_member(MurderWeapon, Weapons),
              writeln("Selecting murder location"),
              random_member(MurderLocation, Rooms),
              assert(murderer(Murderer)),
              assert(murder_weapon(MurderWeapon)),
              assert(murder_location(MurderLocation)).

place(X, Y) :-
         weapon(X),
         room(Y),
	 assert(at(X, Y)),
         write("Placing the "),
         write(X),
         write(" inside the "),
         writeln(Y).

place(X, Y) :-
         suspect(X),
         room(Y),
	 assert(at(X, Y)),
         write("Placing "),
         write(X),
         write(" inside the "),
         writeln(Y).

place(_, _) :-
         writeln("Invalid placement!").

show_cards :-
           i_am(P),
           piece_player_pair(P, S),
           player(S, W, L),
           writeln(""),
           write("Suspect: "),
           writeln(S),
           write("Weapon: "),
           writeln(W),
           write("Location: "),
           writeln(L),
           writeln(""). 

generate_piece_configurations :-
                              findall(X, (suspect(X), \+ picked_piece(X)), Pieces),
                              findall(X, (player(X,_,_), \+ picked_player(X)), Players),
                              random_member(Piece, Pieces),
                              random_member(Player, Players),
                              assert(picked_piece(Piece)),
                              assert(picked_player(Player)),
                              assert(piece_player_pair(Piece, Player)),
                              generate_piece_configurations;
                              true. 

reset :-
      retractall(die_value(_)), 
      retractall(i_am(_)),
      retractall(i_am_at(_)), 
      retractall(chosen(_)), 
      retractall(player(_,_,_)),
      retractall(piece_player_pair(_,_)),
      retractall(picked_card(_)),
      retractall(picked_piece(_)),
      retractall(picked_player(_)),
      retractall(at(_,_)),
      retractall(murderer(_)), 
      retractall(murder_weapon(_)), 
      retractall(murder_location(_)),
      write('\e[H\e[2J').

splash_screen :-
             writeln("    ________               __     "),
             writeln("   / ____/ /_  _____  ____/ /___  "),
             writeln("  / /   / / / / / _ \\/ __  / __ \\ "),
             writeln(" / /___/ / /_/ /  __/ /_/ / /_/ / "),
             writeln(" \\____/_/\\__,_/\\___/\\__,_/\\____/  "),
             writeln(""),
             writeln("Shuffling cards").

start :-
      reset,
      splash_screen,
      set_envelope,
      random_player_configuration,
      generate_piece_configurations,
      set_user,
      show_cards.

