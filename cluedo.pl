/* RULES */

:- dynamic die_value/1, i_am/1, i_am_at/1, at/2, player_card/2, left/2.   /* Needed by SWI-Prolog. */
:- retractall(die_value(_)), 
   retractall(i_am(_)),
   retractall(i_am_at(_)),
   retractall(player_card(_,_)),
   retractall(dealt(_)),
   retractall(at(_,_)),
   retractall(left(_)),
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

card(X) :-
        suspect(X);
        weapon(X);
        room(X).

who_am_i :-
           i_am(X),
           write("You are "),
           string_upper(X, Xu),
           writeln(Xu).

where_am_i :-
           i_am_at(X),
           write("You are in the "),
           string_upper(X, Xu),
           writeln(Xu).

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

deal_cards :-
              findall(P, (suspect(P), findall(C, player_card(P, C), PlayerCards), \+ length(PlayerCards, 3)), Players),
              findall(X, (card(X), \+ dealt(X)), Cards),
              random_member(Player, Players),
              random_member(Card, Cards),
              assert(dealt(Card)),
              assert(player_card(Player, Card)),
	      deal_cards;
              true.

set_user :-
         \+ i_am(_),
         findall(X, suspect(X), Suspects),
         random_member(SelectedSuspect, Suspects),
         assert(i_am(SelectedSuspect)),
         nl,
         who_am_i.

set_envelope :-
              findall(X, (suspect(X), \+ dealt(X)), Suspects),
              findall(Y, (weapon(Y), \+ dealt(Y)), Weapons),
              findall(Z, (room(Z), \+ dealt(Z)), Rooms),
              writeln("Selecting murderer"),
              random_member(Murderer, Suspects),
              writeln("Selecting murder weapon"),
              random_member(MurderWeapon, Weapons),
              writeln("Selecting murder location"),
              random_member(MurderLocation, Rooms),
              assert(murderer(Murderer)),
              assert(dealt(Murderer)),
              assert(murder_weapon(MurderWeapon)),
              assert(dealt(MurderWeapon)),
              assert(murder_location(MurderLocation)),
              assert(dealt(MurderLocation)).

place(X, Y) :-
         weapon(X),
         room(Y),
         retractall(at(X, _)),
	 assert(at(X, Y)),
         write("Placing the "),
         write(X),
         write(" inside the "),
         writeln(Y).

place(X, Y) :-
         suspect(X),
         room(Y),
         retractall(at(X, _)),
	 assert(at(X, Y)),
         write("Placing "),
         write(X),
         write(" inside the "),
         writeln(Y).

place(_, _) :-
         writeln("Invalid placement!").

place_weapons :-
              weapon(W),
              \+ at(W, _),
              findall(R, (room(R), findall(X, (weapon(X), at(X, R)), FoundWeapons), \+ length(FoundWeapons, 1)), Rooms),
              random_member(SelectedRoom, Rooms),
              place(W, SelectedRoom),
              place_weapons;
              writeln("Weapons in place"),
              true.

arrange_players :-
                findall(P, suspect(P), Players),
                random_permutation(Players, PlayerArrangement),
                assert(arrangement(PlayerArrangement)),
                nl,
                writeln("Player Arrangement: "),
                printlist(PlayerArrangement).

show_cards :-
           i_am(P),
           findall(C, player_card(P, C), PlayerCards),
           nl,
           writeln("Cards: "),
           printlist(PlayerCards).

left(_, _) :-
           writeln("Not yet implemented").

disprove_turn(X) :-
               \+ current_player(X),
               (suggestion(Y, _, _);suggestion(_, Y, _);suggestion(_, _, Y)),
               player_card(X, Y),
               write(X),
               write(" has the "),
               write(Y),
               writeln(" card").

disprove_turn(X) :-
               left(X, Y),
               \+ current_player(Y),
               disprove_turn(Y).

disprove_turn(X) :-
               left(X, Y),
               retractall(current_player(_)),
               assert(current_player(Y)).

printlist([]).
    
printlist([X|List]) :-
        write("+ "),
        writeln(X),
        printlist(List).

len([], LenResult):-
    LenResult is 0.

len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

make_suggestion(S, W) :-
                         i_am_at(L),
                         place(S, L),
                         place(W, L),
                         retractall(suggestion(_,_,_)),
                         assert(suggestion(S, W, L)),
                         write("You suggested that the crime was committed by "),
                         write(S), 
                         write(", in the "),
                         write(L),
                         write(", with a "),
                         writeln(W).

make_suggestion(S, W, L) :-
                         i_am_at(L),
                         place(S, L),
                         place(W, L),
                         retractall(suggestion(_,_,_)),
                         assert(suggestion(S, W, L)),
                         write("You suggested that the crime was committed by "),
                         write(S), 
                         write(", in the "),
                         write(L),
                         write(", with a "),
                         writeln(W).

reset :-
      retractall(die_value(_)),
      retractall(i_am(_)),
      retractall(i_am_at(_)),
      retractall(dealt(_)),
      retractall(player_card(_,_)),
      retractall(at(_,_)),
      retractall(left(_)),
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
             nl,
             writeln("Shuffling cards").

start :-
      reset,
      splash_screen,
      set_envelope,
      deal_cards,
      place_weapons,
      arrange_players,
      set_user,
      show_cards,
      nl.

