/* CLUEDO */

:- dynamic die_value/1, i_am/1, i_am_at/1, at/2, player_card/2, left/2, current_player/1.   /* Needed by SWI-Prolog. */
:- retractall(die_value(_)), 
   retractall(arrangement(_)),
   retractall(i_am(_)),
   retractall(i_am_at(_)),
   retractall(player_card(_,_)),
   retractall(dealt(_)),
   retractall(at(_,_)),
   retractall(to_the_left(_)),
   retractall(to_the_right(_)),
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
room("Outside"). /* this one is special */

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

where_is(S) :-
                 suspect(S),
				 at(S, L),
				 room(L),
				 write(S),
				 write(" is in the "),
				 write(L).

where_is(W) :-
                 weapon(W),
				 at(W, L),
				 room(L),
				 write("The "),
				 write(W),
				 write(" is in the "),
				 write(L).
		   
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

open_envelope :-
                      murderer(M),
					  string_upper(M, Mu),
					  murder_weapon(W),
					  string_upper(W, Wu),
					  murder_location(L),
					  string_upper(L, Lu),
                      write("The crime was committed by "),
                      write(Mu), 
                      write(", in the "),
                      write(Lu),
                      write(", with a "),
                      writeln(Wu).

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
         write("Moving "),
         write(X),
         write(" to the "),
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

show_cards(S) :-
           findall(C, player_card(S, C), PlayerCards),
           nl,
           write("Cards("),
		   write(S),
		   writeln(": "),
           printlist(PlayerCards).	
			
show_cards :-
           i_am(P),
           findall(C, player_card(P, C), PlayerCards),
           nl,
           writeln("Cards: "),
           printlist(PlayerCards).
		   
to_the_left(X, Y) :-
           suspect(X),
		   suspect(Y),
		   X \= Y,
		   arrangement(Z),
		   findall(S, suspect(S), Suspects),
		   length(Suspects, Size),
		   nth0(N, Z, Elem1),
		   NP2 is mod(N+1, Size),
		   nth0(NP2, Z, Elem2),
		   X == Elem1,
		   Y == Elem2.

to_the_right(X, Y) :-
           to_the_left(Y, X).

disprove_turn(X) :-
               \+ current_player(X),
               (suggestion(Y, _, _);suggestion(_, Y, _);suggestion(_, _, Y)),
               player_card(X, Y),
               write(X),
               write(" has the "),
               write(Y),
               writeln(" card").

disprove_turn(X) :-
               to_the_left(X, Y),
               disprove_turn(Y).

disprove_turn(X) :-
               to_the_left(X, Y),
               retractall(current_player(_)),
               assert(current_player(Y)).

random_turn :-
                  current_player(X),
                  \+ i_am(X),
				  roll_die,
				  die_value(D),
				  (D > 3, 
				  findall(R, (room(R)), AvailableRooms), 
				  random_member(SelectedRoom, AvailableRooms), 
				  move(X, SelectedRoom)); 
				  (to_the_left(Y, X), retractall(current_player(_)), assert(current_player(Y)), random_turn);
				  writeln("not implemented"),
				  true.

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
	  retractall(arrangement(_)),
      retractall(i_am(_)),
      retractall(i_am_at(_)),
      retractall(dealt(_)),
      retractall(player_card(_,_)),
      retractall(at(_,_)),
      retractall(to_the_left(_)),
	  retractall(to_the_right(_)),
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
