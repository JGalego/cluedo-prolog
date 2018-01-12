# Cluedo in Prolog

A simple implementation of the game <a href="https://en.wikipedia.org/wiki/Cluedo">Cluedo</a> in Prolog.

<img src="cluedo.gif" height="350">

## Instructions

Tested with <a href="http://www.swi-prolog.org">SWI-Prolog</a>.

### Start the Game

	?- [cluedo].
	?- start.

	    ________               __          
	   / ____/ /_  _____  ____/ /___       
	  / /   / / / / / _ \/ __  / __ \    
	 / /___/ / /_/ /  __/ /_/ / /_/ /      
	 \____/_/\__,_/\___/\__,_/\____/  

	Shuffling cards
	Selecting murderer
	Selecting murder weapon
	Selecting murder location
	Placing the Candlestick inside the Study
	Placing the Dagger inside the Kitchen
	Placing the Lead Pipe inside the Dining Room
	Placing the Revolver inside the Ballroom
	Placing the Rope inside the Hall
	Placing the Spanner inside the Billiard Room
	Weapons in place

	Player Arrangement: 
	    + Reverend Mr. Green
	    + Mrs. Peacock
	    + Mrs. White
	    + Miss Scarlett
	    + Professor Plum
	    + Colonel Mustard

	You are MRS. PEACOCK

	Cards: 
	    + Professor Plum
	    + Library
	    + Lounge

	Press any key to continue.

### Look Around Rooms

	?- goto("Study"), look_around.

	You are now inside the Study
	This is what's in the Study: 
	    + Candlestick

### Make a Suggestion

	?- make_suggestion("Colonel Mustard", "Dagger").
	
	Moving Colonel Mustard to the Kitchen
	Placing the Dagger inside the Kitchen
	You suggested that the crime was committed by Colonel Mustard, in the Kitchen, with a Dagger

### Random Turn

	?- next_player, random_turn.

	+ Mrs. White's turn

	The die landed on 4
	Moving Mrs. White to the Conservatory
	The die landed on 2
	Skipping suggestion

	+ Miss Scarlett's turn

	The die landed on 3
	The player is not allowed to move

	+ Professor Plum's turn

	The die landed on 1
	The player is not allowed to move

	+ Colonel Mustard's turn

	The die landed on 5
	Moving Colonel Mustard to the Dining Room
	The die landed on 4
	Moving Mrs. White to the Kitchen
	Placing the Spanner inside the Kitchen
	You suggested that the crime was committed by Mrs. White, in the Kitchen, with a Spanner
	Reverend Mr. Green has the Kitchen card

	+ Colonel Mustard's turn

	The die landed on 3
	The player is not allowed to move

	+ Reverend Mr. Green's turn

	The die landed on 3
	The player is not allowed to move

	Random turn ended - Make your move

### Open the Envelope

    	?- open_envelope.
	The crime was committed by REVEREND MR. GREEN, in the STUDY, with a CANDLESTICK
