# Cluedo in Prolog

A simple implementation of the game <a href="https://en.wikipedia.org/wiki/Cluedo">Cluedo</a> in Prolog.

<img src="cluedo.gif" height="350">

## Instructions

Tested with <a href="http://www.swi-prolog.org">SWI-Prolog</a>.

### Start the Game

    ?- [cluedo].
    ?- start.

### Make a Suggestion

    ?- make_suggestion("Colonel Mustard", "Knife", "Kitchen").

### Open the Envelope

    ?- open_envelope.
