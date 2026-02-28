## GuessCountry - A centralized web app (multiplayer game)

Extension project for the WebApp lab of the Software Construction course (CS-214) at Ecole Polytechnique Fédérale de Lausanne (EPFL), Switzerland.

### Architecture & Technologies
- Scala
- Server-side state machine architecture
- Client-server event-based communication

### Overview

GuessCountry is a multiplayer game inspired by [Wordle](https://en.wikipedia.org/wiki/Wordle) where players take turns guessing a mystery country. Unlike *Wordle*, the number of letters of this country is unknown.

The web app functions as a state machine where clients initiate events and send them to the server. The server receives these events and performs the corresponding state transitions, i.e. update the state. Only the server maintains any state.

The tests have also been written.

### Implementation

- Implemented the state machine, event and view handling, transition logic using Scala.
- Used the `ujson` library for serializing/deserializing events and views.
- Created the user interface using Scala.js and CSS.
- Written the state machine tests using WebappTest (pre-defined).

### Rules

- Players take turns guessing the mystery country.
- Players have all together a limited number of attempts.
- Green letters indicate correct letters in the correct position, yellow letters indicate correct letters in the wrong position, and red letters indicate incorrect letters.
- Players can request a hint. It will show the current player, for a certain amount of time, a list of all the countries in the world. - When a player read a hint, their next oppenent gets one additional turn. 
- If a player reads the hint while having a second turn, they lose their advantage.

### How to Play

1. When it's your turn, guess a country using the keyboard or chose wisely to ask for a hint.
2. Pay attention to the color-coded feedback for your guess.
3. The first to guess the country wins.

### I am responsible for

- `GuessCountryTypes.scala`
- `GuessCountryLogic.scala`
- `GuessCountryWire.scala`
- `GuessCountryUI.scala`
- `GuessCountryTest.scala`
- `main.css` part where only elements of the game's UI are modified

Everything else is the work of the CS-214 teaching staff.
