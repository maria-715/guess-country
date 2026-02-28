package guesscountry

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import guesscountry.*

/**
 * Manages the state transitions and logic for the Guess Country game.
 *
 * This object implements the state machine for the Guess Country game, defining the possible transitions between different game states.
 * It handles the initialization of the game, user actions, and projection of game states to views.
 */
object GuessCountryStateMachine extends cs214.webapp.StateMachine[GuessCountryEvent, GuessCountryState, GuessCountryView]:
  
  val name: String = "guesscountry"
  val wire = GuessCountryWire

  val ALPHABET = Alphabet.ALPHABET
  val COUNTRIES = Countries.COUNTRIES
  val NUMBER_OF_ATTEMPTS = NumberOfAttempts.NUMBER_OF_ATTEMPTS
  val SHOW_HINT_PAUSE_MS = ShowHintPauseMs.SHOW_HINT_PAUSE_MS
  val SHOW_SCORE_PAUSE_MS = ShowScorePauseMs.SHOW_SCORE_PAUSE_MS

  override def init(clients: Seq[UserId]): GuessCountryState =
    val wordToGuess = COUNTRIES.drop(Random.nextInt(COUNTRIES.size)).head
    GuessCountryState(wordToGuess, Board.empty, clients, clients(0), NUMBER_OF_ATTEMPTS, GuessCountryPhaseView.SelectingLetters, 
                      List(), false)
  
  override def transition(state: GuessCountryState)(userId: UserId, event: GuessCountryEvent): Try[Seq[Action[GuessCountryState]]] =
    Try {

      if state.isWordGuessed then
        throw IllegalMoveException("Game is over: the country has already been found!")

      else if state.isNoMoreAvailableTurns then
        throw IllegalMoveException("Game is over: no more available attempts")

      else if state.turn != userId then 
        throw NotYourTurnException()

      else
        event match

          case GuessCountryEvent.GuessSelected => 
            
            val word = state.currSelectedLetters.mkString.reverse
            if COUNTRIES.contains(word) then

              val greenLetters = word.zipWithIndex.toList.zip(state.wordToGuess).filter((e1,e2) => e1._1 == e2).map(e => (e._1)._2)
              val redLetters = word.zipWithIndex.toList.filter((c, _) => !state.wordToGuess.contains(c)).map(_._2)
              val isSameLength = word.length() == state.wordToGuess.length()
              val orangeLetters = word.zipWithIndex.toList.filter((c,i) => (state.wordToGuess.length() <= i || state.wordToGuess(i) != c) && state.wordToGuess.contains(c)).map(_._2)

              val score = (isSameLength, greenLetters, orangeLetters, redLetters)
              val newBoard = (word, score, userId) :: state.wordsGuessed

              val nextTurn = if state.playAgainAfter then state.turn else state.users((state.users.indexOf(userId)+1) % state.users.length)
              List(Action.Render(GuessCountryState(state.wordToGuess, newBoard, state.users, state.turn, state.remainingTurns, GuessCountryPhaseView.WatchingScore, List(), false)),
                  Action.Pause(SHOW_SCORE_PAUSE_MS),
                  Action.Render(GuessCountryState(state.wordToGuess, newBoard, state.users, nextTurn, state.remainingTurns-1, GuessCountryPhaseView.SelectingLetters, List(), false)))

            else
              throw IllegalMoveException(f"Not a country: $word")

          case GuessCountryEvent.Remove => 
            List(Action.Render(GuessCountryState(state.wordToGuess, state.wordsGuessed, state.users, state.turn, state.remainingTurns, 
            GuessCountryPhaseView.SelectingLetters, if !state.currSelectedLetters.isEmpty then state.currSelectedLetters.tail else List(), state.playAgainAfter)))

          case GuessCountryEvent.Select(letter) => 
            List(Action.Render(GuessCountryState(state.wordToGuess, state.wordsGuessed, state.users, state.turn, state.remainingTurns, 
            GuessCountryPhaseView.SelectingLetters, letter :: state.currSelectedLetters, state.playAgainAfter)))

          case GuessCountryEvent.HintRead =>
            var actions = List[Action[GuessCountryState]]()

            actions = Action.Render(GuessCountryState(state.wordToGuess, state.wordsGuessed, state.users, state.turn, state.remainingTurns, GuessCountryPhaseView.ReadingHint, List(), false)) :: actions // reinitialize currWord?
            actions = Action.Pause(SHOW_HINT_PAUSE_MS) :: actions

            // can't play again even if you have the opportunity!
            val nextTurn = state.users((state.users.indexOf(userId)+1) % state.users.length)
            actions = Action.Render(GuessCountryState(state.wordToGuess, state.wordsGuessed, state.users, nextTurn, state.remainingTurns-1, GuessCountryPhaseView.SelectingLetters, List(), true)) :: actions
            actions.reverse
    }


  override def project(state: GuessCountryState)(userId: UserId): GuessCountryView =
    if (state.isWordGuessed && state.phaseByCurrPlayer != GuessCountryPhaseView.WatchingScore) || state.isNoMoreAvailableTurns then 
      GuessCountryView.Finished(state.whoGuessedWord, state.wordToGuess)
    else
      GuessCountryView.Playing(state.phaseByCurrPlayer, state.wordsGuessed, state.turn, state.wordToGuess, state.currSelectedLetters.mkString.reverse, state.remainingTurns)


/* Server registration magic */
class register:
  WebServer.register(GuessCountryStateMachine)
