package guesscountry

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

import org.scalajs.dom
import scalatags.JsDom.all.*

import cs214.webapp.*
import cs214.webapp.client.*
import guesscountry.*


/** Represents a client application for the Guess Country game. */
object GuessCountryClientApp extends WSClientApp:

  /** Gets the name of the application. */
  def name: String = "guesscountry"

  /**
   * Initializes the client application.
   *
   * @param userId The user ID.
   * @param sendMessage The function to send messages.
   * @param target The target DOM element.
   * @return A client application instance.
   */
  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
    GuessCountryClientAppInstance(userId, sendMessage, target)


/**
 * Represents an instance of the Guess Country client application.
 *
 * @param userId The user ID.
 * @param sendMessage The function to send messages.
 * @param target The target DOM element.
 */
class GuessCountryClientAppInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
    extends StateMachineClientAppInstance[GuessCountryEvent, GuessCountryView](userId, sendMessage, target):

  /** Gets the name of the application. */
  def name: String = "guesscountry"

  override val wire: AppWire[GuessCountryEvent, GuessCountryView] = GuessCountryWire

  val ALPHABET = Alphabet.ALPHABET
  val COUNTRIES = Countries.COUNTRIES
  val NUMBER_OF_ATTEMPTS = NumberOfAttempts.NUMBER_OF_ATTEMPTS
  val SHOW_HINT_PAUSE_MS = ShowHintPauseMs.SHOW_HINT_PAUSE_MS

  /** Represents the possible colors of a letter in the board*/
  enum ColorGuess:
    case Green, Yellow, Red

  override def render(userId: UserId, view: GuessCountryView): Frag =
    frag(
      h2(b("GuessCountry: "), "Guess the mystery country!"),
      renderView(view, userId),
      renderRules()
    )


  /**
   * Renders the view of the game.
   *
   * @param view The Guess Country view.
   * @param userId The user ID.
   * @return The rendered HTML fragment.
   */
  private def renderView(view: GuessCountryView, userId: UserId): Frag = 
    view match
      case GuessCountryView.Playing(phase, board, currentPlayer, countryToGuess, currWord, remainingTurns) => 

        phase match

          // When the current player request a hint
          case GuessCountryPhaseView.ReadingHint if userId == currentPlayer =>
            div(cls := "countries-list")(SeqFrag(COUNTRIES.toList.sorted.map(s => ul(s))))

          case _ =>
            frag(
              div(
                cls := "line-space-items align-horizontally",
                p(i(
                    phase match
                      case GuessCountryPhaseView.SelectingLetters if userId == currentPlayer => f"It's your turn! Guess a country."
                      case GuessCountryPhaseView.ReadingHint => f"$currentPlayer is currently reading an hint."
                      case GuessCountryPhaseView.WatchingScore => f"Watching $currentPlayer guess's score."
                      case _ => f"It's $currentPlayer's turn."
                  )),
                renderRemainingAttempts(remainingTurns),
              ),
              renderBoard(board, currWord, currentPlayer, countryToGuess),
              if userId == currentPlayer && phase == GuessCountryPhaseView.SelectingLetters then renderInputSpace() else frag()
        )

      case GuessCountryView.Finished(winner, countryToGuess) =>
        frag(
          p(cls := "finished",
            {
              winner match
                case Some(userId) => f"$userId won!"
                case None => "No one guessed the country..."
            }
          ),
          p(
            cls := "finished",
            f"The mystery country was $countryToGuess :)"
          )
        )


  /**
   * Renders the rules of the game.
   *
   * @return The rendered HTML fragment containing the game rules.
   */
  private def renderRules(): Frag =
    frag(
      div(
        cls := "rules",
        ul(
          li("Guess the mystery country."),
          ul(
            li("The number of letters is unknown but, when you guess a country, it tells you if you have the right amount of letters."),
            li(f"You have all together ${NUMBER_OF_ATTEMPTS} turns to guess the country."),
            li(b(span(cls := "green", "Green")), ": letter in the right position"),
            li(b(span(cls := "yellow", "Yellow")), ": letter in the country but in a different position"),
            li(b(span(cls := "red", "Red")), ": letter not in the country")
          ),
          li("Press '", b("_"), "' to write a space. It is counted as a character."),
          li("Press ", b("⌫"), " to delete the last character."),
          li("In case you need help, ", b("Hint❓"), f" will show you during ${SHOW_HINT_PAUSE_MS/1000} seconds " +
            "the list of all the countries. Be careful, your next opponent will have one additional turn."),
          li("If you have one additional turn but you ask for a hint, you will lose your additional turn.")
        )
      )
    )

  /** Triggers the HintRead event. */
  private def hintRead(e: dom.Event) =
    sendEvent(GuessCountryEvent.HintRead)

  /** Triggers the GuessSelected event. */
  private def guessSelected(e: dom.Event) =
    sendEvent(GuessCountryEvent.GuessSelected)

  /** Triggers the Select event. */
  private def letterSelected(l: Letter)(e: dom.Event) =
    sendEvent(GuessCountryEvent.Select(l))

  /** Triggers the Remove event. */
  private def letterRemove(e: dom.Event) =
    sendEvent(GuessCountryEvent.Remove)

  /**
   * Renders the hint button.
   *
   * @return The rendered HTML fragment containing the hint button.
   */
  private def renderHintButton(): Frag =
    frag(
      button(
          onclick := hintRead,
          "Hint❓"
        )
    )

  /**
   * Renders the alphabet buttons.
   *
   * @return The rendered HTML fragment containing the alphabet buttons.
   */
  private def renderAlphabet(): Frag =
    frag(
        ALPHABET.toList.map(l =>
        button(
          onclick := letterSelected(l),
          l.toString()
        )
      )
    )

  /**
   * Renders the delete button.
   *
   * @return The rendered HTML fragment containing the delete button.
   */
  private def renderDeleteButton(): Frag =
    frag(
      button(
          onclick := letterRemove,
          "⌫"
        )
    )

  /**
   * Renders the guess button.
   *
   * @return The rendered HTML fragment containing the guess button.
   */
  private def renderGuessButton(): Frag =
    frag(
      button(
          onclick := guessSelected,
          "Guess!"
        )
    )

  /**
   * Renders the input space, consisting of the keyboard and of the buttons for deleting, guessing and requesting a hint.
   *
   * @return The rendered HTML fragment containing the input space.
   */
  private def renderInputSpace(): Frag =
    frag(
      div(
        cls := "input-space",
        div(renderAlphabet()),
        div(
          cls := "line-space-items align-horizontally",
          div(renderDeleteButton()),
          div(renderHintButton()),
          div(renderGuessButton())
        )
      )
    )

  
  /**
   * Renders a fragment indicating whether the length of the guessed country is correct or not.
   *
   * @param isValidLength Boolean indicating whether the length of the guessed country is valid.
   * @return Fragment representing the length validation result.
   */
  private def lengthCountryGuessed(isValidLength: Boolean): Frag =
    frag(p(if isValidLength then "Correct size 😄" else "Wrong size ☹️"))


  /**
   * Renders a fragment representing a letter along with its color (green, yellow, or red) based on the guess.
   *
   * @param l The letter to render.
   * @param c The color of the letter based on the guess.
   * @return Fragment representing the letter along with its color.
   */
  private def renderLetterAndColor(l: Letter, c: ColorGuess): Frag =
    frag(
      p(
        cls := {c match
          case ColorGuess.Green => "letter-green"
          case ColorGuess.Yellow => "letter-yellow"
          case ColorGuess.Red => "letter-red"
        } + " letter", l.toString())
    )


  /**
   * Renders a list of fragments representing each letter of a word along with its color based on the guess.
   *
   * @param w The word to render.
   * @param mapIndexToColor Mapping of letter indices to their corresponding colors.
   * @return List of fragments representing each letter along with its color.
   */
  private def renderOneWordAndColor(w: Word, mapIndexToColor: Map[Int, ColorGuess]): List[Frag] =
    (for
      i <- 0 until w.size
    yield renderLetterAndColor(w.charAt(i), mapIndexToColor(i))).toList
    

  /**
   * Renders a fragment representing one guess of a country, displaying the guessed letters along with their colors and the guessing user.
   *
   * @param guess The guessed word along with its score and the guessing user.
   * @param countryToGuess The country being guessed.
   * @return Fragment representing the guess.
   */
  private def renderOneGuess(guess: (Word, Score, UserId), countryToGuess: Word): Frag =
    val word = guess._1
    val score = guess._2
    val userId = guess._3
    val mapIndexToColor = (score._2.map( i => (i, ColorGuess.Green)) ++ score._3.map( i => (i, ColorGuess.Yellow))
                       ++ score._4.map( i => (i, ColorGuess.Red))).toMap
    frag(div(
        cls := "one-line",
        div(
          cls := "userId-country-guessed-line",
          p(cls := "userId-guessed-line",
            userId
          ),
          p(cls := "country-guessed-line",
            div(cls := "align-horizontally")(SeqFrag(renderOneWordAndColor(word, mapIndexToColor)))
          )
        ),
        div(cls := "length-guessed-line", lengthCountryGuessed(word.length == countryToGuess.length))
      )
    )


  /**
   * Renders a fragment representing a single letter, with no color.
   *
   * @param l The letter to render.
   * @return Fragment representing the letter.
   */
  private def renderLetter(l: Letter): Frag =
    frag(p(cls := "letter", l.toString()))


  /**
   * Renders a fragment representing the letters that the current player is selecting.
   *
   * @param currWord The word formed with the letters selected by the current player.
   * @param currPlayer The current player.
   * @return Fragment representing the guessing line.
   */
  private def renderGuessingLine(currWord: Word, currPlayer: UserId): Frag =
    frag(
      div(
        cls := "align-horizontally one-line",
        div(
          cls := "userId-country-not-guessed-line",
          p(cls := "userId-not-guessed-line", currPlayer),
          p(cls := "country-not-guessed-line",
            { if !currWord.isEmpty then div(cls := "align-horizontally")(SeqFrag(for l <- currWord yield renderLetter(l)))
              else div(cls := "empty-guess", frag())
          })
        )
      )
    )
  

  /**
   * Renders the board of guesses.
   *
   * @param board The board of guesses.
   * @param currWord The word formed by the letters selected by the current player.
   * @param currPlayer The current player.
   * @param countryToGuess The country to guess.
   * @return The rendered HTML fragment.
   */
  private def renderBoard(board: Board, currWord: Word, currPlayer: UserId, countryToGuess: Word): Frag =
    frag(
      div(
        cls := "entire-board",
        SeqFrag(for country <- board.reverse yield renderOneGuess(country, countryToGuess)),
        renderGuessingLine(currWord, currPlayer)
    ))

  /**
   * Renders the remaining attempts.
   *
   * @param remainingTurns The remaining turns.
   * @return The rendered HTML fragment.
   */
  private def renderRemainingAttempts(remainingTurns: Int): Frag =
    frag(
      div(
        cls := "remaining-turn",
        p(span(style := "text-decoration: underline;", "Remaining Turns:"), f" $remainingTurns")
      )
    )
    

// Scala.js magic to register our application from this file
@JSExportAll
object GuessCountryRegistration:
  @JSExportTopLevel("GuessCountryExport")
  val registration = 
    WebClient.register(GuessCountryClientApp)
