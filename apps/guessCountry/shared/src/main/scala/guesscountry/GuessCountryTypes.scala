package guesscountry

import java.nio.file.{Files, Paths}

import scala.io.Source
import scala.util.{Random, Try}

import cs214.webapp.UserId

/**
 * Represents the score of a guess.
 *
 * @param isRightNumberLetters Indicates whether the guessed country's length matches the mystery country's length.
 * @param greenLetters         A list of indexes of the letters whose positions have been correctly guessed.
 * @param yellowLetters        A list of indexes of the letters that are in the mystery word but whose guessed positions are incorrect.
 * @param redLetters           A list of indexes of the letters that are not contained in the mystery country.
 */
type Score = (Boolean, List[Int], List[Int], List[Int])
extension(s: Score)
  def isRightNumberLetters = s._1
  def greenLetters = s._2
  def yellowLetters = s._3
  def redLetters = s._4

type Word = String
type Letter = Char


/**
 * Stores all information about the current game.
 *
 * @param wordToGuess         The mystery country to be guessed in the game.
 * @param wordsGuessed        A board displaying the countries that have already been guessed, along with their corresponding 
 *                            scores and the users who made the guesses.
 * @param users               A sequence of all the players participating in the game.
 * @param turn                The UserId of the current player.
 * @param remainingTurns      The number of remaining turns to guess the country.
 * @param phaseByCurrPlayer  The phase of the game being viewed by the current player.
 * @param currSelectedLetters A list of the letters currently selected by the current player.
 * @param playAgainAfter     Indicates whether the current player gets another turn after their current turn.
 */
case class GuessCountryState(wordToGuess: Word, wordsGuessed: Board, users: Seq[UserId], turn: UserId, remainingTurns: Int, 
phaseByCurrPlayer: GuessCountryPhaseView, currSelectedLetters: List[Letter], playAgainAfter: Boolean):

  /**
    * @return `true` if the last guessed country matches the mystery country; otherwise, `false`.
    */
  def isWordGuessed: Boolean = !wordsGuessed.isEmpty && wordsGuessed(0)._1 == wordToGuess

  /**
   * @return An optional UserId representing the player who guessed the mystery country, or None if no one guessed the country 
   *         and there are no more remaining turns.
   */
  def whoGuessedWord: Option[UserId] = if isWordGuessed then Some(wordsGuessed(0)._3) else None

  /**
   * @return `true` if no more turns are available; otherwise, `false`.
   */
  def isNoMoreAvailableTurns = remainingTurns <= 0


/** Represents the possible events in the Guess Country game. */
enum GuessCountryEvent:

  /** Selects or deselect a letter. */
  case Select(letter: Letter)

  /** Removes the last selected letter. */
  case Remove

  /** Attempts to guess the selected letters. */
  case GuessSelected
  
   /** Reads the hint. */
  case HintRead


/** Client views reflect the state of the game: playing or finished. */
enum GuessCountryView:

  /** Game in progress. */
  case Playing(phase: GuessCountryPhaseView, wordsGuessed: Board, currentPlayer: UserId, countryToGuess: String, currWord: Word, 
              remainingTurns: Int)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished(winner: Option[UserId], countryToGuess: String)


/** Represents the possible phases in the game. */
enum GuessCountryPhaseView:

  /** It's our turn to select letters to form a country. */
  case SelectingLetters

  /** It's another player's turn, so we're just waiting for them to make a guess. */
  case Waiting

  /** After making a guess, watching the score on the board. */
  case WatchingScore

  /** Indicates that the player is reading a hint. */
  case ReadingHint


/**
 * Represents the score of a guess.
 *
 * @param word          The word in the entry
 * @param score  The score of the entry
 * @param userId The user who made the guess
 */
type BoardEntry = (Word, Score, UserId)
extension (bE: BoardEntry)
  def word = bE._1
  def score = bE._2
  def userId = bE._3

/** Represents the board containing guessed countries, their scores, and the users who made the guesses. */
type Board = List[BoardEntry]
object Board:
  /** Returns an empty board. */
  def empty: Board = List.empty


// ALPHABET, COUNTRIES, NUMBER_OF_ATTEMPTS, SHOW_HINT_PAUSE_MS were originally in GuessCountryLogic. However, due to the configuration 
// of the project, they were not accessible from GuessCountryUI (it would have required me to modify the dependencies). 
// From GuessCountryTypes, they are available in the two files.
object Alphabet:
  /** The alphabet used in the game. */
  val ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_".toList
  
object NumberOfAttempts:
  /** The number of attempts allowed for guessing a country. */
  val NUMBER_OF_ATTEMPTS = 10

object ShowHintPauseMs:
  /** The duration to show a hint in milliseconds. */
  val SHOW_HINT_PAUSE_MS = 20_000

object ShowScorePauseMs:
  /** The duration to show a hint in milliseconds. */
  val SHOW_SCORE_PAUSE_MS = 1_500

object Countries:
  /** The set of all countries in the game. */
  val COUNTRIES = Set(
    "Afghanistan",
    "Albania",
    "Algeria",
    "Andorra",
    "Angola",
    "Antigua_and_Barbuda",
    "Argentina",
    "Armenia",
    "Australia",
    "Austria",
    "Azerbaijan",
    "Bahamas",
    "Bahrain",
    "Bangladesh",
    "Barbados",
    "Belarus",
    "Belgium",
    "Belize",
    "Benin",
    "Bhutan",
    "Bolivia",
    "Bosnia_and_Herzegovina",
    "Botswana",
    "Brazil",
    "Brunei",
    "Bulgaria",
    "Burkina_Faso",
    "Burundi",
    "Cambodia",
    "Cameroon",
    "Canada",
    "Cape_Verde",
    "Central_African_Republic",
    "Chad",
    "Chile",
    "China",
    "Colombia",
    "Comoros",
    "Costa_Rica",
    "Croatia",
    "Cuba",
    "Cyprus",
    "Czech_Republic",
    "Democratic_Republic_of_the_Congo",
    "Denmark",
    "Djibouti",
    "Dominica",
    "Dominican_Republic",
    "East_Timor",
    "Ecuador",
    "Egypt",
    "El_Salvador",
    "Equatorial_Guinea",
    "Eritrea",
    "Estonia",
    "Eswatini",
    "Ethiopia",
    "Federated_States_of_Micronesia",
    "Fiji",
    "Finland",
    "France",
    "Gabon",
    "Gambia",
    "Georgia",
    "Germany",
    "Ghana",
    "Greece",
    "Grenada",
    "Guatemala",
    "Guinea",
    "Guinea_Bissau",
    "Guyana",
    "Haiti",
    "Honduras",
    "Hungary",
    "Iceland",
    "India",
    "Indonesia",
    "Iran",
    "Iraq",
    "Ireland",
    "Israel",
    "Italy",
    "Ivory_Coast",
    "Jamaica",
    "Japan",
    "Jordan",
    "Kazakhstan",
    "Kenya",
    "Kiribati",
    "North_Korea",
    "South_Korea",
    "Kosovo",
    "Kuwait",
    "Kyrgyzstan",
    "Laos",
    "Latvia",
    "Lebanon",
    "Lesotho",
    "Liberia",
    "Libya",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Macedonia",
    "Madagascar",
    "Malawi",
    "Malaysia",
    "Maldives",
    "Mali",
    "Malta",
    "Marshall_Islands",
    "Mauritania",
    "Mauritius",
    "Mexico",
    "Moldova",
    "Monaco",
    "Mongolia",
    "Montenegro",
    "Morocco",
    "Mozambique",
    "Myanmar",
    "Namibia",
    "Nauru",
    "Nepal",
    "Netherlands",
    "New_Zealand",
    "Nicaragua",
    "Niger",
    "Nigeria",
    "Norway",
    "Oman",
    "Pakistan",
    "Palau",
    "Panama",
    "Papua_New_Guinea",
    "Paraguay",
    "Peru",
    "Philippines",
    "Poland",
    "Portugal",
    "Qatar",
    "Republic_of_the_Congo",
    "Romania",
    "Russia",
    "Rwanda",
    "Saint_Kitts_and_Nevis",
    "Saint_Lucia",
    "Saint_Vincent_and_the_Grenadines",
    "Samoa",
    "San_Marino",
    "Sao_Tome_and_Principe",
    "Saudi_Arabia",
    "Senegal",
    "Serbia",
    "Seychelles",
    "Sierra_Leone",
    "Singapore",
    "Slovakia",
    "Slovenia",
    "Solomon_Islands",
    "Somalia",
    "South_Africa",
    "South_Sudan",
    "Spain",
    "Sri_Lanka",
    "Sudan",
    "Suriname",
    "Sweden",
    "Switzerland",
    "Syria",
    "Taiwan",
    "Tajikistan",
    "Tanzania",
    "Thailand",
    "Togo",
    "Tonga",
    "Trinidad_and_Tobago",
    "Tunisia",
    "Turkey",
    "Turkmenistan",
    "Tuvalu",
    "Uganda",
    "Ukraine",
    "United_Arab_Emirates",
    "United_Kingdom",
    "United_States_of_America",
    "Uruguay",
    "Uzbekistan",
    "Vanuatu",
    "Vatican_City",
    "Venezuela",
    "Vietnam",
    "Yemen",
    "Zambia",
    "Zimbabwe"
  ).map(_.toUpperCase)
