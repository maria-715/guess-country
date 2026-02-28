package driver

import cs214.webapp.*
import cs214.webapp.messages.Action
import guesscountry.*
import scala.util.Random
import cs214.webapp.exceptions.*

class GuessCountryTest extends WebappTest[GuessCountryEvent, GuessCountryState, GuessCountryView]:
  val sm = guesscountry.GuessCountryStateMachine

  private val USER_IDS = Seq(UID0, UID1, UID2)

  val RNG = util.Random(0)
  val ALPHABET = Alphabet.ALPHABET
  val COUNTRIES = Countries.COUNTRIES
  val NUMBER_OF_ATTEMPTS = NumberOfAttempts.NUMBER_OF_ATTEMPTS
  val SHOW_HINT_PAUSE_MS = ShowHintPauseMs.SHOW_HINT_PAUSE_MS

  /** Projects a given state for each given player and extract the [[stateView]]
    * field of the result.
    */
  def projectPlayingViews(userIds: Seq[UserId])(state: GuessCountryState) =
    USER_IDS
      .map(sm.project(state))
      .map(_.assertInstanceOf[GuessCountryView.Playing])

  case class GuessCountryResult(
      guess: Word,
      countryToGuess: Word,
      selections: List[GuessCountryState],
      actions: Seq[Action[GuessCountryState]]
  ):
    def isGuessed= guess == countryToGuess

    def stateWithScoreShown =
      assert(actions.nonEmpty)
      actions.head.assertInstanceOf[messages.Action.Render[GuessCountryState]].st

    def stateWithNextTurn =
      assert(actions.nonEmpty)
      actions.last.assertInstanceOf[messages.Action.Render[GuessCountryState]].st

    def allStates =
      selections ++ (actions.collect { case Action.Render(st) => st })

  def pickRandomCountry: Word = COUNTRIES.drop(Random.nextInt(COUNTRIES.size)).head

  /** Makes a user select letters and try to guess the country and
    * returns the score.
    */
  def tryToGuessCountryOnlySelect(state: GuessCountryState, userId: UserId, selectedCountry: Word): GuessCountryResult =
    //println(f"tryToGuessCountryOnlySelect: selectedCountry = $selectedCountry")
    val letters = selectedCountry.toList
    var selections = List[GuessCountryState]()  // selections in reverse order
    for i <- 0 until letters.length do
        //println(f"tryToGuessCountryOnlySelect: i = $i")
        selections = (assertSingleRender:
            sm.transition(if i != 0 then selections.head else state)(userId, GuessCountryEvent.Select(letters(i))))
            :: selections

    //println(f"tryToGuessCountryOnlySelect: currSelectedLetters = ${sm.currSelectedLetters}")
    val guessCountry = assertSuccess:
        sm.transition(selections.head)(userId, GuessCountryEvent.GuessSelected)

    assert(guessCountry.nonEmpty)

    val finalState = guessCountry.head.asInstanceOf[messages.Action.Render[GuessCountryState]].st
    val playingGuessCountryView = sm.project(finalState)(userId).assertInstanceOf[GuessCountryView.Playing]

    GuessCountryResult(
      selectedCountry,
      state.wordToGuess,
      selections,
      guessCountry
    )

  def boardSize(state: GuessCountryState): Int =
    sm.project(state)(UID0).assertInstanceOf[GuessCountryView.Playing].wordsGuessed.size

/// # Unit tests

/// ## Initial state

  lazy val initState = sm.init(USER_IDS)

  test("GuessCountry: Initial state has empty board"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for view <- views do
      assertEquals(view.wordsGuessed.size, 0)

  test("GuessCountry: Initial state has the number of remaining turns at the maximum"):
    val views = projectPlayingViews(USER_IDS)(initState)
    for view <- views do
      assertEquals(view.remainingTurns, NUMBER_OF_ATTEMPTS)

  test("GuessCountry: Initial state has a valid country to guess"):
    val views = projectPlayingViews(USER_IDS)(initState)
    val mysteryCountry = views(0).countryToGuess
    assert(COUNTRIES.contains(mysteryCountry))
    for playingView <- views do
      assert(playingView.countryToGuess == mysteryCountry)

  test("GuessCountry: Initial state has correct initial player"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for playingView <- views do
      assertEquals(playingView.currentPlayer, UID0)

/// ## Playing state

  test("GuessCountry: Playing state should let the player select a letter and add it to the current word being constructed"):

    val newState = assertSingleRender:
      sm.transition(initState)(UID0, GuessCountryEvent.Select(ALPHABET(0)))

    for playingView <- projectPlayingViews(USER_IDS)(newState) do
      assertEquals(playingView.currWord, ALPHABET(0).toString())

  def tryToGuessCountry(guess: Word, state: GuessCountryState = initState, userId: UserId = UID0) =
    tryToGuessCountryOnlySelect(state, userId, guess)

  /* Cheat by looking at all the cards to facilitate testing. */
  def guessCountryForEachCountry(state: GuessCountryState): Seq[Word] =
    COUNTRIES.toList.map: country =>
      tryToGuessCountry(country, state, UID0).guess

  def guessRandomCountry(matching: Boolean, state: GuessCountryState = initState, userId: UserId = UID0): GuessCountryResult =
    val countries = RNG.shuffle(guessCountryForEachCountry(initState))
    assert(countries.size > 1, "There should be at least two different countries!")
    val selectedCountry =
        if matching then state.wordToGuess
        else if countries.head != state.wordToGuess then countries.head else countries.tail.head
    tryToGuessCountryOnlySelect(state, userId, selectedCountry)


  test("GuessCountry: Playing state shows the guess on the board"):
    val countries = guessCountryForEachCountry(initState)
    val afterGuess = guessRandomCountry(matching = false)

    // Three actions: render watching score, pause, render for next turn
    assertEquals(afterGuess.actions.length, 3)

    // The country is on the board for all the players
    for GuessCountryView.Playing(phase, wordsGuessed, currentPlayer, countryToGuess, currWord, remainingTurns) <- projectPlayingViews(USER_IDS)(afterGuess.stateWithScoreShown)
    do
      assertEquals(wordsGuessed.size, NUMBER_OF_ATTEMPTS-remainingTurns+1)
      assertEquals(wordsGuessed(0).word, afterGuess.guess)

    // Check that we have a proper pause
    val Action.Pause(durationMs) = afterGuess.actions(1).assertInstanceOf[Action.Pause[GuessCountryState]]
    assert(durationMs > 100, "Too fast!")

    // Assert that the current word is empty for the next turn
    val lastActionState = afterGuess.stateWithNextTurn
    for GuessCountryView.Playing(phase, wordsGuessed, currentPlayer, countryToGuess, currWord, remainingTurns) <- projectPlayingViews(USER_IDS)(lastActionState) do
      assertEquals(currWord.isEmpty, true)

  test("GuessCountry: Playing state shows correct Score if mystery word has been guessed"):
    for (userId, userIdx) <- USER_IDS.zipWithIndex do
      var st = initState
      //println(f"USER_IDS: ${USER_IDS.take(userIdx)}")
      // Skip others' turns
      for otherId <- USER_IDS.take(userIdx) do
        //println(f"otherId: $otherId")
        st = guessRandomCountry(matching = false, state = st, userId = otherId).stateWithNextTurn
        //println(f"userId: $userId WordsGuessed: ${st.wordsGuessed}")
      // Find a match when it comes to userId's turn
      val afterGuess = guessRandomCountry(matching = true, state = st, userId = userId).stateWithNextTurn
      //println(f"WordsGuessed: ${afterGuess.wordsGuessed}")
      val score = afterGuess.wordsGuessed(0).score
      assertEquals(score.greenLetters.size, st.wordToGuess.length)
      assertEquals(score.yellowLetters.isEmpty, true)
      assertEquals(score.redLetters.isEmpty, true)


  test("GuessCountry: Playing state shows correct Score if mystery word has NOT been guessed"):
    // not useful because exactly same algorithm as I did -> more efficient to just check the board directly

    /*
    // One turn each. Checks randomly (if we checked for everything: 195^195 comparisons).
    var st = initState
    for userId <- USER_IDS do
      var afterFlip = guessRandomCountry(matching = false, state = st, userId = userId)
      val score = st.wordsGuessed(0).score
      val green = score.greenLetters.toList.sorted
      val yellow = score.yellowLetters.toList.sorted
      val red = score.redLetters.toList.sorted
    */    
    true


  def playEntireGame(initState: GuessCountryState): Seq[GuessCountryResult] =
    // Since there is only a limit number of attempts (10 attempts and 3 players):
    val state = initState.copy(remainingTurns = 100_000)
    val chooseFirstCountry = RNG.shuffle(COUNTRIES.toList)

    val firstGuess = tryToGuessCountryOnlySelect(state, UID0, chooseFirstCountry.head)

      var i = 0
      val mysteryCountry = chooseFirstCountry.indexOf(state.wordToGuess)
      chooseFirstCountry.tail.take(mysteryCountry).scanLeft(firstGuess) { case (guess, country) =>
        i += 1
        tryToGuessCountryOnlySelect(guess.stateWithNextTurn, (initState.users)(i % (initState.users).length), country)
      }

  test("GuessCountry: Playing state should first show that word is all green before declaring a winner"):
    val guesses = playEntireGame(initState)
    val lastGuess = guesses.last // head or last

    // Penultimate state should be showing the last guess on the board
    val guessViews = projectPlayingViews(USER_IDS)(lastGuess.stateWithScoreShown)
    for guessView <- guessViews do
      assertEquals(guessViews(0).wordsGuessed(0).word, lastGuess.countryToGuess)

    // Last state should be showing the winner
    val winViews = USER_IDS.map(sm.project(lastGuess.stateWithNextTurn))
    for winView <- winViews do
      winView.assertInstanceOf[GuessCountryView.Finished]

    /*
    TODO: Other Tests (even if works when testing by running the game):
        - when a user is reading a hint, no user can play
        - Delete event works as expected (1. delete last letter 2. nothing happens when trying to delete empty word)
        - Remaining Turns works as expected (starts at NUMBER_OF_ATTEMPTS and game finishes when = 0)
    */

/// ## Won state

  test("GuessCountry: Won state should prevent any playing interaction by displaying an error"):
    val lastState = playEntireGame(initState).last.stateWithNextTurn

    for userId <- USER_IDS do
      assertFailure[exceptions.IllegalMoveException]:
        sm.transition(lastState)(userId, GuessCountryEvent.Select(ALPHABET(0)))

      assertFailure[exceptions.IllegalMoveException]:
        sm.transition(lastState)(userId, GuessCountryEvent.Select(ALPHABET(0)))

  test("GuessCountry: Won state should contain the id of the player who guessed the country and not another one"):
    val lastState = playEntireGame(initState).last.stateWithNextTurn

    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).assertInstanceOf[GuessCountryView.Finished]
    do
      assertEquals(_, GuessCountryView.Finished(Some(UID0), lastState.wordToGuess))


/// ## Encoding and decoding

  test("GuessCountry: Event wire"):
    for letter <- ALPHABET do
      GuessCountryEvent.Select(letter).testEventWire
    GuessCountryEvent.GuessSelected.testEventWire

  test("GuessCountry: View wire"):
    for
      n <- 1 to USER_IDS.length
      userIds = USER_IDS.take(n)
      guess <- playEntireGame(sm.init(userIds))
      s <- guess.allStates
      u <- userIds
    do
      sm.project(s)(u).testViewWire
