package guesscountry

import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import ujson.*

/** Defines wire formats for encoding and decoding Guess Country events and views. */
object GuessCountryWire extends AppWire[GuessCountryEvent, GuessCountryView]:
  import GuessCountryEvent.*
  import GuessCountryView.*

  /** Represents the wire format for encoding and decoding Guess Country events. */
  override object eventFormat extends WireFormat[GuessCountryEvent]:

    /**
     * Encodes a Guess Country event into a ujson.Value.
     *
     * @param t The Guess Country event to encode.
     * @return The encoded ujson.Value representing the event.
     */
    override def encode(t: GuessCountryEvent): Value =
      t match
        case Select(letter) => Obj("select" -> Str(letter.toString()))
        case Remove => Obj("remove" -> Null)
        case GuessSelected => Obj("guessSelected" -> Null)
        case HintRead => Obj("hintRead" -> Null)
      
    /**
     * Decodes a ujson.Value into a Guess Country event.
     *
     * @param json The ujson.Value to decode.
     * @return A Try monad containing the decoded Guess Country event.
     */
    override def decode(json: Value): Try[GuessCountryEvent] =
      Try {
        val jsonObj = json.obj
        jsonObj.get("select") match
          case None => 
            jsonObj.get("remove") match
              case None => 
                jsonObj.get("guessSelected") match             
                  case None =>
                    jsonObj.get("hintRead") match
                      case None => throw DecodingException(f"Error Decoding GuessCountryEvent: $json")
                      case _ => GuessCountryEvent.HintRead
                  case _ => GuessCountryEvent.GuessSelected
              case _ => GuessCountryEvent.Remove        
          case Some(letter) => GuessCountryEvent.Select(letter.str.head)
      }


  /** Represents the wire format for encoding and decoding Guess Country views. */
  override object viewFormat extends WireFormat[GuessCountryView]:

    /**
     * Encodes a Guess Country view into a ujson.Value.
     *
     * @param t The Guess Country view to encode.
     * @return The encoded ujson.Value representing the view.
     */
    def encode(t: GuessCountryView): Value =
      t match
        case Playing(phaseView, board, turn, countryToGuess, currWord, remainingTurns) => 
          Obj("view" -> "Playing", "phaseView" -> PhaseViewWire().encode(phaseView), "board" -> BoardWire().encode(board), 
          "turn" -> Str(turn), "countryToGuess" -> Str(countryToGuess), "currWord" -> Str(currWord), "remainingTurns" -> Num(remainingTurns))

        case Finished(winner, countryToGuess) => Obj("view" -> "Finished", "winner" -> OptionWire(StringWire).encode(winner),
        "countryToGuess" -> Str(countryToGuess))
      
      
    /**
     * Decodes a ujson.Value into a Guess Country view.
     *
     * @param json The ujson.Value to decode.
     * @return A Try monad containing the decoded Guess Country view.
     */
    def decode(json: Value): Try[GuessCountryView] =
      Try {
        val jsonObj = json.obj
        jsonObj("view").str match
          case "Playing" => GuessCountryView.Playing(PhaseViewWire().decode(jsonObj("phaseView")).get, 
            BoardWire().decode(jsonObj("board")).get, jsonObj("turn").str, jsonObj("countryToGuess").str,
            jsonObj("currWord").str, jsonObj("remainingTurns").num.toInt)
          case "Finished" => GuessCountryView.Finished(OptionWire(StringWire).decode(jsonObj("winner")).get, jsonObj("countryToGuess").str)
          case _ => throw DecodingException(f"View neither 'Playing' nor 'Finished': $json")
      }
      

    /** Represents the wire format for encoding and decoding Guess Country phase views.*/
    case class PhaseViewWire() extends WireFormat[GuessCountryPhaseView]:
      def encode(t: GuessCountryPhaseView): Value = t match
        case GuessCountryPhaseView.SelectingLetters => Str("selecting letters")
        case GuessCountryPhaseView.Waiting => Str("waiting")
        case GuessCountryPhaseView.ReadingHint => Str("reading hint")
        case GuessCountryPhaseView.WatchingScore => Str("watching score")

      def decode(json: Value): Try[GuessCountryPhaseView] = Try:
        json.str match
          case "selecting letters" => GuessCountryPhaseView.SelectingLetters
          case "waiting" => GuessCountryPhaseView.Waiting
          case "reading hint" => GuessCountryPhaseView.ReadingHint
          case "watching score" => GuessCountryPhaseView.WatchingScore
          case _ => throw DecodingException(f"Phase View invalid: $json")
        

    /** Represents the wire format for encoding and decoding Guess Country boards. */
    case class BoardWire() extends WireFormat[Board]:
      val trioWire = TrioWire(StringWire, ScoreWire(), StringWire)
      def encode(t: Board): Value = 
        SeqWire(trioWire).encode(t)

      def decode(json: Value): Try[Board] = Try(SeqWire(trioWire).decode(json).get.toList)


    /** Represents the wire format for encoding and decoding Guess Country scores. */
    case class ScoreWire() extends WireFormat[Score]:
      val w = SeqWire(IntWire)
      def encode(t: Score): Value = 
        
        Arr(BooleanWire.encode(t._1), w.encode(t._2), w.encode(t._3), w.encode(t._4))

      def decode(json: Value): Try[Score] = 
        Try {
          val jsonArr = json.arr
          (jsonArr(0).bool, w.decode(jsonArr(1)).get.toList, w.decode(jsonArr(2)).get.toList, w.decode(jsonArr(3)).get.toList)
        }
        

    /** Represents the wire format for encoding and decoding characters. */
    case class CharWire() extends WireFormat[Char]:
      def encode(t: Char): Value = StringWire.encode(t.toString())
      def decode(json: Value): Try[Char] = Try(StringWire.decode(json).get.toCharArray()(0))


    /** Represents the wire format for encoding and decoding tuples of three elements. */
    case class TrioWire[T1, T2, T3](w1: WireFormat[T1], w2: WireFormat[T2], w3: WireFormat[T3]) extends WireFormat[(T1, T2, T3)]:
      def encode(p: (T1, T2, T3)): ujson.Value =
        Arr(w1.encode(p._1), w2.encode(p._2), w3.encode(p._3))
      
      def decode(js: ujson.Value): Try[(T1, T2, T3)] = Try:
        (w1.decode(js.arr(0)).get, w2.decode(js.arr(1)).get, w3.decode(js.arr(2)).get)
      
