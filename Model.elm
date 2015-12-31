module Model where
import Random

type GameState = NotStarted | Running | Stopped
type Language = Polish | English
type alias Multiplication = (Int, Int)
type alias Model = { counter: Int
                   , input: String
                   , currentSeed: Random.Seed
                   , multiplication: Multiplication
                   , userInput: String
                   , score: Int
                   , gameState: GameState
                   , language: Language }

initialModel : Model
initialModel =
  { counter = 9
    , input = "We need more time"
    , currentSeed = Random.initialSeed 0
    , multiplication = (10, 5)
    , userInput = ""
    , score = 0
    , gameState = NotStarted
    , language = English }
