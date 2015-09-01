module Model where
import Random

type GameState = NotStarted | Running | Stopped
type alias Multiplication = (Int, Int)
type alias Model = { counter: Int
                   , input: String
                   , currentSeed: Random.Seed
                   , multiplication: Multiplication
                   , userInput: String
                   , score: Int
                   , gameState: GameState }
