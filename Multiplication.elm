module Multiplication where

import String
import Html exposing (div, button, input, text, Html)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Random exposing (..)
import Signal exposing (Signal, (<~))
import Action


type GameState = NotStarted | Running | Stopped
type alias Multiplication = (Int, Int)
type alias Model = { counter: Int
                   , input: String
                   , currentSeed: Random.Seed
                   , multiplication: Multiplication
                   , userInput: String
                   , score: Int
                   , gameState: GameState }

-- main function emits Html over time (like in react)
main : Signal Html
main =
  let
    -- the only place, where I can do something with side effects
    -- creating mailbox, that will passed to view
    userActionsMailbox = Signal.mailbox (Action.Tick 0)
    -- create signal of model updates
    modelUpdates = 
      Signal.foldp update initialModel (actions userActionsMailbox.signal)
  in
    modelUpdatesToView userActionsMailbox.address modelUpdates
    
-- takes mailbox needed by view, signal of models and returns html
-- basically the same as view, but takes signal of models instead of one model
modelUpdatesToView : Signal.Address Action.Action -> Signal Model -> Signal Html
modelUpdatesToView userActionsMailboxAddress modelUpdates =                     
  Signal.map (view userActionsMailboxAddress) modelUpdates

initialModel : Model
initialModel =
  { counter = 10
    , input = "We need more time"
    , currentSeed = initialSeed 0
    , multiplication = (10, 5)
    , userInput = ""
    , score = 0
    , gameState = Running }
  
-- all user inputs need to go to mailbox expecting Action
-- view takes mailbox and model and turns into html
view : Signal.Address Action.Action -> Model -> Html
view userActionsMailboxAddress model =
  case model.gameState of
    Running ->
      div [] [ timeBar model.counter
             , timer model.counter
             , div [] [text ("Twój wynik: " ++ (toString model.score))]
             , div [] [text (stringFromMultiplication model.multiplication)]
             , div [] [myInput userActionsMailboxAddress model.userInput]]
    Stopped ->
      div [] [div [] [text ("Twój wynik: " ++ (toString model.score))]
             , div [] [text (stringFromMultiplication model.multiplication)]
             , div [] [text ("właściwa odpowiedź: " ++ toString (resultOfMultiplication model.multiplication))]]

-- address is a mailbox expecting Actions (Signal Action)
-- currently it does nothing
myInput : Signal.Address Action.Action -> String -> Html
myInput userActionsMailboxAddress userInput =
  input [on "input"
            targetValue
            (\input -> Signal.message userActionsMailboxAddress (Action.Input input))
        , type' "number"
        , value userInput
        , autofocus True ] []


timeBar: Int -> Html
timeBar timeLeft =
  div [style [ ("background-color", "red")
             , ("width", (toString (2*timeLeft)) ++ "%")
             , ("height", "15px")]] []

timer: Int -> Html
timer timeLeft =
  div [] [text ("Pozostały czas: " ++ (toString timeLeft))]

update: Action.Action -> Model -> Model
update action model =
  case model.counter <= 0 of
    False ->
      updateRunning action model
    True ->
      { model | gameState <- Stopped }

-- update takes action and model and returns new model
updateRunning : Action.Action -> Model -> Model
updateRunning action model =
  case action of
    Action.Tick timeStamp ->
      { model |
                counter <- model.counter - 1
              , currentSeed <- initialSeed (round timeStamp) } 
    Action.Input string ->
      handleInput string model

-- merges user actions and ticks
actions : Signal Action.Action -> Signal Action.Action
actions userActions =
  Signal.merge ticks userActions

-- creates signal of ticks
ticks : Signal Action.Action
ticks =
  Signal.map (\(timeStamp, tick) -> Action.Tick timeStamp) (timestamp (every second))

-- seed : Signal Random.Seed
-- seed = (\ (t, _) -> Random.initialSeed <| round t) <~ Time.timestamp (Signal.constant ())

stringFromMultiplication : Multiplication -> String
stringFromMultiplication multiplication =
  toString (fst multiplication) ++ "x" ++ toString (snd multiplication)

resultOfMultiplication : Multiplication -> Int
resultOfMultiplication multiplication =                      
  (fst multiplication) * (snd multiplication)

compareInputWithMultiplication : Multiplication -> String -> Bool
compareInputWithMultiplication multipliction userInput = 
  case String.toInt userInput of
    Ok integer ->
      integer == resultOfMultiplication multipliction
    Err reason ->
      False

handleInput : String -> Model -> Model
handleInput userInput model =
  case compareInputWithMultiplication model.multiplication userInput of
    True ->
      { model | counter <- model.counter + 1
        , multiplication <- generateMultiplication model.currentSeed
        , score <- model.score + 1
        , userInput <- "" }
    False ->
      { model | userInput <- userInput }

generateMultiplication : Random.Seed -> Multiplication
generateMultiplication seed0 =                        
  let
    (first, seed1) = generate (int 0 10) seed0
    (second, seed2) = generate (int 0 10) seed1 
  in
    (first, second)
