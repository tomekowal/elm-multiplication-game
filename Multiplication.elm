module Multiplication where

import String
import Html exposing (div, button, input, text, Html)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Random exposing (..)
import Signal exposing (Signal, (<~))
import Action
import UI
import Model

-- main function emits Html over time (like in react)
main : Signal Html
main =
  modelUpdatesToView userActionsMailbox.address modelUpdates

userActionsMailbox : Signal.Mailbox Action.Action
userActionsMailbox = Signal.mailbox (Action.Tick 0)

modelUpdates : Signal Model.Model
modelUpdates =
  Signal.foldp update initialModel (actions userActionsMailbox.signal)

-- takes mailbox needed by view, signal of models and returns html
-- basically the same as view, but takes signal of models instead of one model
modelUpdatesToView : Signal.Address Action.Action -> Signal Model.Model -> Signal Html
modelUpdatesToView userActionsMailboxAddress modelUpdates =
  Signal.map (UI.view userActionsMailboxAddress) modelUpdates

initialModel : Model.Model
initialModel =
  { counter = 10
    , input = "We need more time"
    , currentSeed = initialSeed 0
    , multiplication = (10, 5)
    , userInput = ""
    , score = 0
    , gameState = Model.NotStarted }

update: Action.Action -> Model.Model -> Model.Model
update action model =
  case model.gameState of
    Model.NotStarted ->
      updateNotStarted action model
    Model.Running ->
      updateRunning action model
    Model.Stopped ->
      updateStopped action model

updateNotStarted : Action.Action -> Model.Model -> Model.Model
updateNotStarted action model =
  case action of
    Action.Input string ->
      handleInput string model
    Action.Tick timeStamp ->
      { model | currentSeed <- initialSeed (round timeStamp) }
    anything ->
      model

updateStopped : Action.Action -> Model.Model -> Model.Model
updateStopped action model =
  model

updateRunning : Action.Action -> Model.Model -> Model.Model
updateRunning action model =
  case model.counter <= 0 of
    False ->
      updateGame action model
    True ->
      { model | gameState <- Model.Stopped }

-- update takes action and model and returns new model
updateGame : Action.Action -> Model.Model -> Model.Model
updateGame action model =
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

compareInputWithMultiplication : Model.Multiplication -> String -> Bool
compareInputWithMultiplication multipliction userInput =
  case String.toInt userInput of
    Ok integer ->
      integer == UI.resultOfMultiplication multipliction
    Err reason ->
      False

handleInput : String -> Model.Model -> Model.Model
handleInput userInput model =
  case compareInputWithMultiplication model.multiplication userInput of
    True ->
      let
        (multiplication, newSeed) = generateMultiplication model.currentSeed
      in
        { model | counter <- model.counter + 1
          , multiplication <- multiplication
          , score <- model.score + 1
          , userInput <- ""
          , currentSeed <- newSeed
          , gameState <- Model.Running }
    False ->
      { model | userInput <- userInput }

generateMultiplication : Random.Seed -> (Model.Multiplication, Random.Seed)
generateMultiplication seed0 =
  let
    (first, seed1) = generate (int 0 10) seed0
    (second, seed2) = generate (int 0 10) seed1
  in
    ((first, second), seed2)

port focusElement : Signal String
port focusElement =
  modelToInputId <~ modelUpdates

modelToInputId : Model.Model -> String
modelToInputId model =
  "input"
