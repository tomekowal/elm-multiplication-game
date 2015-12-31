module UI where

import String exposing (..)
import Html.Events exposing (..)
import Html exposing (div, button, input, text, Html)
import Html.Attributes exposing (..)
import Action
import Model
import Locale

-- all user inputs need to go to mailbox expecting Action
-- view takes mailbox and model and turns into html
view : Signal.Address Action.Action -> Model.Model -> Html
view userActionsMailboxAddress model =
  case model.gameState of
    Model.NotStarted ->
      viewNotStarted userActionsMailboxAddress model
    Model.Running ->
      viewRunning userActionsMailboxAddress model
    Model.Stopped ->
      viewStopped userActionsMailboxAddress model

center : List Html.Attribute
center = [style
          [("text-align", "center")
          , ("font-size", "2em")
          , ("width", "100%")]]

viewNotStarted : Signal.Address Action.Action -> Model.Model -> Html
viewNotStarted userActionsMailboxAddress model =
  div [] [ timeBar model.counter
         , div center
           [text (Locale.begin model.language)]
         , scoreDiv model
         , multiplicationDiv model
         , inputDiv userActionsMailboxAddress model
         , langButton userActionsMailboxAddress Model.Polish
         , langButton userActionsMailboxAddress Model.English]

viewRunning : Signal.Address Action.Action -> Model.Model -> Html
viewRunning userActionsMailboxAddress model =
  div [] [ timeBar model.counter
         , timer model.language model.counter
         , scoreDiv model
         , multiplicationDiv model
         , inputDiv userActionsMailboxAddress model]

viewStopped : Signal.Address Action.Action -> Model.Model -> Html
viewStopped userActionsMailboxAddress model =
      div [] [ timeBar model.counter
             , timer model.language model.counter
             , scoreDiv model
             , multiplicationDiv model
             -- I want to preserve input field, because autofocus needs it, otherwise it crashes everything
             , inputDiv userActionsMailboxAddress model
             , div center [text ((Locale.correctAnswer model.language) ++ toString (resultOfMultiplication model.multiplication))]
             , resetButton userActionsMailboxAddress ]

resetButton : Signal.Address Action.Action -> Html
resetButton userActionsMailboxAddress =
  button ((onClick userActionsMailboxAddress Action.Reset) :: center) [text "Reset"]

langButton : Signal.Address Action.Action -> Model.Language -> Html
langButton userActionsMailboxAddress language =
  button ((onClick userActionsMailboxAddress (Action.ChangeLanguage language)) :: center) [text (toString language)]

-- address is a mailbox expecting Actions (Signal Action)
myInput : Signal.Address Action.Action -> Model.Model -> Html
myInput userActionsMailboxAddress model =
  input ([on "input"
             targetValue
             (\input -> Signal.message userActionsMailboxAddress (Action.Input input))
         , type' "number"
         , value model.userInput
         , autofocus True
         , disabled (isDisabled model)
         , id "input" ] ++ center) []

scoreDiv : Model.Model -> Html
scoreDiv model =
  div center [text ((Locale.score model.language) ++ (toString model.score))]

multiplicationDiv : Model.Model -> Html
multiplicationDiv model =
  div center [text (stringFromMultiplication model.multiplication)]

inputDiv : Signal.Address Action.Action -> Model.Model -> Html
inputDiv userActionsMailboxAddress model =
  div center [myInput userActionsMailboxAddress model]

timeBar: Int -> Html
timeBar timeLeft =
  div [style [ ("background-color", "blue")
             , ("width", (toString (4*timeLeft)) ++ "%")
             , ("height", "2em")
             , ("margin-left", "auto")
             , ("margin-right", "auto")]] []

timer: Model.Language -> Int -> Html
timer lang timeLeft =
  div center [text ((Locale.timeLeft lang) ++ (toString timeLeft))]

stringFromMultiplication : Model.Multiplication -> String
stringFromMultiplication multiplication =
  toString (fst multiplication) ++ "x" ++ toString (snd multiplication)

resultOfMultiplication : Model.Multiplication -> Int
resultOfMultiplication multiplication =
  (fst multiplication) * (snd multiplication)

isDisabled : Model.Model -> Bool
isDisabled model =
  case model.gameState of
    Model.Stopped ->
      True
    anythingElse ->
      False
