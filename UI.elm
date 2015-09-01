module UI where

import String exposing (..)
import Html.Events exposing (..)
import Html exposing (div, button, input, text, Html)
import Html.Attributes exposing (..)
import Action
import Model

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

viewNotStarted : Signal.Address Action.Action -> Model.Model -> Html
viewNotStarted userActionsMailboxAddress model =
  div [] [ text "Witaj, stary!" ]

viewRunning : Signal.Address Action.Action -> Model.Model -> Html
viewRunning userActionsMailboxAddress model =
  div [] [ timeBar model.counter
         , timer model.counter
         , div [] [text ("Twój wynik: " ++ (toString model.score))]
         , div [] [text (stringFromMultiplication model.multiplication)]
         , div [] [myInput userActionsMailboxAddress model.userInput]]

viewStopped : Signal.Address Action.Action -> Model.Model -> Html
viewStopped userActionsMailboxAddress model =
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

stringFromMultiplication : Model.Multiplication -> String
stringFromMultiplication multiplication =
  toString (fst multiplication) ++ "x" ++ toString (snd multiplication)

resultOfMultiplication : Model.Multiplication -> Int
resultOfMultiplication multiplication =
  (fst multiplication) * (snd multiplication)
