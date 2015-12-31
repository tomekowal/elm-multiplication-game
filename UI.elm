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

center = [style
          [("text-align", "center")
          , ("font-size", "2em")
          , ("width", "100%")]]

viewNotStarted : Signal.Address Action.Action -> Model.Model -> Html
viewNotStarted userActionsMailboxAddress model =
  div [] [ timeBar model.counter
         , div center
           [text "Wpisz wynik mnożenia, aby rozpocząć"]
         , div center [text ("Twój wynik: " ++ (toString model.score))]
         , div center [text (stringFromMultiplication model.multiplication)]
         , div center [myInput userActionsMailboxAddress model.userInput]
         , langButton userActionsMailboxAddress Model.Polish
         , langButton userActionsMailboxAddress Model.English]

viewRunning : Signal.Address Action.Action -> Model.Model -> Html
viewRunning userActionsMailboxAddress model =
  div [] [ timeBar model.counter
         , timer model.counter
         , div center [text ("Twój wynik: " ++ (toString model.score))]
         , div center [text (stringFromMultiplication model.multiplication)]
         , div center [myInput userActionsMailboxAddress model.userInput]]

viewStopped : Signal.Address Action.Action -> Model.Model -> Html
viewStopped userActionsMailboxAddress model =
      div [] [ timeBar model.counter
             , timer model.counter
             , div center [text ("Twój wynik: " ++ (toString model.score))]
             , div center [text (stringFromMultiplication model.multiplication)]
             , div center [myInput userActionsMailboxAddress model.userInput]
             , div center [text ("właściwa odpowiedź: " ++ toString (resultOfMultiplication model.multiplication))]
             , resetButton userActionsMailboxAddress]

resetButton : Signal.Address Action.Action -> Html
resetButton userActionsMailboxAddress =
  button ((onClick userActionsMailboxAddress Action.Reset) :: center) [text "Reset"]

langButton : Signal.Address Action.Action -> Model.Language -> Html
langButton userActionsMailboxAddress language =
  button ((onClick userActionsMailboxAddress (Action.ChangeLanguage language)) :: center) [text (toString language)]

-- address is a mailbox expecting Actions (Signal Action)
-- currently it does nothing
myInput : Signal.Address Action.Action -> String -> Html
myInput userActionsMailboxAddress userInput =
  input ([on "input"
            targetValue
            (\input -> Signal.message userActionsMailboxAddress (Action.Input input))
        , type' "number"
        , value userInput
        , autofocus True
        , id "input" ] ++ center) []

timeBar: Int -> Html
timeBar timeLeft =
  div [style [ ("background-color", "blue")
             , ("width", (toString (2*timeLeft)) ++ "%")
             , ("height", "2em")
             , ("margin-left", "auto")
             , ("margin-right", "auto")]] []

timer: Int -> Html
timer timeLeft =
  div center [text ("Pozostały czas: " ++ (toString timeLeft))]

stringFromMultiplication : Model.Multiplication -> String
stringFromMultiplication multiplication =
  toString (fst multiplication) ++ "x" ++ toString (snd multiplication)

resultOfMultiplication : Model.Multiplication -> Int
resultOfMultiplication multiplication =
  (fst multiplication) * (snd multiplication)
