module UI where

import String exposing (..)
import Html.Events exposing (..)
import Html exposing (div, button, input, text, Html)
import Html.Attributes exposing (..)
import Action

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
