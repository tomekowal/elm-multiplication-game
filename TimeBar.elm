module TimeBar where

import Html exposing (div, Html)
import Html.Attributes exposing (..)

-- VIEW

view : Int -> Html
view timeLeft =
  div [style [ ("background-color", "blue")
    , ("width", (toString (4*timeLeft)) ++ "%")
    , ("height", "2em")
    , ("margin-left", "auto")
    , ("margin-right", "auto")]] []
