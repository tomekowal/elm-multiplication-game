module Locale where
import Model

begin : Model.Language -> String
begin lang =
  case lang of
    Model.Polish ->
      "Wpisz wynik mnożenia, aby rozpocząć"
    Model.English ->
      "Type the result of multiplying to begin"

score : Model.Language -> String
score lang =
  case lang of
    Model.Polish ->
      "Twój wynik: "
    Model.English ->
      "Your score: "

correctAnswer : Model.Language -> String
correctAnswer lang =
  case lang of
    Model.Polish ->
      "Właściwa odpowiedź: "
    Model.English ->
      "Correct answer: "

timeLeft : Model.Language -> String
timeLeft lang =
  case lang of
    Model.Polish ->
      "Pozostały czas: "
    Model.English ->
      "Time left: "
