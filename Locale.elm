module Locale where
import Model

begin : Model.Language -> String
begin lang =
  case lang of
    Model.Polish ->
      "Wpisz wynik mnożenia, aby rozpocząć"
    Model.English ->
      "Type the result of multiplying to begin"
