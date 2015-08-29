import Html exposing (div, button, input, text, Html)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
    
type Action = Tick | Increment | Input String
type alias Model = { counter: Int, input: String }

-- main function emits Html over time (like in react)
main : Signal Html
main =
  let
    -- the only place, where I can do something with side effects
    -- creating mailbox, that will passed to view
    userActionsMailbox = Signal.mailbox Tick
    -- create signal of model updates
    modelUpdates = 
      Signal.foldp update initialModel (actions userActionsMailbox.signal)
  in
    modelUpdatesToView userActionsMailbox.address modelUpdates
    
-- takes mailbox needed by view, signal of models and returns html
-- basically the same as view, but takes signal of models instead of one model
modelUpdatesToView : Signal.Address Action -> Signal Model -> Signal Html
modelUpdatesToView userActionsMailboxAddress modelUpdates =                     
  Signal.map (view userActionsMailboxAddress) modelUpdates

initialModel : Model
initialModel =
  { counter = 100, input = "We need more time" }
  
-- all user inputs need to go to mailbox expecting Action
-- view takes mailbox and model and turns into html
view : Signal.Address Action -> Model -> Html
view userActionsMailboxAddress model =
  div [] [div [] [text (toString model.counter)]
         , div [] [myButton userActionsMailboxAddress model.input]
         , div [] [myInput userActionsMailboxAddress]]

-- address is a mailbox expecting Actions (Signal Action)
-- currently it does nothing
myInput : Signal.Address Action -> Html
myInput userActionsMailboxAddress =
  input [on "input"
            targetValue
            (\input -> Signal.message userActionsMailboxAddress (Input input))
        , type' "number" ] []

-- address is a mailbox expecting Actions (Signal Action)
-- button click sends Increment action
myButton : Signal.Address Action -> String -> Html
myButton userActionsMailboxAddress userInput =
  button [onClick userActionsMailboxAddress Increment] [text userInput]

-- update takes action and model and returns new model
update : Action -> Model -> Model
update action model =
  case action of
    Tick ->
      { model | counter <- model.counter - 1 } 
    Increment ->
      { model | counter <- model.counter + 1 } 
    Input string ->
      { model | input <- string }

-- merges user actions and ticks
actions : Signal Action -> Signal Action
actions userActions =
  Signal.merge ticks userActions

-- creates signal of ticks
ticks : Signal Action
ticks =
  Signal.map (\tick -> Tick) (every second)
