import Html exposing(..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Signal exposing (Address)

import Advent.Day1

type alias Config model action =
  { model : model
  , update : action -> model -> model
  , view : Address action -> model -> Html
  }


type Action 
  = Input String
  | Select String

type alias Model 
  = ( String, String )


onInput : Signal.Address action -> (String -> action) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

update : Action -> Model -> Model
update action model =
  case action of
    Input i -> ( "day1", i )
    Select d -> ( d, "x")


view : Address Action -> Model -> Html
view address ( day, input ) =
  div []
  [ select [] 
      [ option [] [ text "Day 1"]
      , option [] [ text "Day 3"]
      , option [] [ text "Day 4"]
      , option [] [ text "Day 5"]
      , option [] [ text "Day 6"]
      , option [] [ text "Day 7"]
      , option [] [ text "Day 8"]
      , option [] [ text "Day 9"]
      ]
  , textarea [style [("width", "75%"), ("height", "100%")], onInput address Input] []
  , div [ style [ ("width", "25%") ] ] [ text (day ++ input) ]
  ]

start : Config model action -> Signal Html
start cfg =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    update' maybeAction model =
      case maybeAction of
        Just action ->
          cfg.update action model

        Nothing ->
          Debug.crash "This is really wrong"

    model' =
      Signal.foldp update' cfg.model actions.signal

  in
    Signal.map (cfg.view address) model'



main = start { model = ("0", ""), view = view, update = update }
