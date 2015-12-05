import Html exposing(..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Signal exposing (Address)

import Advent.Day1 as Day1

type alias Config model action =
  { model : model
  , update : action -> model -> model
  , view : Address action -> model -> Html
  }


type Action 
  = Input String
  | Select String

type alias Model =
  { day : String
  , input : String
  , result : String 
  }


onInput : Signal.Address action -> (String -> action) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

onSelect : Signal.Address action -> (String -> action) -> Attribute
onSelect address contentToValue =
  on "change" targetValue (\str -> Signal.message address (contentToValue str))

update : Action -> Model -> Model
update action model =
  case action of
    Select day -> { day = day, input = "", result = "" }
    Input input -> { model | input = input, result = toString (Day1.solution input) }
    


view : Address Action -> Model -> Html
view address { day, input, result } =
  let 
    x = Debug.log "input" input
    y = Debug.log "day" day
    z = Debug.log "result" result
  in
    div []
      [ h1 [] [ text "Advent of code" ]
      , div []
        [ select [ onSelect address Select, style [ ("float", "left"), ("clear", "both") ] ] 
            [ option [ selected ( day == "Day 1" ) ] [ text "Day 1"]
            , option [ selected ( day == "Day 2" ) ] [ text "Day 2"]
            , option [ selected ( day == "Day 3" ) ] [ text "Day 3"]
            , option [ selected ( day == "Day 4" ) ] [ text "Day 4"]
            , option [ selected ( day == "Day 5" ) ] [ text "Day 5"]
            , option [ selected ( day == "Day 6" ) ] [ text "Day 6"]
            , option [ selected ( day == "Day 7" ) ] [ text "Day 7"]
            , option [ selected ( day == "Day 8" ) ] [ text "Day 8"]
            , option [ selected ( day == "Day 9" ) ] [ text "Day 9"]
            ]
        , textarea [style [("width", "75%"), ("height", "300px"), ("float", "left"), ("clear", "left")], onInput address Input, value input ] [ ]
        , div [ style [ ("width", "24%"), ("height", "300px"), ("float", "left"), ("background", "#ffa") ] ] [ text result ]
        ]
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



main = start { model = { day = "Day 1", input = "", result = "" }, view = view, update = update }
