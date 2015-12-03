import Html exposing(..)

import Advent.Day1

type alias Config model action =
  { model : model
  , update : action -> model -> model
  , view : }



view : model -> Html
view model =
  div [] [ text "abcd" ]


start : {model : model, view : model -> Html, update : action -> model -> model } -> Signal Html
start m v u =
  let view = v
      update = u
      model = m
  in
    Signal.map view 



main = start { model = 0, view = view, update = identity }
