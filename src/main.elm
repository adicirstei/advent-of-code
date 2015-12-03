import Html exposing(..)

import Advent.Day1

view : model -> Signal Html
view model =
  div [] [ text "abcd" ]


start : {model : model, view : model -> Html, update : action -> model -> model } -> Signal Html
start m v u =
  Nothing


app =
  { html = view () }



main = start { model = 0, view = view, update = identity }
