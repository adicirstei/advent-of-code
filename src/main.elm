import Html exposing(..)
import Signal exposing (Address)

import Advent.Day1

type alias Config model action =
  { model : model
  , update : action -> model -> model
  , view : Address action -> model -> Html
  }

view : Address action -> model -> Html
view address model =
  div [] [ text "abcd" ]

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



main = start { model = 0, view = view, update = identity }
