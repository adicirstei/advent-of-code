module Advent.Day6  where

import String
import Regex
import Array


type alias Pair = (Int, Int)

type Command
  = NoOp
  | TurnOn (Pair, Pair)
  | TurnOff (Pair, Pair)
  | Toggle (Pair, Pair)


isError : Result x a -> Bool
isError result =
  case result of
    Err _ -> True
    _ -> False

isNothing : Maybe a -> Bool
isNothing maybe =
  case maybe of
    Nothing -> True
    _ -> False


toInt : Maybe String -> Maybe Int
toInt str =
  case str of
    Nothing -> Nothing
    Just s -> (Result.toMaybe << String.toInt) s



toPair : List (Maybe Int) -> Maybe (Int, Int)
toPair list =
  case list of
    (Just a :: Just b :: []) -> Just (a, b)
    _ -> Nothing


listToPairs list =
  let
    left = toPair (List.take 2 list)
    right = toPair (List.drop 2 list)
  in
    case (left, right) of
      (Just a, Just b) -> Just (a, b)
      _ -> Nothing


match : String -> Maybe Regex.Match
match =
  let
    rex = Regex.regex "^([^0-9]+)(\\d+),(\\d)+ through (\\d+),(\\d+)$"
  in
    List.head << (Regex.find (Regex.AtMost 1) rex)



matchToCommand : List (Maybe String) -> Command
matchToCommand list =
  case list of
    (str :: area) ->
      let
        coords = listToPairs (List.map toInt area)
        z = Debug.log "coords" coords
      in
        NoOp
    _ -> NoOp


    --     if coords == Nothing then
    --       NoOp
    --     else
    --       Just c ->
    --         case str of
    --           "turn on " -> TurnOn c
    --           "turn off " -> TurnOff c
    --           "toggle " -> Toggle c
    --           _ -> NoOp
    -- _ -> NoOp




commandFromString : String -> Command
commandFromString str =
  let
    m = match str
  in
    case m of
      Nothing -> NoOp
      Just a -> matchToCommand a.submatches




grid =
  Array.repeat 1000 (Array.repeat 1000 0)

set x y v m =
  let
    row = Maybe.withDefault Array.empty (Array.get y m)
    newRow = Array.set x v row
  in
    Array.set y newRow m

get x y m =
  let
    row = Maybe.withDefault Array.empty (Array.get y m)
  in
    Maybe.withDefault 0 (Array.get x row)


switch x y m =
  let
    row = Maybe.withDefault Array.empty (Array.get y m)
    v = Maybe.withDefault 0 (Array.get x row)
    newRow = Array.set x (if v == 0 then 1 else 0) row
  in
    Array.set y newRow m


solution input =
  Ok (2, 'a')
