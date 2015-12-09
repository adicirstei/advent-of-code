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
    rex = Regex.regex "^([^0-9]+)(\\d+),(\\d+) through (\\d+),(\\d+)$"
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
        case coords of
          Nothing -> NoOp

          Just c ->
            case str of
              Just "turn on " -> TurnOn c
              Just "turn off " -> TurnOff c
              Just "toggle " -> Toggle c
              _ -> NoOp
    _ -> NoOp




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


updateArea cmd ((x1,y1),(x2,y2)) m =
  if cmd == NoOp then
    m
  else if y1 <= y2 then
    updateArea cmd ((x1,y1+1),(x2,y2)) (Array.set y1 (updateLine cmd (x1, x2) (Maybe.withDefault Array.empty (Array.get y1 m))) m)
  else
    m

updateLine cmd (x1, x2) line =
  if cmd == NoOp then
    line
  else if x1 <= x2 then
    let
      val = Maybe.withDefault 0 (Array.get x1 line)
      val' =
        case cmd of
          TurnOn _ -> 1 + val
          TurnOff _ ->
            if val > 0 then
              val - 1
            else
              0
          Toggle _ -> 2 + val
          _ -> val
    in
      updateLine cmd (x1+1, x2) (Array.set x1 val' line)
  else
    line

updateLine' cmd (x1, x2) line =
  if cmd == NoOp then
    line
  else if x1 <= x2 then
    let
      val = Maybe.withDefault 0 (Array.get x1 line)
      val' =
        case cmd of
          TurnOn _ -> 1
          Toggle _ ->
            if val == 0 then
              1
            else
              0
          TurnOff _ -> 0
          _ -> val
    in
      updateLine cmd (x1+1, x2) (Array.set x1 val' line)
  else
    line


batchUpdate batch m =
  case batch of
    (first :: rest) ->
      let
        cmd = commandFromString first
        area = case cmd of
          NoOp -> ((0,0),(-1,-1))
          Toggle a -> a
          TurnOn b -> b
          TurnOff c -> c
      in
        batchUpdate rest (updateArea cmd area m)
    _ ->
      m

sumLine = Array.toList >> List.sum


sum m =
  sumLine (Array.map sumLine m)


solution input =
  let
    lines = String.lines input
  in
    sum (batchUpdate lines grid)
