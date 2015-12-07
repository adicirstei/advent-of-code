module Advent.Day6 (solution, grid, set, get, switch) where

import String
import Char
import Array

type Action
  = On Int Int
  | Off Int Int
  | Switch Int Int


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
