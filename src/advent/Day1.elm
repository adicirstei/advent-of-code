module Advent.Day1 where

import String


solution input =
  String.toList input
  |> List.map (\c -> if c == '(' then 1 else -1)
  |> List.scanl (+) 0
  |> List.indexedMap (,) 
