module Advent.Day2 (solution) where

import String

toInt s =
  Result.withDefault 0 (String.toInt s) 



parseLine line =
  let 
    parts = String.split "x" line
    dims = List.map toInt parts
    (x,y,z) = (List.head dims, List.head (List.drop 1 dims), List.head (List.drop 2 dims))
  in
    { h = Maybe.withDefault 0 x
    , w = Maybe.withDefault 0 y
    , l = Maybe.withDefault 0 z}



surface gift =
  let 
    faces = [ gift.h*gift.l, gift.h*gift.w, gift.w*gift.l ]
    minf = List.minimum faces
  in
    2 * (List.sum faces) + Maybe.withDefault 0 minf



solution : String -> String
solution input =
  let 
    lines = String.split "\n\r" input 
    gifts = List.map parseLine lines
    surfaces = List.map surface gifts
  in
    toString (List.sum surfaces)
