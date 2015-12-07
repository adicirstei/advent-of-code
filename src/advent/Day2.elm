module Advent.Day2 (solution) where

import String

toInt s =
  Result.withDefault 0 (String.toInt s)



parseLine line =
  let
    parts = String.split "x" line
    dims = List.map toInt parts
    (x,y,z) = (List.head dims, List.head (List.drop 1 dims), List.head (List.drop 2 dims))
    d = Debug.log "line" line
  in
    { h = Maybe.withDefault 0 x
    , w = Maybe.withDefault 0 y
    , l = Maybe.withDefault 0 z}

ribbon gift =
  let
    edges = List.sort [gift.l, gift.h, gift.w]
    e1 = Maybe.withDefault 0 (List.head edges)
    e2 = Maybe.withDefault 0 (List.head (List.drop 1 edges))
  in
    2 * (e1 + e2) + (List.product edges)

surface gift =
  let
    faces = [ gift.h*gift.l, gift.h*gift.w, gift.w*gift.l ]
    minf = List.minimum faces
  in
    2 * (List.sum faces) + Maybe.withDefault 0 minf



solution : String -> String
solution input =
  let
    lines = String.split "\n" input
    gifts = List.map parseLine lines
    surfaces = List.map surface gifts
    ribbons = List.map ribbon gifts
  in
    toString (List.sum surfaces) ++ "\n" ++ toString (List.sum ribbons )
