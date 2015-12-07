module Advent.Day4 (solution) where

import String
import Set

toDir s =
  case s of
    '>' -> (1, 0)
    '<' -> (-1, 0)
    '^' -> (0, -1)
    _ -> (0, 1)


split : List a -> (List a, List a)
split list =
  case list of
    [] -> ([], [])
    x :: rest ->
      let
        (xs, ys) = split rest
      in
        (x :: ys, xs)



visit start dirs =
  List.scanl (\ (a1,a2) (b1, b2) -> (a1+b1, a2+b2)) start dirs

solution : String -> String
solution input =
  let
    dirs = List.map toDir (String.toList input)
    start = (0, 0)
    (santaDirs, robotDirs) = split dirs

    santaHouses = Set.fromList (visit start santaDirs)
    robotHouses = Set.fromList (visit start robotDirs)

    alone = Set.fromList (visit start dirs)

  in
    toString (Set.size alone) ++ "::" ++
    toString (Set.size (Set.union santaHouses robotHouses))
