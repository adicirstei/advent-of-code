module Advent.Day10 where

import String


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x :: xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

takeWhileSame list =
  case list of
    [] -> (0, "0")
    x :: xs ->
      let
        pred = (\a el -> a == el) x
        h = takeWhile pred list
      in
        (List.length h, String.fromChar x)


lookAndSay : String -> String
lookAndSay str =
  let
    lst = String.toList str
    h = takeWhileSame lst
  in
    case h of
      (0, _) -> ""
      (n, c) -> (toString n) ++ c ++ lookAndSay (String.dropLeft n str)



repeat f n start =
  if n <=0 then
    start
  else
    repeat f (n-1) (f start)


solution input =
  input
