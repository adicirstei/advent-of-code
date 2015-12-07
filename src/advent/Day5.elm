module Advent.Day5 (solution) where

import String
import Char
import Regex

vowel c =
  String.contains (String.fromChar c) "aeiou"


threeVowels str =
  3 <= String.length (String.filter vowel str)

double = Regex.contains (Regex.regex "(.)\\1")

notForbiden str =
  not (Regex.contains (Regex.regex "(ab)|(cd)|(pq)|(xy)") str)


nice str =
  let
    three = threeVowels str
    d = double str
    nf = notForbiden str
    ddd = Debug.log "double" d
  in
    three && d && nf


nice' str =
  let
    doubles = Regex.contains (Regex.regex "(..).*\\1") str
    separated = Regex.contains (Regex.regex "(.).\\1") str
    ddd = Debug.log "double" doubles
  in
    doubles && separated



solution : String -> String
solution input =
  let
    phrases = String.split "\n" input
    nicePhrases = List.filter nice phrases
    secondTh = List.filter nice' phrases
  in
    toString (List.length nicePhrases) ++ "::" ++
    toString (List.length secondTh)
