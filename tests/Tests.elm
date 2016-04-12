module Tests where

import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test

import Cons exposing (..)
import List


all =
  quickCheck checkSuite |> Check.Test.evidenceToTest

checkSuite =
  suite "elm-check tests"
  [ claim "cons agrees with ::"
  `that` (uncurry Cons.cons >> toList)
  `is` (uncurry (::))
  `for` tuple (int, list int)

  , claim "cons is inverse of uncons"
  `that` (uncurry Cons.cons << uncons)
  `is` identity
  `for` cons int

  , claim "uncons is inverse of cons"
  `that` (uncons << uncurry Cons.cons)
  `is` identity
  `for` tuple (int, list int)

  , claim "singleton cons agrees with singleton list"
  `that` (singleton >> toList)
  `is` (\x -> [x])
  `for` int

  , claim "toList agrees with toList'"
  `that` toList
  `is` (toList' << Just)
  `for` cons int

  , claim "fromList is inverse of toList'"
  `that` (fromList << toList')
  `is` identity
  `for` maybe (cons int)

  , claim "toList' is inverse of fromList"
  `that` (toList' << fromList)
  `is` identity
  `for` list int

  , claim "tail' agrees with tail"
  `that` tail'
  `is` (tail >> fromList)
  `for` cons int

  , claim "cons' is inverse of uncons'"
  `that` (uncurry cons' << uncons')
  `is` identity
  `for` cons int

  , claim "uncons' is inverse of cons'"
  `that` (uncons' << uncurry cons')
  `is` identity
  `for` tuple (int, maybe (cons int))

  , claim "length agrees with List.length"
  `that` length
  `is` (List.length << toList)
  `for` cons int

  , claim "head agrees with List.head"
  `that` (head >> Just)
  `is` (List.head << toList)
  `for` cons int

  , claim "tail agrees with List.tail"
  `that` (tail >> Just)
  `is` (List.tail << toList)
  `for` cons int

  , claim "maximum agrees with List.maximum"
  `that` (maximum >> Just)
  `is` (List.maximum << toList)
  `for` cons int

  , claim "minimum agrees with List.minimum"
  `that` (minimum >> Just)
  `is` (List.minimum << toList)
  `for` cons int

  , claim "reverse agrees with List.reverse"
  `that` (reverse >> toList)
  `is` (List.reverse << toList)
  `for` cons int

  , claim "append agrees with List.append"
  `that` (uncurry append >> toList)
  `is` (\(c, d) -> List.append (toList c) <| toList d)
  `for` tuple (cons int, cons int)

  , claim "appendToList agrees with List.append"
  `that` (uncurry appendToList >> toList)
  `is` (\(l, d) -> List.append l <| toList d)
  `for` tuple (list int, cons int)

  , claim "appendList agrees with List.append"
  `that` (uncurry appendList >> toList)
  `is` (\(c, l) -> List.append (toList c) l)
  `for` tuple (cons int, list int)

  , claim "concat agrees with List.concat"
  `that` (concat >> toList)
  `is` (List.concat << toList << Cons.map toList)
  `for` cons (cons int)

  , claim "intersperse agrees with List.intersperse"
  `that` (uncurry intersperse >> toList)
  `is` (\(x, c) -> List.intersperse x <| toList c)
  `for` tuple (int, cons int)

  , claim "unzip agrees with List.unzip"
  `that` (\cd -> let (c, d) = unzip cd in (toList c, toList d))
  `is` (List.unzip << toList)
  `for` cons (tuple (int, string))

  , claim "map agrees with List.map"
  `that` (uncurry Cons.map >> toList)
  `is` (\(f, c) -> List.map f <| toList c)
  `for` tuple (func string, cons int)

  , claim "map2 agrees with List.map2"
  `that` (\(f, c, d) -> Cons.map2 f c d |> toList)
  `is` (\(f, c, d) -> List.map2 f (toList c) (toList d))
  `for` tuple3 (func2 string, cons int, cons float)

  , claim "map3 agrees with List.map3"
  `that` (\(f, a, b, c) -> Cons.map3 f a b c |> toList)
  `is` (\(f, a, b, c) -> List.map3 f (toList a) (toList b) (toList c))
  `for` tuple4 (func3 string, cons int, cons float, cons bool)

  , claim "map4 agrees with List.map4"
  `that` (\(f, a, b, c, d) -> Cons.map4 f a b c d |> toList)
  `is` (\(f, a, b, c, d) -> List.map4 f (toList a) (toList b) (toList c) (toList d))
  `for` tuple5 (func4 string, cons int, cons float, cons bool, cons string)

  , claim "map5 agrees with List.map5"
  `that` (\(f, (a, b, c, d, e)) -> Cons.map5 f a b c d e |> toList)
  `is` (\(f, (a, b, c, d, e)) -> List.map5 f (toList a) (toList b) (toList c) (toList d) (toList e))
  `for` tuple (func5 string, tuple5 (cons int, cons float, cons bool, cons string, cons percentage))

  , claim "indexedMap agrees with List.indexedMap"
  `that` (uncurry indexedMap >> toList)
  `is` (\(f, c) -> List.indexedMap f <| toList c)
  `for` tuple (func2 string, cons float)

  , claim "scanl agrees with List.scanl"
  `that` (\(f, x, c) -> scanl f x c |> toList)
  `is` (\(f, x, c) -> List.scanl f x <| toList c)
  `for` tuple3 (func2 string, string, cons float)

  ]

cons : Producer a -> Producer (Cons a)
cons x =
  convert (uncurry Cons.cons) uncons <| tuple (x, list x)
