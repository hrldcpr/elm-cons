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

  , claim "length is 1 more than length of tail"
  `that` length
  `is` (tail >> List.length >> (+) 1)
  `for` cons int
  ]

cons : Producer a -> Producer (Cons a)
cons x =
  convert (uncurry Cons.cons) uncons <| tuple (x, list x)
