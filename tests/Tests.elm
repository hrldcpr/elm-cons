module Tests where

import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test

import Cons exposing (..)
import List


checkSuite =
  suite "elm-check tests"
  [ claim "cons increases list length by 1"
  `that` (\ (x, xs) -> cons x xs |> length)
  `is` (\ (x, xs) -> 1 + List.length xs)
  `for` tuple (int, list int)
  ]

all =
  quickCheck checkSuite |> Check.Test.evidenceToTest
