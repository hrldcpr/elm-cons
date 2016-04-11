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
  [ claim "length is 1 more than length of tail"
  `that` length
  `is` (tail >> List.length >> (+) 1)
  `for` cons int
  ]

cons : Producer a -> Producer (Cons a)
cons x =
  let
    toCons = uncurry Cons.cons
    fromCons c = (head c, tail c)
    headAndTail = tuple (x, list x)
  in
    convert toCons fromCons headAndTail
