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
  [
  -- Basics

    claim "cons agrees with ::"
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


  -- Avoiding Maybe

  , claim "head agrees with List.head"
  `that` (head >> Just)
  `is` (List.head << toList)
  `for` cons int

  , claim "tail agrees with List.tail"
  `that` (tail >> Just)
  `is` (List.tail << toList)
  `for` cons int

  , claim "minimum agrees with List.minimum"
  `that` (minimum >> Just)
  `is` (List.minimum << toList)
  `for` cons int

  , claim "maximum agrees with List.maximum"
  `that` (maximum >> Just)
  `is` (List.maximum << toList)
  `for` cons int


  -- Convenient Folding

  , claim "foldl1 agrees with List.foldl"
  `that` (foldl1 toString2)
  `is` (\c -> List.foldl toString2 (head c) <| tail c)
  `for` cons string

  , claim "foldr1 is the reverse of foldl1"
  `that` (foldr1 toString2)
  `is` (foldl1 toString2 << reverse)
  `for` cons string

  , claim "scanl1 agrees with scanlList"
  `that` (scanl1 toString2)
  `is` (\c -> scanlList toString2 (head c) <| tail c)
  `for` cons string


  -- List May Be Cons

  , claim "fromList is inverse of toList'"
  `that` (fromList << toList')
  `is` identity
  `for` maybe (cons int)

  , claim "cons' is inverse of uncons'"
  `that` (uncurry cons' << uncons')
  `is` identity
  `for` cons int

  , claim "uncons' is inverse of cons'"
  `that` (uncons' << uncurry cons')
  `is` identity
  `for` tuple (int, maybe (cons int))

  , claim "tail' agrees with tail"
  `that` tail'
  `is` (tail >> fromList)
  `for` cons int

  , claim "toList' is inverse of fromList"
  `that` (toList' << fromList)
  `is` identity
  `for` list int

  , claim "forList turns head, tail, minimum, and maximum into their List equivalents"
  `that` (\l -> (forList head l, forList tail l, forList minimum l, forList maximum l))
  `is` (\l -> (List.head l, List.tail l, List.minimum l, List.maximum l))
  `for` list int


  -- Preserving Non-Emptiness

  , claim "reverse agrees with List.reverse"
  `that` (reverse >> toList)
  `is` (List.reverse << toList)
  `for` cons int

  , claim "append agrees with List.append"
  `that` (uncurry append >> toList)
  `is` (\(c, d) -> List.append (toList c) <| toList d)
  `for` tuple (cons int, cons int)

  , claim "appendList agrees with List.append"
  `that` (uncurry appendList >> toList)
  `is` (\(c, l) -> List.append (toList c) l)
  `for` tuple (cons int, list int)

  , claim "appendToList agrees with List.append"
  `that` (uncurry appendToList >> toList)
  `is` (\(l, d) -> List.append l <| toList d)
  `for` tuple (list int, cons int)

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
  `that` (Cons.map toString >> toList)
  `is` (List.map toString << toList)
  `for` cons int

  , claim "map2 agrees with List.map2"
  `that` (\(c, d) -> Cons.map2 toString2 c d |> toList)
  `is` (\(c, d) -> List.map2 toString2 (toList c) (toList d))
  `for` tuple (cons int, cons string)

  , claim "map3 agrees with List.map3"
  `that` (\(a, b, c) -> Cons.map3 toString3 a b c |> toList)
  `is` (\(a, b, c) -> List.map3 toString3 (toList a) (toList b) (toList c))
  `for` tuple3 (cons int, cons string, cons float)

  , claim "map4 agrees with List.map4"
  `that` (\(a, b, c, d) -> Cons.map4 toString4 a b c d |> toList)
  `is` (\(a, b, c, d) -> List.map4 toString4 (toList a) (toList b) (toList c) (toList d))
  `for` tuple4 (cons int, cons string, cons float, cons bool)

  , claim "map5 agrees with List.map5"
  `that` (\(a, b, c, d, e) -> Cons.map5 toString5 a b c d e |> toList)
  `is` (\(a, b, c, d, e) -> List.map5 toString5 (toList a) (toList b) (toList c) (toList d) (toList e))
  `for` tuple5 (cons int, cons string, cons float, cons bool, cons percentage)

  , claim "concatMap agrees with List.concatMap"
  `that` (concatMap reverse >> toList)
  `is` (List.concatMap (reverse >> toList) << toList)
  `for` cons (cons int)

  , claim "indexedMap agrees with List.indexedMap"
  `that` (indexedMap toString2 >> toList)
  `is` (List.indexedMap toString2 << toList)
  `for` cons int

  , claim "scanl agrees with scanlList"
  `that` (\(x, c) -> scanl toString2 x c)
  `is` (\(x, c) -> scanlList toString2 x <| toList c)
  `for` tuple (string, cons int)

  , claim "scanlList agrees with List.scanl"
  `that` (\(x, l) -> scanlList toString2 x l |> toList)
  `is` (\(x, l) -> List.scanl toString2 x l)
  `for` tuple (string, list int)

  , claim "sort agrees with List.sort"
  `that` (sort >> toList)
  `is` (List.sort << toList)
  `for` cons int

  , claim "sortBy agrees with List.sortBy"
  `that` (sortBy (\x -> -x) >> toList)
  `is` (List.sortBy (\x -> -x) << toList)
  `for` cons int

  , claim "sortWith agrees with List.sortWith"
  `that` (sortWith reverseCompare >> toList)
  `is` (List.sortWith reverseCompare << toList)
  `for` cons int


  -- List Functions

  , claim "isEmpty agrees with List.isEmpty"
  `that` isEmpty
  `is` (List.isEmpty << toList)
  `for` cons int

  , claim "length agrees with List.length"
  `that` length
  `is` (List.length << toList)
  `for` cons int

  , claim "member agrees with List.member"
  `that` (uncurry member)
  `is` (\(x, c) -> List.member x <| toList c)
  `for` tuple (int, cons int)

  , claim "filter agrees with List.filter"
  `that` (Cons.filter ((<) 0))
  `is` (List.filter ((<) 0) << toList)
  `for` cons int

  , claim "take agrees with List.take"
  `that` (uncurry take)
  `is` (\(n, c) -> List.take n <| toList c)
  `for` tuple (int, cons string)

  , claim "drop agrees with List.drop"
  `that` (uncurry drop)
  `is` (\(n, c) -> List.drop n <| toList c)
  `for` tuple (int, cons string)

  , claim "partition agrees with List.partition"
  `that` (partition ((<) 0))
  `is` (List.partition ((<) 0) << toList)
  `for` cons int

  , claim "filterMap agrees with List.filterMap"
  `that` (filterMap (sqrt >> maybeNaN))
  `is` (List.filterMap (sqrt >> maybeNaN) << toList)
  `for` cons float

  , claim "foldl agrees with List.foldl"
  `that` (\(x, c) -> foldl toString2 x c)
  `is` (\(x, c) -> List.foldl toString2 x <| toList c)
  `for` tuple (string, cons int)

  , claim "foldr agrees with List.foldr"
  `that` (\(x, c) -> foldr toString2 x c)
  `is` (\(x, c) -> List.foldr toString2 x <| toList c)
  `for` tuple (string, cons int)

  , claim "isEmpty agrees with List.isEmpty"
  `that` isEmpty
  `is` (List.isEmpty << toList)
  `for` cons int

  , claim "isEmpty agrees with List.isEmpty"
  `that` isEmpty
  `is` (List.isEmpty << toList)
  `for` cons int

  , claim "isEmpty agrees with List.isEmpty"
  `that` isEmpty
  `is` (List.isEmpty << toList)
  `for` cons int

  , claim "isEmpty agrees with List.isEmpty"
  `that` isEmpty
  `is` (List.isEmpty << toList)
  `for` cons int

  ]

cons : Producer a -> Producer (Cons a)
cons x =
  convert (uncurry Cons.cons) uncons <| tuple (x, list x)

toString2 x y = toString (x, y)
toString3 x y z = toString (x, y, z)
toString4 w x y z = toString (w, x, y, z)
toString5 v w x y z = toString (v, w, x, y, z)

reverseCompare : comparable -> comparable -> Order
reverseCompare x y =
  case compare x y of
    LT -> GT
    EQ -> EQ
    GT -> LT

maybeNaN : Float -> Maybe Float
maybeNaN x = if isNaN x then Nothing else Just x
