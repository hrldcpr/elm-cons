module Tests exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (Test, describe, fuzz)
import Debug exposing (toString)
import Cons exposing (..)
import List

checkSuite =
    describe "fuzz tests"
        [ -- Basics
          fuzzEq (tuple ( int, list int )) "cons agrees with ::" (uncurry Cons.cons >> toList) (uncurry (::))
        , fuzzEq (cons int) "cons is inverse of uncons" (uncurry Cons.cons << uncons) identity
        , fuzzEq (tuple ( int, list int )) "uncons is inverse of cons" (uncons << uncurry Cons.cons) identity
        , fuzzEq int "singleton cons agrees with singleton list" (singleton >> toList) (\x -> [ x ])
        , fuzzEq (cons int) "toList agrees with maybeToList" toList (maybeToList << Just)
          -- Avoiding Maybe
        , fuzzEq (cons int) "head agrees with List.head" (head >> Just) (List.head << toList)
        , fuzzEq (cons int) "tail agrees with List.tail" (tail >> Just) (List.tail << toList)
        , fuzzEq (cons int) "minimum agrees with List.minimum" (minimum >> Just) (List.minimum << toList)
        , fuzzEq (cons int) "maximum agrees with List.maximum" (maximum >> Just) (List.maximum << toList)
          -- Convenient Folding
        , fuzzEq (cons string) "foldl1 agrees with List.foldl" (foldl1 (++)) (\c -> List.foldl (++) (head c) <| tail c)
        , fuzzEq (cons string) "foldr1 is the reverse of foldl1" (foldr1 (++)) (foldl1 (++) << reverse)
        , fuzzEq (cons string) "scanl1 agrees with scanlList" (scanl1 (++)) (\c -> scanlList (++) (head c) <| tail c)
          -- List May Be Cons
        , fuzzEq (maybe (cons int)) "fromList is inverse of maybeToList" (fromList << maybeToList) identity
        , fuzzEq (cons int) "consWithMaybe is inverse of unconsWithMaybe" (uncurry consWithMaybe << unconsWithMaybe) identity
        , fuzzEq (tuple ( int, maybe (cons int) )) "unconsWithMaybe is inverse of consWithMaybe" (unconsWithMaybe << uncurry consWithMaybe) identity
        , fuzzEq (cons int) "maybeTail agrees with tail" maybeTail (tail >> fromList)
        , fuzzEq (list int) "maybeToList is inverse of fromList" (maybeToList << fromList) identity
        , fuzzEq (list int) "forList turns head, tail, and minimum into their List equivalents" (\l -> ( forList head l, forList tail l, forList minimum l )) (\l -> ( List.head l, List.tail l, List.minimum l ))
          -- Preserving Non-Emptiness
        , fuzzEq (cons int) "reverse agrees with List.reverse" (reverse >> toList) (List.reverse << toList)
        , fuzzEq (tuple ( cons int, cons int )) "append agrees with List.append" (uncurry append >> toList) (\( c, d ) -> List.append (toList c) <| toList d)
        , fuzzEq (tuple ( cons int, list int )) "appendList agrees with List.append" (uncurry appendList >> toList) (\( c, l ) -> List.append (toList c) l)
        , fuzzEq (tuple ( list int, cons int )) "appendToList agrees with List.append" (uncurry appendToList >> toList) (\( l, d ) -> List.append l <| toList d)
        , fuzzEq (cons (cons int)) "concat agrees with List.concat" (concat >> toList) (List.concat << toList << Cons.map toList)
        , fuzzEq (tuple ( int, cons int )) "intersperse agrees with List.intersperse" (uncurry intersperse >> toList) (\( x, c ) -> List.intersperse x <| toList c)
        , fuzzEq (cons (tuple ( int, string ))) "unzip agrees with List.unzip"
            (\cd ->
                 let
                     ( c, d ) =
                         unzip cd
                 in
                     ( toList c, toList d )
            )
            (List.unzip << toList)
        , fuzzEq (cons int) "map agrees with List.map" (Cons.map toString >> toList) (List.map toString << toList)
        , fuzzEq (tuple ( cons int, cons string )) "map2 agrees with List.map2" (\( c, d ) -> Cons.map2 toString2 c d |> toList) (\( c, d ) -> List.map2 toString2 (toList c) (toList d))
        , fuzzEq (tuple3 ( cons int, cons string, cons float )) "map3 agrees with List.map3" (\( a, b, c ) -> Cons.map3 toString3 a b c |> toList) (\( a, b, c ) -> List.map3 toString3 (toList a) (toList b) (toList c))
        , fuzzEq (cons (cons int)) "concatMap agrees with List.concatMap" (concatMap reverse >> toList) (List.concatMap (reverse >> toList) << toList)
        , fuzzEq (cons int) "indexedMap agrees with List.indexedMap" (indexedMap toString2 >> toList) (List.indexedMap toString2 << toList)
        , fuzzEq (tuple ( string, cons string )) "scanl agrees with scanlList" (\( x, c ) -> scanl (++) x c) (\( x, c ) -> scanlList (++) x <| toList c)
        , fuzzEq (cons int) "sort agrees with List.sort" (sort >> toList) (List.sort << toList)
        , fuzzEq (cons int) "sortBy agrees with List.sortBy" (sortBy (\x -> -x) >> toList) (List.sortBy (\x -> -x) << toList)
        , fuzzEq (cons int) "sortWith agrees with List.sortWith" (sortWith reverseCompare >> toList) (List.sortWith reverseCompare << toList)
          -- List Functions
        , fuzzEq (cons int) "isEmpty agrees with List.isEmpty" isEmpty (List.isEmpty << toList)
        , fuzzEq (cons int) "length agrees with List.length" length (List.length << toList)
        , fuzzEq (tuple ( int, cons int )) "member agrees with List.member" (uncurry member) (\( x, c ) -> List.member x <| toList c)
        , fuzzEq (cons int) "filter agrees with List.filter" (Cons.filter ((<) 0)) (List.filter ((<) 0) << toList)
        , fuzzEq (tuple ( int, cons string )) "take agrees with List.take" (uncurry take) (\( n, c ) -> List.take n <| toList c)
        , fuzzEq (tuple ( int, cons string )) "drop agrees with List.drop" (uncurry drop) (\( n, c ) -> List.drop n <| toList c)
        , fuzzEq (cons int) "partition agrees with List.partition" (partition ((<) 0)) (List.partition ((<) 0) << toList)
        , fuzzEq (cons float) "filterMap agrees with List.filterMap" (filterMap (sqrt >> maybeNaN)) (List.filterMap (sqrt >> maybeNaN) << toList)
        , fuzzEq (tuple ( string, cons string )) "foldl agrees with List.foldl" (\( x, c ) -> foldl (++) x c) (\( x, c ) -> List.foldl (++) x <| toList c)
        , fuzzEq (tuple ( string, cons string )) "foldr agrees with List.foldr" (\( x, c ) -> foldr (++) x c) (\( x, c ) -> List.foldr (++) x <| toList c)
        ]


fuzzEq : Fuzzer a -> String -> (a -> b) -> (a -> b) -> Test
fuzzEq a s f g = fuzz a s (\x -> Expect.equal (f x) (g x))

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

cons : Fuzzer a -> Fuzzer (Cons a)
cons = Fuzz.map (\x -> Cons.cons x [x])

toString2 x y =
    toString ( x, y )


toString3 x y z =
    toString ( x, y, z )


reverseCompare : comparable -> comparable -> Order
reverseCompare x y =
    case compare x y of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


maybeNaN : Float -> Maybe Float
maybeNaN x =
    if isNaN x then
        Nothing
    else
        Just x
