module Tests exposing (..)

import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import Legacy.ElmTest as ElmTest
import Cons exposing (..)
import List


main =
    ElmTest.runSuite all


all =
    quickCheck checkSuite |> Check.Test.evidenceToTest


checkSuite =
    suite "elm-check tests"
        [ -- Basics
          for (is (that (claim "cons agrees with ::") (uncurry Cons.cons >> toList)) (uncurry (::))) (tuple ( int, list int ))
        , for (is (that (claim "cons is inverse of uncons") (uncurry Cons.cons << uncons)) identity) (cons int)
        , for (is (that (claim "uncons is inverse of cons") (uncons << uncurry Cons.cons)) identity) (tuple ( int, list int ))
        , for (is (that (claim "singleton cons agrees with singleton list") (singleton >> toList)) (\x -> [ x ])) int
        , for (is (that (claim "toList agrees with toList'") toList) (toList_ << Just)) (cons int)
          -- Avoiding Maybe
        , for (is (that (claim "head agrees with List.head") (head >> Just)) (List.head << toList)) (cons int)
        , for (is (that (claim "tail agrees with List.tail") (tail >> Just)) (List.tail << toList)) (cons int)
        , for (is (that (claim "minimum agrees with List.minimum") (minimum >> Just)) (List.minimum << toList)) (cons int)
        , for (is (that (claim "maximum agrees with List.maximum") (maximum >> Just)) (List.maximum << toList)) (cons int)
          -- Convenient Folding
        , for (is (that (claim "foldl1 agrees with List.foldl") (foldl1 (++))) (\c -> List.foldl (++) (head c) <| tail c)) (cons string)
        , for (is (that (claim "foldr1 is the reverse of foldl1") (foldr1 (++))) (foldl1 (++) << reverse)) (cons string)
        , for (is (that (claim "scanl1 agrees with scanlList") (scanl1 (++))) (\c -> scanlList (++) (head c) <| tail c)) (cons string)
          -- List May Be Cons
        , for (is (that (claim "fromList is inverse of toList'") (fromList << toList_)) identity) (maybe (cons int))
        , for (is (that (claim "cons' is inverse of uncons'") (uncurry cons_ << uncons_)) identity) (cons int)
        , for (is (that (claim "uncons' is inverse of cons'") (uncons_ << uncurry cons_)) identity) (tuple ( int, maybe (cons int) ))
        , for (is (that (claim "tail' agrees with tail") tail_) (tail >> fromList)) (cons int)
        , for (is (that (claim "toList' is inverse of fromList") (toList_ << fromList)) identity) (list int)
        , for (is (that (claim "forList turns head, tail, minimum, and maximum into their List equivalents") (\l -> ( forList head l, forList tail l, forList minimum l, forList maximum l ))) (\l -> ( List.head l, List.tail l, List.minimum l, List.maximum l ))) (list int)
          -- Preserving Non-Emptiness
        , for (is (that (claim "reverse agrees with List.reverse") (reverse >> toList)) (List.reverse << toList)) (cons int)
        , for (is (that (claim "append agrees with List.append") (uncurry append >> toList)) (\( c, d ) -> List.append (toList c) <| toList d)) (tuple ( cons int, cons int ))
        , for (is (that (claim "appendList agrees with List.append") (uncurry appendList >> toList)) (\( c, l ) -> List.append (toList c) l)) (tuple ( cons int, list int ))
        , for (is (that (claim "appendToList agrees with List.append") (uncurry appendToList >> toList)) (\( l, d ) -> List.append l <| toList d)) (tuple ( list int, cons int ))
        , for (is (that (claim "concat agrees with List.concat") (concat >> toList)) (List.concat << toList << Cons.map toList)) (cons (cons int))
        , for (is (that (claim "intersperse agrees with List.intersperse") (uncurry intersperse >> toList)) (\( x, c ) -> List.intersperse x <| toList c)) (tuple ( int, cons int ))
        , for
            (is
                (that (claim "unzip agrees with List.unzip")
                    (\cd ->
                        let
                            ( c, d ) =
                                unzip cd
                        in
                            ( toList c, toList d )
                    )
                )
                (List.unzip << toList)
            )
            (cons (tuple ( int, string )))
        , for (is (that (claim "map agrees with List.map") (Cons.map toString >> toList)) (List.map toString << toList)) (cons int)
        , for (is (that (claim "map2 agrees with List.map2") (\( c, d ) -> Cons.map2 toString2 c d |> toList)) (\( c, d ) -> List.map2 toString2 (toList c) (toList d))) (tuple ( cons int, cons string ))
        , for (is (that (claim "map3 agrees with List.map3") (\( a, b, c ) -> Cons.map3 toString3 a b c |> toList)) (\( a, b, c ) -> List.map3 toString3 (toList a) (toList b) (toList c))) (tuple3 ( cons int, cons string, cons float ))
        , for (is (that (claim "map4 agrees with List.map4") (\( a, b, c, d ) -> Cons.map4 toString4 a b c d |> toList)) (\( a, b, c, d ) -> List.map4 toString4 (toList a) (toList b) (toList c) (toList d))) (tuple4 ( cons int, cons string, cons float, cons bool ))
        , for (is (that (claim "map5 agrees with List.map5") (\( a, b, c, d, e ) -> Cons.map5 toString5 a b c d e |> toList)) (\( a, b, c, d, e ) -> List.map5 toString5 (toList a) (toList b) (toList c) (toList d) (toList e))) (tuple5 ( cons int, cons string, cons float, cons bool, cons percentage ))
        , for (is (that (claim "concatMap agrees with List.concatMap") (concatMap reverse >> toList)) (List.concatMap (reverse >> toList) << toList)) (cons (cons int))
        , for (is (that (claim "indexedMap agrees with List.indexedMap") (indexedMap toString2 >> toList)) (List.indexedMap toString2 << toList)) (cons int)
        , for (is (that (claim "scanl agrees with scanlList") (\( x, c ) -> scanl (++) x c)) (\( x, c ) -> scanlList (++) x <| toList c)) (tuple ( string, cons string ))
        , for (is (that (claim "scanlList agrees with List.scanl") (\( x, l ) -> scanlList (++) x l |> toList)) (\( x, l ) -> List.scanl (++) x l)) (tuple ( string, list string ))
        , for (is (that (claim "sort agrees with List.sort") (sort >> toList)) (List.sort << toList)) (cons int)
        , for (is (that (claim "sortBy agrees with List.sortBy") (sortBy (\x -> -x) >> toList)) (List.sortBy (\x -> -x) << toList)) (cons int)
        , for (is (that (claim "sortWith agrees with List.sortWith") (sortWith reverseCompare >> toList)) (List.sortWith reverseCompare << toList)) (cons int)
          -- List Functions
        , for (is (that (claim "isEmpty agrees with List.isEmpty") isEmpty) (List.isEmpty << toList)) (cons int)
        , for (is (that (claim "length agrees with List.length") length) (List.length << toList)) (cons int)
        , for (is (that (claim "member agrees with List.member") (uncurry member)) (\( x, c ) -> List.member x <| toList c)) (tuple ( int, cons int ))
        , for (is (that (claim "filter agrees with List.filter") (Cons.filter ((<) 0))) (List.filter ((<) 0) << toList)) (cons int)
        , for (is (that (claim "take agrees with List.take") (uncurry take)) (\( n, c ) -> List.take n <| toList c)) (tuple ( int, cons string ))
        , for (is (that (claim "drop agrees with List.drop") (uncurry drop)) (\( n, c ) -> List.drop n <| toList c)) (tuple ( int, cons string ))
        , for (is (that (claim "partition agrees with List.partition") (partition ((<) 0))) (List.partition ((<) 0) << toList)) (cons int)
        , for (is (that (claim "filterMap agrees with List.filterMap") (filterMap (sqrt >> maybeNaN))) (List.filterMap (sqrt >> maybeNaN) << toList)) (cons float)
        , for (is (that (claim "foldl agrees with List.foldl") (\( x, c ) -> foldl (++) x c)) (\( x, c ) -> List.foldl (++) x <| toList c)) (tuple ( string, cons string ))
        , for (is (that (claim "foldr agrees with List.foldr") (\( x, c ) -> foldr (++) x c)) (\( x, c ) -> List.foldr (++) x <| toList c)) (tuple ( string, cons string ))
        , for (is (that (claim "isEmpty agrees with List.isEmpty") isEmpty) (List.isEmpty << toList)) (cons int)
        , for (is (that (claim "isEmpty agrees with List.isEmpty") isEmpty) (List.isEmpty << toList)) (cons int)
        , for (is (that (claim "isEmpty agrees with List.isEmpty") isEmpty) (List.isEmpty << toList)) (cons int)
        , for (is (that (claim "isEmpty agrees with List.isEmpty") isEmpty) (List.isEmpty << toList)) (cons int)
        ]


cons : Producer a -> Producer (Cons a)
cons x =
    convert (uncurry Cons.cons) uncons <| tuple ( x, list x )


toString2 x y =
    toString ( x, y )


toString3 x y z =
    toString ( x, y, z )


toString4 w x y z =
    toString ( w, x, y, z )


toString5 v w x y z =
    toString ( v, w, x, y, z )


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
