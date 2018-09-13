module Cons
    exposing
        ( Cons(..)
        , cons
        , uncons
        , singleton
        , toList
        , head
        , tail
        , minimum
        , maximum
        , foldl1
        , foldr1
        , scanl1
        , fromList
        , consWithMaybe
        , unconsWithMaybe
        , maybeTail
        , maybeToList
        , forList
        , reverse
        , append
        , appendList
        , appendToList
        , concat
        , intersperse
        , unzip
        , map
        , map2
        , map3
        , map4
        , map5
        , concatMap
        , indexedMap
        , scanl
        , scanlList
        , sort
        , sortBy
        , sortWith
        , isEmpty
        , length
        , member
        , filter
        , take
        , drop
        , partition
        , filterMap
        , foldl
        , foldr
        , sum
        , product
        , all
        , any
        )

{-| This library provides a type for non-empty lists, called `Cons`.

Being able to encode non-emptiness in the type system can lead to simpler, clearer code.

For example, to find the largest element in a List, you have to account for the empty list, which complicates things:

    maximum : List comparable -> Maybe comparable
    maximum l =
      case l of
        [] -> Nothing
        first::rest -> Just <| List.foldl max first rest

Using Cons, on the other hand, the type system knows the list will never be empty, leading to much simpler code:

    maximum : Cons comparable -> comparable
    maximum = foldl1 max


# Basics

@docs Cons, cons, uncons, singleton, toList


# Avoiding Maybe

Some functions on Lists are forced to use Maybe to handle the empty list. The following functions are quivalent to their List counterparts, but with no need for Maybe.

@docs head, tail, minimum, maximum


# Convenient Folding

Folds over Lists require a start value, but the following fold functions take the start value from the cons.

@docs foldl1, foldr1, scanl1


# List May Be Cons

A cons can't be empty, but a `Maybe (Cons a)` can be, if we treat Nothing as empty.

Thus `List a` and `Maybe (Cons a)` are completely equivalent, and the following functions let you go back and forth between them.

This is useful for recursion on Cons. For example, to recursively find the maximum element of a cons:

    maximum : Cons comparable -> comparable
    maximum c =
      case unconsWithMaybe c of
        (first, Nothing) -> first
        (first, Just rest) -> max first <| maximum rest

@docs fromList, consWithMaybe, unconsWithMaybe, maybeTail, maybeToList, forList


# Preserving Non-Emptiness

The following functions preserve non-emptiness, so given a cons they return a cons.

@docs reverse, append, appendList, appendToList, concat, intersperse, unzip, map, map2, map3, map4, map5, concatMap, indexedMap, scanl, scanlList, sort, sortBy, sortWith


# List Functions

Every function from the List library has been adapted to Cons.

The following are just convenience functions which convert the cons to a list and then apply the corresponding list function. For example, the definition of `sum` is:

    sum = toList >> List.sum

@docs isEmpty, length, member, filter, take, drop, partition, filterMap, foldl, foldr, sum, product, all, any
-}

import List


{-| A non-empty list of elements of type `a`.
-}
type Cons a
    = Cons a (List a)



-- Basics


{-| A cons with the given head and tail. Equivalent to ::

    c = cons 1 [2, 3]
    head c == 1
    tail c == [2, 3]

-}
cons : a -> List a -> Cons a
cons =
    Cons


{-| The head and tail of the cons.

    c = cons 1 [2, 3]
    uncons c == (1, [2, 3])
-}
uncons : Cons a -> ( a, List a )
uncons (Cons head tail) =
    ( head, tail )


{-| A cons containing only the given element.

    c = singleton "a"
    toList c == ["a"]
-}
singleton : a -> Cons a
singleton x =
    cons x []


{-| Convert the cons to the equivalent list.

    c = cons 1 [2, 3]
    toList c == [1, 2, 3]
-}
toList : Cons a -> List a
toList (Cons head tail) =
    head :: tail



-- Avoiding Maybe


{-| The first element of the cons.

    c = cons 1 [2, 3]
    head c == 1
-}
head : Cons a -> a
head (Cons head _) =
    head


{-| The list of all elements after the first element of the cons.

    c = cons 1 [2, 3]
    tail c == [2, 3]
-}
tail : Cons a -> List a
tail (Cons _ tail) =
    tail


{-| The smallest element of the cons.

    c = cons 1 [2, 3]
    minimum c == 1

    minimum == foldl1 min
-}
minimum : Cons comparable -> comparable
minimum =
    foldl1 min


{-| The largest element of the cons.

    c = cons 1 [2, 3]
    maximum c == 3

    maximum == foldl1 max
-}
maximum : Cons comparable -> comparable
maximum =
    foldl1 max



-- Convenient Folding


{-| Reduce the cons from the left.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldl1 step c == "abc"
-}
foldl1 : (a -> a -> a) -> Cons a -> a
foldl1 f (Cons head tail) =
    List.foldl f head tail


{-| Reduce the cons from the right.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldr1 step c == "cba"
-}
foldr1 : (a -> a -> a) -> Cons a -> a
foldr1 f c =
    case unconsWithMaybe c of
        ( head, Nothing ) ->
            head

        ( head, Just tail ) ->
            f head <| foldr1 f tail


{-| Reduce the cons from the left, producing a cons of all intermediate results.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    scanl1 step c == cons "a" ["ab", "abc"]
-}
scanl1 : (a -> a -> a) -> Cons a -> Cons a
scanl1 f (Cons head tail) =
    scanlList f head tail



-- List May Be Cons


{-| Convert the list to the equivalent cons, or Nothing for the empty list.

    fromList [] == Nothing
    fromList [1, 2, 3] == Just <| cons 1 [2, 3]
-}
fromList : List a -> Maybe (Cons a)
fromList l =
    case l of
        [] ->
            Nothing

        head :: tail ->
            Just <| cons head tail


{-| A cons with the given head and tail.

    c = consWithMaybe "a" Nothing
    toList c == ["a"]

    d = consWithMaybe 1 <| Just <| consWithMaybe 2 <| Just <| consWithMaybe 3 Nothing
    toList d = [1, 2, 3]
-}
consWithMaybe : a -> Maybe (Cons a) -> Cons a
consWithMaybe head tail =
    cons head <| maybeToList tail


{-| The head and tail of the cons.

    c = consWithMaybe "a" Nothing
    unconsWithMaybe c == ("a", Nothing)

    d = consWithMaybe 1 <| Just <| consWithMaybe 2 <| Just <| consWithMaybe 3 Nothing
    unconsWithMaybe d == (1, Just <| consWithMaybe 2 <| Just <| consWithMaybe 3 Nothing)

    maximum : Cons comparable -> comparable
    maximum c =
      case unconsWithMaybe c of
        (first, Nothing) -> first
        (first, Just rest) -> max first <| maximum rest
-}
unconsWithMaybe : Cons a -> ( a, Maybe (Cons a) )
unconsWithMaybe (Cons head tail) =
    ( head, fromList tail )


{-| The tail of the cons.

    c = consWithMaybe "a" Nothing
    maybeTail c == Nothing

    d = consWithMaybe 1 <| Just <| consWithMaybe 2 <| Just <| consWithMaybe 3 Nothing
    maybeTail d == Just <| consWithMaybe 2 <| Just <| consWithMaybe 3 Nothing

    length : Cons a -> Int
    length c =
      case maybeTail c of
        Nothing -> 1
        Just rest -> 1 + length rest
-}
maybeTail : Cons a -> Maybe (Cons a)
maybeTail =
    tail >> fromList


{-| Convert the cons to the equivalent list, or the empty list for Nothing.

This is the inverse of fromList.

    c = fromList []
    c == Nothing
    maybeToList c == []

    c = fromList [1, 2, 3]
    c == Just <| cons 1 [2, 3]
    maybeToList c == [1, 2, 3]
-}
maybeToList : Maybe (Cons a) -> List a
maybeToList =
    Maybe.map toList >> Maybe.withDefault []


{-| Convert a function that operates on Cons to a function that operates on List, where the empty list results in Nothing.

    maximum : Cons comparable -> comparable
    maximum = foldl1 max

    listMaximum : List comparable -> Maybe comparable
    listMaximum = forList maximum

    listMaximum [] == Nothing
    listMaximum [1, 2, 3] == Just 3
-}
forList : (Cons a -> b) -> List a -> Maybe b
forList f =
    fromList >> Maybe.map f



-- Preserving Non-Emptiness


{-| Reverse the cons.

    c = cons 1 [2, 3]
    reverse c == cons 3 [2, 1]
-}
reverse : Cons a -> Cons a
reverse (Cons head tail) =
    appendToList (List.reverse tail) <| singleton head


{-| Append the second cons to the first.

    c = cons 1 [2, 3]
    d = cons 4 [5, 6]
    append c d == cons 1 [2, 3, 4, 5, 6]
-}
append : Cons a -> Cons a -> Cons a
append c d =
    let
        step x xs =
            cons x <| toList xs
    in
        foldr step d c


{-| Append a list to a cons.

    c = cons 1 [2, 3]
    l = [4, 5, 6]
    appendList c l == cons 1 [2, 3, 4, 5, 6]
-}
appendList : Cons a -> List a -> Cons a
appendList c l =
    case fromList l of
        Nothing ->
            c

        Just d ->
            append c d


{-| Append a cons to a list.

    l = [1, 2, 3]
    c = cons 4 [5, 6]
    appendToList l c == cons 1 [2, 3, 4, 5, 6]
-}
appendToList : List a -> Cons a -> Cons a
appendToList l d =
    case fromList l of
        Nothing ->
            d

        Just c ->
            append c d


{-| Concatenate a non-empty list of non-empty lists.

    c = cons 1 [2, 3]
    d = singleton 4
    e = cons 5 [6]
    cs = cons c [d, e]
    concat cs == cons 1 [2, 3, 4, 5, 6]

    concat == foldr1 append
-}
concat : Cons (Cons a) -> Cons a
concat =
    foldr1 append


{-| Intersperse the value between each element of the cons.

    c = cons "first" ["second", "third"]
    intersperse "and" c == cons "first" ["and", "second", "and", "third"]
-}
intersperse : a -> Cons a -> Cons a
intersperse x c =
    case uncons c of
        ( head, [] ) ->
            c

        ( head, tail ) ->
            cons head <| x :: List.intersperse x tail


{-| A tuple of each cons, corresponding to a cons of tuples.

    c = cons (1, "a") [(2, "b"), (3, "c")]
    unzip c == (cons 1 [2, 3], cons "a" ["b", "c"])
-}
unzip : Cons ( a, b ) -> ( Cons a, Cons b )
unzip (Cons ( x, y ) tail) =
    let
        ( xs, ys ) =
            List.unzip tail
    in
        ( cons x xs, cons y ys )


{-| Apply a function to each element of the cons.

    c = cons 1 [4, 9]
    map sqrt c == cons 1 [2, 3]
-}
map : (a -> b) -> Cons a -> Cons b
map f (Cons head tail) =
    cons (f head) <| List.map f tail


{-| Apply a function to each pair of elements, limited by the shortest cons.

    zip : Cons a -> Cons b -> Cons (a, b)
    zip = map2 (,)
    c = cons 1 [2, 3]
    d = cons "a" ["b", "c", "d", "e"]
    zip c d = cons (1, "a") [(2, "b"), (3, "c")]
-}
map2 : (a -> b -> c) -> Cons a -> Cons b -> Cons c
map2 f (Cons x xs) (Cons y ys) =
    cons (f x y) <| List.map2 f xs ys


{-| -}
map3 : (a -> b -> c -> d) -> Cons a -> Cons b -> Cons c -> Cons d
map3 f (Cons x xs) (Cons y ys) (Cons z zs) =
    cons (f x y z) <| List.map3 f xs ys zs


{-| -}
map4 : (a -> b -> c -> d -> e) -> Cons a -> Cons b -> Cons c -> Cons d -> Cons e
map4 f (Cons v vs) (Cons w ws) (Cons x xs) (Cons y ys) =
    cons (f v w x y) <| List.map4 f vs ws xs ys


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Cons a -> Cons b -> Cons c -> Cons d -> Cons e -> Cons f
map5 f (Cons v vs) (Cons w ws) (Cons x xs) (Cons y ys) (Cons z zs) =
    cons (f v w x y z) <| List.map5 f vs ws xs ys zs


{-| Also known as "flat map", map each element of the cons to a cons, and then concatenate them together.

    f : number -> Cons number
    f x = cons x [-x]

    c = cons 1 [2, 3]
    concatMap f c == cons 1 [-1, 2, -2, 3, -3]

    concatMap f == concat << map f
-}
concatMap : (a -> Cons b) -> Cons a -> Cons b
concatMap f =
    concat << map f


{-| Apply a function to each element of the cons, as well as the index.

    c = cons "a" ["b", "c"]
    indexedMap (,) c == cons (0, "a") [(1, "b"), (2, "c")]
-}
indexedMap : (Int -> a -> b) -> Cons a -> Cons b
indexedMap f c =
    let
        go i c =
            consWithMaybe (f i <| head c) <| Maybe.map (go (i + 1)) <| maybeTail c
    in
        go 0 c


{-| Reduce the cons from the left, producing a cons of all intermediate results.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    scanl step "" c == cons "" ["a", "ab", "abc"]
-}
scanl : (a -> b -> b) -> b -> Cons a -> Cons b
scanl f x c =
    scanlList f x <| toList c


{-| Reduce the list from the left, producing a cons of all intermediate results, since even for the empty list there is one intermediate result.

Equivalent to List.scanl, but with a more specific return type.

    step value result = result ++ value
    scanlList step "" [] == cons "" []
    scanlList step "" ["a", "b", "c"] == cons "" ["a", "ab", "abc"]
-}
scanlList : (a -> b -> b) -> b -> List a -> Cons b
scanlList f x l =
    cons x <|
        case l of
            [] ->
                []

            head :: tail ->
                List.scanl f (f head x) tail


{-| Sort the cons in ascending order.

    c = cons 2 [3, 1]
    sort c == cons 1 [2, 3]
-}
sort : Cons comparable -> Cons comparable
sort =
    sortWith compare


{-| Sort the cons in ascending order, by applying the given function to each value.

    alice = {name="Alice", age=30}
    bob = {name="Bob", age=20}
    charlie = {name="Charlie", age=40}

    c = cons alice [bob, charlie]
    sortBy .age c == cons bob [alice, charlie]
-}
sortBy : (a -> comparable) -> Cons a -> Cons a
sortBy f =
    sortWith (\x y -> compare (f x) (f y))


{-| Sort the cons in ascending order, based on the given comparison function.

    reverseCompare : comparable -> comparable -> Order
    reverseCompare x y =
      case compare x y of
        LT -> GT
        EQ -> EQ
        GT -> LT

    c = cons "b" ["a", "c"]
    sortWith reverseCompare c == cons "c" ["b", "a"]

    sortWith compare == sort
-}
sortWith : (a -> a -> Order) -> Cons a -> Cons a
sortWith f c =
    insortWith f (head c) <| List.sortWith f <| tail c



-- insert a value into a sorted list


insortWith : (a -> a -> Order) -> a -> List a -> Cons a
insortWith f x l =
    case l of
        [] ->
            singleton x

        head :: tail ->
            if f x head == GT then
                cons head <| toList <| insortWith f x tail
            else
                cons x l



-- List Functions


{-| Always false for a cons, only here to make porting List code easier.

    isEmpty == always False
-}
isEmpty : Cons a -> Bool
isEmpty =
    toList >> List.isEmpty


{-| The number of elements in the cons.

    c = cons 1 [2, 3]
    length c == 3
-}
length : Cons a -> Int
length =
    toList >> List.length


{-| True if and only if the given element is in the given cons.

    c = cons 1 [2, 3]
    member 5 c == False
    member 2 c == True
-}
member : a -> Cons a -> Bool
member x =
    toList >> List.member x


{-| The list of elements from the cons which satisfy the given predicate. This can't generally be a cons itself, because it might be empty.

    c = cons 1 [2, 3]
    filter (\x -> x > 10) c == []
    filter (\x -> x > 1) c == [2, 3]
-}
filter : (a -> Bool) -> Cons a -> List a
filter f =
    toList >> List.filter f


{-| The first *n* elements of the cons, up to the length of the cons. This can't generally be a cons itself, since *n* might not be positive.

    c = cons "a" ["b", "c"]
    take 2 c == ["a", "b"]
    take 100 c == toList c
    take -10 c == []
-}
take : Int -> Cons a -> List a
take n =
    toList >> List.take n


{-| The cons without its first *n* elements. This can't generally be a cons itself, because it might be empty.

    c = cons "a" ["b", "c"]
    drop 2 c == ["c"]
    drop 100 c == []
    drop -10 c == toList c
-}
drop : Int -> Cons a -> List a
drop n =
    toList >> List.drop n


{-| Partition the cons into two lists, the first containing the elements which satisfy the given predicate, the second containing the elements which don't. These can't generally be a cons themselves, since one might be empty.

    c = cons 1 [2, 3]
    partition (\x -> x > 1) c == ([2, 3], [1])
-}
partition : (a -> Bool) -> Cons a -> ( List a, List a )
partition f =
    toList >> List.partition f


{-| Map the given Maybe function over the cons, discarding every Nothing. This can't generally be a cons itself, because it might be empty.

    String.toInt : String -> Maybe Int

    c = cons "1" ["a", "2", "b"]
    filterMap String.toInt c == [1, 2]
-}
filterMap : (a -> Maybe b) -> Cons a -> List b
filterMap f =
    toList >> List.filterMap f


{-| Reduce the cons from the right, starting with the given value. To start with the last value in the cons, use [foldr1](#foldr1).

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldr1 step "x" c == "xcba"
-}
foldr : (a -> b -> b) -> b -> Cons a -> b
foldr f x =
    toList >> List.foldr f x


{-| Reduce the cons from the left, starting with the given value. To start with the first value in the cons, use [foldl1](#foldl1).

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldl1 step "x" c == "xabc"
-}
foldl : (a -> b -> b) -> b -> Cons a -> b
foldl f x =
    toList >> List.foldl f x


{-| The sum of the elements of the cons.

    c = cons 2 [3, 4]
    sum c == 9
-}
sum : Cons number -> number
sum =
    toList >> List.sum


{-| The product of the elements of the cons.

    c = cons 2 [3, 4]
    product c == 24
-}
product : Cons number -> number
product =
    toList >> List.product


{-| True if and only if all elements of the cons satisfy the given predicate.

    c = cons 1 [2, 3]
    all (\x -> x > 2) == False
    all (\x -> x > 0) == True
-}
all : (a -> Bool) -> Cons a -> Bool
all f =
    toList >> List.all f


{-| True if and only if any elements of the cons satisfy the given predicate.

    c = cons 1 [2, 3]
    any (\x -> x > 5) == False
    any (\x -> x > 2) == True
-}
any : (a -> Bool) -> Cons a -> Bool
any f =
    toList >> List.any f
