module Cons (..) where

{-| This library provides a type for non-empty lists, called `Cons`.

Being able to encode non-emptiness in the type system can lead to simpler, clearer code. For example, to find the largest element in a List, you have to account for the empty list, which complicates things:

    maximum : List comparable -> Maybe comparable
    maximum l =
      case l of
        [] -> Nothing
        first::rest -> Just <| List.foldl max first rest

Using Cons, on the other hand, the type system knows the list will never be empty, leading to much simpler code:

    maximum : Cons comparable -> comparable
    maximum = foldl1 max

Every function in the List package has been implemented for Cons, using the native List implementation whenever possible, for performance. And as illustrated above, all List functions that use Maybe--List.head, List.tail, List.maximum, and List.minimum--need no Maybe in their Cons equivalents. Simpler folding functions are also possible on non-empty lists--since no initial value need be provided--leading to the functions foldr1, foldl1, and scanl1.


# Basics

@docs Cons, cons, uncons, singleton, toList


# Simplified List Functions

Some functions on Lists are forced to use Maybe to handle the empty list. The following functions are quivalent to their List counterparts, but with no need for Maybe.

@docs head, tail, maximum, minimum

Folds over Lists require a start value, but the following fold functions automatically get the start value from the cons.

@docs foldr1, foldl1, scanl1


# Recursive Cons

Recursion on a Cons could be difficult, because the tail of a Cons is a List, not another Cons, since it might be empty. The following functions solve this problem by letting you treat a `List a` as a `Maybe (Cons a)`, and vice-versa.

For example if you wanted to recursively find the maximum element of a cons:

    maximum : Cons comparable -> comparable
    maximum c =
      case uncons' c of
        (first, Nothing) -> first
        (first, Just rest) -> max first <| maximum rest

@docs fromList, cons', uncons', tail', toList', forList


# List Functions Preserving Non-Emptiness

The following functions adapted from the List library preserve non-emptiness, so given a cons they return a cons.

@docs reverse, append, appendList, appendToList, concat, intersperse, unzip, map, map2, map3, map4, map5, indexedMap, scanl, scanlList, sort, sortBy, sortWith


# All Other List Functions

Every function from the List library has been adapted to Cons. The following are simply convenience functions which convert the cons to a list and then apply the corresponding list function.

For example, the definition of `sum` is simply:

    sum = toList >> List.sum

@docs isEmpty, length, member, filter, take, drop, partition, filterMap, concatMap, foldr, foldl, sum, product, all, any
-}

import List


{-| A non-empty list of elements of type `a`.
-}
type Cons a = Cons a (List a)


-- Basics

{-| A cons with the given head and tail. Equivalent to List.(::)

    c = cons 1 [2, 3]
    head c == 1
    tail c == [2, 3]

-}
cons : a -> List a -> Cons a
cons = Cons

{-| The head and tail of the cons.

    c = cons 1 [2, 3]
    uncons c == (1, [2, 3])
-}
uncons : Cons a -> (a, List a)
uncons (Cons head tail) = (head, tail)

{-| A cons containing only the given element.

    c = singleton "a"
    toList c == ["a"]
-}
singleton : a -> Cons a
singleton x = cons x []

{-| Convert the cons to the equivalent list.

    c = cons 1 [2, 3]
    toList c == [1, 2, 3]
-}
toList : Cons a -> List a
toList (Cons head tail) = head::tail


-- Simplified List Functions

{-| The first element of the cons. Equivalent to List.head, but with no need for Maybe.

    c = cons 1 [2, 3]
    head c == 1
-}
head : Cons a -> a
head (Cons head _) = head

{-| The list of all elements after the first element of the cons. Equivalent to List.tail, but with no need for Maybe.

    c = cons 1 [2, 3]
    tail c == [2, 3]
-}
tail : Cons a -> List a
tail (Cons _ tail) = tail

{-| The largest element of the cons. Equivalent to List.maximum, but with no need for Maybe.

    c = cons 1 [2, 3]
    maximum c == 3
-}
maximum : Cons comparable -> comparable
maximum = foldl1 max

{-| The smallest element of the cons. Equivalent to List.minimum, but with no need for Maybe.

    c = cons 1 [2, 3]
    minimum c == 1
-}
minimum : Cons comparable -> comparable
minimum = foldl1 min

{-| Reduce the cons from the right. Equivalent to foldr, but using the last element of the cons as the start value.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldr1 step c == "cba"
-}
foldr1 : (a -> a -> a) -> Cons a -> a
foldr1 f c =
  case uncons' c of
    (head, Nothing) -> head
    (head, Just tail) -> f head <| foldr1 f tail

{-| Reduce the cons from the left. Equivalent to foldl, but using the first element of the cons as the start value.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    foldl1 step c == "abc"
-}
foldl1 : (a -> a -> a) -> Cons a -> a
foldl1 f (Cons head tail) = List.foldl f head tail

{-| Reduce the cons from the left, producing a cons of all intermediate results. Equivalent to scanl, but using the first element of the cons as the start value.

    c = cons "a" ["b", "c"]
    step value result = result ++ value
    scanl1 step c == cons "a" ["ab", "abc"]
-}
scanl1 : (a -> a -> a) -> Cons a -> Cons a
scanl1 f (Cons head tail) = scanlList f head tail


-- Recursive Cons

{- Convert the list to the equivalent cons, or Nothing for the empty list.

    fromList [] == Nothing
    fromList [1, 2, 3] == Just <| cons 1 [2, 3]
-}
fromList : List a -> Maybe (Cons a)
fromList l =
  case l of
    [] -> Nothing
    head::tail -> Just <| cons head tail

{- A cons with the given head and tail. Equivalent to `cons`, but takes a tail of type `Maybe (Cons a)` instead of `List a`.

    c = cons' "a" Nothing
    toList c == ["a"]

    d = cons' 1 <| Just <| cons' 2 <| Just <| cons' 3 Nothing
    toList d = [1, 2, 3]
-}
cons' : a -> Maybe (Cons a) -> Cons a
cons' head tail = cons head <| toList' tail

{- The head and tail of the cons. Equivalent to `uncons`, but gives a tail of type `Maybe (Cons a)` instead of `List a`. This is useful for recursion on the Cons type.

    maximum c =
      case uncons' c of
        (first, Nothing) -> first
        (first, Just rest) -> max first <| maximum rest
-}
uncons' : Cons a -> (a, Maybe (Cons a))
uncons' (Cons head tail) = (head, fromList tail)

{- The tail of the cons. Equivalent to `tail`, but is a `Maybe (Cons a)` instead of a `List a`. This is useful for recursion on the Cons type.

    length c =
      case tail' c of
        Nothing -> 1
        Just rest -> 1 + length rest
-}
tail' : Cons a -> Maybe (Cons a)
tail' = tail >> fromList

{-| Convert the cons to the equivalent list, or the empty list for Nothing. This is the inverse of fromList.

    c = fromList []
    c == Nothing
    toList' c == []

    c = fromList [1, 2, 3]
    c == Just <| cons 1 [2, 3]
    toList' c == [1, 2, 3]
-}
toList' : Maybe (Cons a) -> List a
toList' = Maybe.map toList >> Maybe.withDefault []

{-| Convert a function that operates on Cons to a function that operates on List, where the empty list results in Nothing.

    maximum : Cons comparable -> comparable
    maximum = foldl1 max
    listMaximum : List comparable -> Maybe comparable
    listMaximum = forList maximum
    listMaximum [] == Nothing
    listMaximum [1, 2, 3] == 3
-}
forList : (Cons a -> b) -> List a -> Maybe b
forList f = fromList >> Maybe.map f


-- List Functions Preserving Non-Emptiness

reverse : Cons a -> Cons a
reverse (Cons head tail) =
  appendToList (List.reverse tail) <| singleton head

append : Cons a -> Cons a -> Cons a
append c d =
  let
    step x xs = cons x <| toList xs
  in
    foldr step d c

appendList : Cons a -> List a -> Cons a
appendList c l =
  case fromList l of
    Nothing -> c
    Just d -> append c d

appendToList : List a -> Cons a -> Cons a
appendToList = fromList >> Maybe.map append >> Maybe.withDefault identity

concat : Cons (Cons a) -> Cons a
concat (Cons head tail) =
  appendList head <| List.concatMap toList tail

intersperse : a -> Cons a -> Cons a
intersperse x c =
  case uncons c of
    (head, []) -> c
    (head, tail) -> cons head <| x :: List.intersperse x tail

unzip : Cons (a, b) -> (Cons a, Cons b)
unzip (Cons (x, y) tail) =
  let
    (xs, ys) = List.unzip tail
  in
    (cons x xs, cons y ys)

map : (a -> b) -> Cons a -> Cons b
map f (Cons head tail) = cons (f head) <| List.map f tail

map2 : (a -> b -> c) -> Cons a -> Cons b -> Cons c
map2 f (Cons x xs) (Cons y ys) = cons (f x y) <| List.map2 f xs ys

map3 : (a -> b -> c -> d) -> Cons a -> Cons b -> Cons c -> Cons d
map3 f (Cons x xs) (Cons y ys) (Cons z zs) = cons (f x y z) <| List.map3 f xs ys zs

map4 : (a -> b -> c -> d -> e) -> Cons a -> Cons b -> Cons c -> Cons d -> Cons e
map4 f (Cons v vs) (Cons w ws) (Cons x xs) (Cons y ys) = cons (f v w x y) <| List.map4 f vs ws xs ys

map5 : (a -> b -> c -> d -> e -> f) -> Cons a -> Cons b -> Cons c -> Cons d -> Cons e -> Cons f
map5 f (Cons v vs) (Cons w ws) (Cons x xs) (Cons y ys) (Cons z zs) = cons (f v w x y z) <| List.map5 f vs ws xs ys zs

indexedMap : (Int -> a -> b) -> Cons a -> Cons b
indexedMap f c =
  let
    go i c = cons' (f i <| head c) <| Maybe.map (go (i + 1)) <| tail' c
  in
    go 0 c

scanl : (a -> b -> b) -> b -> Cons a -> Cons b
scanl f x c = scanlList f x <| toList c

scanlList : (a -> b -> b) -> b -> List a -> Cons b
scanlList f x l =
  cons x <| case l of
              [] -> []
              head::tail -> List.scanl f (f head x) tail


-- All Other List Functions

isEmpty = toList >> List.isEmpty
length = toList >> List.length
member x = toList >> List.member x
filter f = toList >> List.filter f
take n = toList >> List.take n
drop n = toList >> List.drop n
partition f = toList >> List.partition f
filterMap f = toList >> List.filterMap f
concatMap f = toList >> List.concatMap f
foldr f x = toList >> List.foldr f x
foldl f x = toList >> List.foldl f x
sum = toList >> List.sum
product = toList >> List.product
all f = toList >> List.all f
any f = toList >> List.any f
sort = toList >> List.sort
sortBy f = toList >> List.sortBy f
sortWith f = toList >> List.sortWith f
