module Cons (..) where

import List


type Cons a = Cons a (List a)


-- Constructor and deconstructor

cons : a -> List a -> Cons a
cons = Cons

uncons : Cons a -> (a, List a)
uncons (Cons head tail) = (head, tail)


-- Convenience functions

singleton : a -> Cons a
singleton x = cons x []

toList : Cons a -> List a
toList (Cons head tail) = head::tail

foldr1 : (a -> a -> a) -> Cons a -> a
foldr1 f c =
  case uncons' c of
    (head, Nothing) -> head
    (head, Just tail) -> f head <| foldr1 f tail

foldl1 : (a -> a -> a) -> Cons a -> a
foldl1 f (Cons head tail) = List.foldl f head tail

scanl1 : (a -> a -> a) -> Cons a -> Cons a
scanl1 f (Cons head tail) = scanlList f head tail

scanlList : (a -> b -> b) -> b -> List a -> Cons b
scanlList f x l =
  cons x <| case l of
              [] -> []
              head::tail -> List.scanl f (f head x) tail


-- Maybe functions

fromList : List a -> Maybe (Cons a)
fromList l =
  case l of
    [] -> Nothing
    head::tail -> Just <| cons head tail

toList' : Maybe (Cons a) -> List a
toList' = Maybe.map toList >> Maybe.withDefault []

tail' : Cons a -> Maybe (Cons a)
tail' = tail >> fromList

cons' : a -> Maybe (Cons a) -> Cons a
cons' head tail = cons head <| toList' tail

uncons' : Cons a -> (a, Maybe (Cons a))
uncons' (Cons head tail) = (head, fromList tail)


-- List functions that avoid Maybe

head : Cons a -> a
head (Cons head _) = head

tail : Cons a -> List a
tail (Cons _ tail) = tail

maximum : Cons comparable -> comparable
maximum = foldl1 max

minimum : Cons comparable -> comparable
minimum = foldl1 min


-- List functions that preserve non-emptiness

reverse : Cons a -> Cons a
reverse (Cons head tail) =
  appendToList (List.reverse tail) <| singleton head

append : Cons a -> Cons a -> Cons a
append c d =
  let
    step x xs = cons x <| toList xs
  in
    foldr step d c

appendToList : List a -> Cons a -> Cons a
appendToList = fromList >> Maybe.map append >> Maybe.withDefault identity

appendList : Cons a -> List a -> Cons a
appendList c l =
  case fromList l of
    Nothing -> c
    Just d -> append c d

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


-- List methods

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
