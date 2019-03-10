# Cons

This library provides a type for non-empty lists, called `Cons`.

Being able to encode non-emptiness in the type system can lead to simpler, clearer code.

For example, to find the largest element in a List, you have to account for the empty list, which complicates things:
```elm
maximum : List comparable -> Maybe comparable
maximum l =
  case l of
    [] -> Nothing
    first::rest -> Just <| List.foldl max first rest
```

Using Cons, on the other hand, the type system knows the list will never be empty, leading to much simpler code:
```elm
maximum : Cons comparable -> comparable
maximum = foldl1 max
```

See [the full documentation](http://package.elm-lang.org/packages/hrldcpr/elm-cons/latest/Cons) for more.
