module NonEmpty.List exposing (..)


type NonEmptyList a
    = NonEmptyList a (List a)


singleton : a -> NonEmptyList a
singleton a =
    NonEmptyList a []


push : a -> NonEmptyList a -> NonEmptyList a
push a (NonEmptyList x xs) =
    NonEmptyList a (x :: xs)


pop : NonEmptyList a -> ( a, List a )
pop (NonEmptyList x xs) =
    ( x, xs )


concat : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
concat (NonEmptyList x xs) (NonEmptyList y ys) =
    NonEmptyList x (xs ++ y :: ys)


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map fn (NonEmptyList x xs) =
    NonEmptyList (fn x) (List.map fn xs)


foldl : (a -> b -> b) -> NonEmptyList a -> b -> b
foldl fn (NonEmptyList x xs) b =
    List.foldl fn (fn x b) xs


toList : NonEmptyList a -> List a
toList (NonEmptyList x xs) =
    x :: xs
