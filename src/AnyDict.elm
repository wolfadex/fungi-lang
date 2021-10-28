module AnyDict exposing
    ( AnyDict
    , diff
    , empty
    , filter
    , foldl
    , foldr
    , fromList
    , get
    , insert
    , intersect
    , isEmpty
    , keys
    , map
    , member
    , merge
    , partition
    , remove
    , singleton
    , size
    , toList
    , union
    , update
    , values
    )

import Dict exposing (Dict)
import String exposing (left)


type AnyDict comparable key value
    = AnyDict (Dict comparable value)


empty : AnyDict comparable key value
empty =
    AnyDict Dict.empty


singleton : { r | toComparable : key -> comparable } -> key -> value -> AnyDict comparable key value
singleton { toComparable } key value =
    AnyDict (Dict.singleton (toComparable key) value)


insert : { r | toComparable : key -> comparable } -> key -> value -> AnyDict comparable key value -> AnyDict comparable key value
insert { toComparable } key value (AnyDict dict) =
    AnyDict (Dict.insert (toComparable key) value dict)


update :
    { r | toComparable : key -> comparable }
    -> key
    -> (value -> value)
    -> AnyDict comparable key value
    -> AnyDict comparable key value
update { toComparable } key fn (AnyDict dict) =
    AnyDict (Dict.update (toComparable key) (Maybe.map fn) dict)


remove : { r | toComparable : key -> comparable } -> key -> AnyDict comparable key value -> AnyDict comparable key value
remove { toComparable } key (AnyDict dict) =
    AnyDict (Dict.remove (toComparable key) dict)


isEmpty : AnyDict comparable key value -> Bool
isEmpty (AnyDict dict) =
    Dict.isEmpty dict


member : { r | toComparable : key -> comparable } -> key -> AnyDict comparable key value -> Bool
member { toComparable } key (AnyDict dict) =
    Dict.member (toComparable key) dict


get : { r | toComparable : key -> comparable } -> key -> AnyDict comparable key value -> Maybe value
get { toComparable } key (AnyDict dict) =
    Dict.get (toComparable key) dict


size : AnyDict comparable key value -> Int
size (AnyDict dict) =
    Dict.size dict


keys : { r | fromComparable : comparable -> key } -> AnyDict comparable key value -> List key
keys { fromComparable } (AnyDict dict) =
    List.map fromComparable (Dict.keys dict)


values : AnyDict comparable key value -> List value
values (AnyDict dict) =
    Dict.values dict


toList : { r | fromComparable : comparable -> key } -> AnyDict comparable key value -> List ( key, value )
toList { fromComparable } (AnyDict dict) =
    List.map (\( key, val ) -> ( fromComparable key, val )) (Dict.toList dict)


fromList : { r | toComparable : key -> comparable } -> List ( key, value ) -> AnyDict comparable key value
fromList { toComparable } xs =
    AnyDict (Dict.fromList (List.map (\( key, val ) -> ( toComparable key, val )) xs))


map : { r | fromComparable : comparable -> key } -> (key -> a -> b) -> AnyDict comparable key a -> AnyDict comparable key b
map { fromComparable } fn (AnyDict dict) =
    AnyDict (Dict.map (\k -> fn (fromComparable k)) dict)


foldl : { r | fromComparable : comparable -> key } -> (key -> value -> b -> b) -> b -> AnyDict comparable key value -> b
foldl { fromComparable } fn b (AnyDict dict) =
    Dict.foldl (\k -> fn (fromComparable k)) b dict


foldr : { r | fromComparable : comparable -> key } -> (key -> value -> b -> b) -> b -> AnyDict comparable key value -> b
foldr { fromComparable } fn b (AnyDict dict) =
    Dict.foldr (\k -> fn (fromComparable k)) b dict


filter : { r | fromComparable : comparable -> key } -> (key -> value -> Bool) -> AnyDict comparable key value -> AnyDict comparable key value
filter { fromComparable } fn (AnyDict dict) =
    AnyDict (Dict.filter (\k -> fn (fromComparable k)) dict)


partition :
    { r | fromComparable : comparable -> key }
    -> (key -> value -> Bool)
    -> AnyDict comparable key value
    -> ( AnyDict comparable key value, AnyDict comparable key value )
partition { fromComparable } fn (AnyDict dict) =
    Tuple.mapBoth AnyDict AnyDict (Dict.partition (\k -> fn (fromComparable k)) dict)


union : AnyDict comparable key value -> AnyDict comparable key value -> AnyDict comparable key value
union (AnyDict left) (AnyDict right) =
    AnyDict (Dict.union left right)


intersect : AnyDict comparable key value -> AnyDict comparable key value -> AnyDict comparable key value
intersect (AnyDict left) (AnyDict right) =
    AnyDict (Dict.intersect left right)


diff : AnyDict comparable key a -> AnyDict comparable key b -> AnyDict comparable key a
diff (AnyDict left) (AnyDict right) =
    AnyDict (Dict.diff left right)


merge :
    { r | fromComparable : comparable -> key }
    -> (key -> a -> result -> result)
    -> (key -> a -> b -> result -> result)
    -> (key -> b -> result -> result)
    -> AnyDict comparable key a
    -> AnyDict comparable key b
    -> result
    -> result
merge { fromComparable } leftFn bothFn rightFn (AnyDict left) (AnyDict right) result =
    Dict.merge
        (\k -> leftFn (fromComparable k))
        (\k -> bothFn (fromComparable k))
        (\k -> rightFn (fromComparable k))
        left
        right
        result
