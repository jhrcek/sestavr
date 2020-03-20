module Id exposing
    ( Id
    , IdDict
    , IdSet
    , buildDict
    , buildSet
    , decode
    , emptyDict
    , emptySet
    , fromInt
    , toString
    )

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Set.Any exposing (AnySet)


type Id tag
    = Id Int


type alias IdDict tag a =
    AnyDict Int (Id tag) a


type alias IdSet tag =
    AnySet Int (Id tag)


type alias Resource tag r =
    { r | id : Id tag }


fromInt : Int -> Id tag
fromInt =
    Id


buildDict : List (Resource tag r) -> IdDict tag (Resource tag r)
buildDict =
    Dict.Any.fromList toInt << List.map (\r -> ( r.id, r ))


toInt : Id tag -> Int
toInt (Id i) =
    i


toString : Id tag -> String
toString (Id i) =
    String.fromInt i


decode : Decoder (Id tag)
decode =
    Decode.map Id Decode.int


emptyDict : IdDict tag a
emptyDict =
    Dict.Any.empty toInt


emptySet : IdSet tag
emptySet =
    Set.Any.empty toInt


buildSet : List (Id tag) -> IdSet tag
buildSet =
    Set.Any.fromList toInt
