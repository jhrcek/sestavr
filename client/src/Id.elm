module Id exposing
    ( Id
    , IdDict
    , IdSet
    , buildDict
    , buildSet
    , decode
    , dictFromList
    , emptyDict
    , emptySet
    , encode
    , fromInt
    , toString
    )

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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


dictFromList : List ( Id tag, a ) -> IdDict tag a
dictFromList =
    Dict.Any.fromList toInt


toInt : Id tag -> Int
toInt (Id i) =
    i


toString : Id tag -> String
toString (Id i) =
    String.fromInt i


decode : Decoder (Id tag)
decode =
    Decode.map Id Decode.int


encode : Id tag -> Value
encode (Id i) =
    Encode.int i


emptyDict : IdDict tag a
emptyDict =
    Dict.Any.empty toInt


emptySet : IdSet tag
emptySet =
    Set.Any.empty toInt


buildSet : List (Id tag) -> IdSet tag
buildSet =
    Set.Any.fromList toInt
