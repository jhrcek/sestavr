module Id exposing
    ( Id
    , IdDict
    , buildDict
    , decode
    , emptyDict
    , fromInt
    , toString
    )

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)


type Id tag
    = Id Int


type alias IdDict tag a =
    AnyDict Int (Id tag) a


type alias Resource tag r =
    { r | id : Id tag }


fromInt : Int -> Id tag
fromInt =
    Id


emptyDict : IdDict tag a
emptyDict =
    Dict.Any.empty toInt


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
