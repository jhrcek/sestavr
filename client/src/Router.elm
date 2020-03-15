module Router exposing
    ( Route(..)
    , parseUrl
    , toHash
    )

import Url exposing (Protocol(..), Url)
import Url.Parser as P exposing (Parser)


type Route
    = Home
    | Targets
    | Exercises
    | NotFound String


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home P.top
        , P.map Targets (P.s "target")
        , P.map Exercises (P.s "exercise")
        ]


parseUrl : Url -> Route
parseUrl url =
    let
        fragment =
            Maybe.withDefault "" url.fragment
    in
    P.parse route { url | path = fragment, fragment = Nothing }
        |> Maybe.withDefault (NotFound fragment)


toHash : Route -> String
toHash r =
    case r of
        Home ->
            ""

        Targets ->
            "target"

        Exercises ->
            "exercise"

        NotFound bad ->
            bad
