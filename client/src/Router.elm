module Router exposing
    ( Route(..)
    , href
    , parseUrl
    , toHash
    )

import Domain exposing (ExerciseId)
import Id
import Url exposing (Protocol(..), Url)
import Url.Parser as P exposing ((</>), Parser, int, s, top)


type Route
    = Home
    | Targets
    | Positions
    | Exercises
    | Exercise ExerciseId
    | ExerciseEditor ExerciseId
    | NotFound String


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home top
        , P.map Targets (s "target")
        , P.map Positions (s "position")
        , P.map Exercises (s "exercise")
        , P.map (Exercise << Id.fromInt) (s "exercise" </> int)
        , P.map (ExerciseEditor << Id.fromInt) (s "exercise" </> int </> s "edit")
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
            "/"

        Targets ->
            "/target"

        Positions ->
            "/position"

        Exercises ->
            "/exercise"

        Exercise exerciseId ->
            "/exercise/" ++ Id.toString exerciseId

        ExerciseEditor exerciseId ->
            "/exercise/" ++ Id.toString exerciseId ++ "/edit"

        NotFound bad ->
            bad


href : Route -> String
href r =
    "#" ++ toHash r
