module Http.Extra exposing
    ( Error(..)
    , errorToString
    , expectJson
    , expectWhatever
    )

import Http exposing (Metadata, Response(..))
import Json.Decode as Decode exposing (Decoder)


{-| Like Http.Error, but with BadStatus carrying full Metadata and response body
-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Metadata String
    | BadBody String


expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg (fromResponse decoder)


expectWhatever : (Result Error () -> msg) -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg (fromResponse (Decode.succeed ()))


fromResponse : Decoder a -> Response String -> Result Error a
fromResponse bodyDecoder response =
    case response of
        BadUrl_ url ->
            Err (BadUrl url)

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        GoodStatus_ _ body ->
            case Decode.decodeString bodyDecoder body of
                Ok value ->
                    Ok value

                Err decodeErr ->
                    Err (BadBody (Decode.errorToString decodeErr))


errorToString : Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "Špatná url : " ++ url

        Timeout ->
            "Čas požadavku vypršel"

        NetworkError ->
            "Problém se sítí"

        BadStatus _ body ->
            body

        BadBody body ->
            body
