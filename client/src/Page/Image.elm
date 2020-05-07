module Page.Image exposing
    ( Config
    , view
    )

import Color
import Common exposing (heading1)
import Dict.Any
import Domain exposing (ExerciseIdTag, ImageVerificationResult)
import Element as E exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Id exposing (IdDict)
import List.Extra as List
import Router exposing (Route(..))


type alias Config msg =
    { deleteImage : String -> msg
    }


view : Config msg -> ImageVerificationResult -> Element msg
view config imageVerificationResult =
    E.column [ E.spacing 5 ]
        [ heading1 "Obrázky"
        , invalidLinksView imageVerificationResult.invalidLinks
        , unusedImagesView config imageVerificationResult.unusedImages
        , knownImagesView imageVerificationResult.knownImages
        ]


knownImagesView : List String -> Element msg
knownImagesView knownImages =
    if List.isEmpty knownImages then
        E.none

    else
        E.column []
            (E.text "Existující obrázky"
                :: List.map
                    (\group ->
                        E.row [] <|
                            List.map
                                (imagePreview smallImageSize)
                                group
                    )
                    (List.greedyGroupsOf 8 knownImages)
            )


imagePreview : Int -> String -> Element msg
imagePreview imageSize imageFileName =
    E.el
        [ E.width (E.px imageSize)
        , E.height (E.px imageSize)
        , Border.color Color.darkGrey
        , Border.width 1
        ]
    <|
        E.image
            [ E.width <| E.px <| imageSize - 2
            , E.height <| E.px <| imageSize - 2
            ]
            { src = imageFileName
            , description = imageFileName
            }


invalidLinksView : IdDict ExerciseIdTag v -> Element msg
invalidLinksView invalidLinks =
    if Dict.Any.isEmpty invalidLinks then
        E.none

    else
        E.column [ E.spacing 5 ]
            (E.text "Následující cviky obsahují odkazy na neexistující obrázky"
                :: List.map
                    (\exerciseId ->
                        E.link Common.linkAttrs
                            { url = Router.href (ExerciseEditor (Just exerciseId))
                            , label = E.text <| Id.toString exerciseId
                            }
                    )
                    (Dict.Any.keys invalidLinks)
            )


unusedImagesView : Config msg -> List String -> Element msg
unusedImagesView config unusedImages =
    if List.isEmpty unusedImages then
        E.none

    else
        E.column [ E.spacing 5 ]
            (E.text "Následující obrázky nejsou použity v žádném cviku"
                :: List.map
                    (\imageFileName ->
                        E.row [ E.spacing 10 ]
                            [ imagePreview largeImageSize imageFileName
                            , E.column [ E.spacing 5 ]
                                [ E.text imageFileName
                                , Input.button Common.coralButton
                                    { onPress = Just (config.deleteImage imageFileName)
                                    , label = E.text "Smazat"
                                    }
                                ]
                            ]
                    )
                    unusedImages
            )


largeImageSize : Int
largeImageSize =
    300


smallImageSize : Int
smallImageSize =
    150
