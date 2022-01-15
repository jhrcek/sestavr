{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model (
    Exercise,
    ExerciseId,
    ExerciseTagId,
    ExerciseTag (..),
    EntityField (
        ExerciseTagExerciseId,
        RoutineExerciseRoutineId
    ),
    ImageVerificationResult (..),
    Inspiration,
    InspirationId,
    Lesson,
    LessonId,
    Position,
    PositionId,
    RoutineWithExercises,
    RoutineId,
    RoutineExercise (..),
    Routine,
    RoutineExerciseId,
    Tag,
    TagId,
    ExerciseWithTags,
    createDemoData,
    eirDuration,
    eirExerciseId,
    exerciseDescription,
    exerciseId,
    exerciseImage,
    fromExercise,
    fromRoutine,
    getDurationMinutes,
    migrateAll,
    routineId,
    rweExercises,
    tagIds,
    toExercise,
    toRoutine,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import qualified Data.List as List
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Class (EntityField, insert)
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Database.Persist.Types (Entity, entityKey, entityVal)
import GHC.Generics (Generic)


share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Tag json
    name Text
    UniqueTagName name
Position json
    name Text
    UniquePositionName name
Exercise json
    name Text
    sanskritName Text Maybe
    image Text Maybe
    description Text
    positionId PositionId
    UniqueExerciseName name
ExerciseTag json
    exerciseId ExerciseId
    tagId TagId
    Primary exerciseId tagId
Routine json
    topic Text
RoutineExercise json
    routineId RoutineId
    exerciseId ExerciseId
    durationMin Int
    order Int
    UniqueExerciseRoutineOrder exerciseId routineId order
Lesson json
    routineId RoutineId
    datetime UTCTime
Inspiration json
    monthNumber Int
    description Text
    UniqueInspirationMonthNumber monthNumber
|]


-- This is to alleviate frontend from having to join TagIds from join table
data ExerciseWithTags = ExerciseWithTags
    { exerciseId :: ExerciseId
    , name :: Text
    , sanskritName :: Maybe Text
    , image :: Maybe Text
    , description :: Text
    , positionId :: PositionId
    , tagIds :: [TagId]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)


fromExercise :: Entity Exercise -> [TagId] -> ExerciseWithTags
fromExercise entity tagIds =
    let exercise = entityVal entity
        exerciseId = entityKey entity
     in ExerciseWithTags
            { exerciseId = exerciseId
            , name = exerciseName exercise
            , sanskritName = exerciseSanskritName exercise
            , image = exerciseImage exercise
            , description = exerciseDescription exercise
            , positionId = exercisePositionId exercise
            , tagIds = tagIds
            }


toExercise :: ExerciseWithTags -> Exercise
toExercise ewt =
    Exercise
        { exerciseName = name ewt
        , exerciseSanskritName = sanskritName ewt
        , exerciseImage = image ewt
        , exerciseDescription = description ewt
        , exercisePositionId = positionId ewt
        }


-- This is to alleviate frontend from having to join Exercises from RoutineExercises join table
data RoutineWithExercises = RoutineWithExercises
    { routineId :: RoutineId
    , topic :: Text
    , rweExercises :: [ExerciseInRoutine]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)


data ExerciseInRoutine = ExerciseInRoutine
    { eirExerciseId :: ExerciseId
    , eirDuration :: DurationMinutes
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)


newtype DurationMinutes = DurationMinutes {getDurationMinutes :: Int}
    deriving (ToJSON, FromJSON) via Int


fromRoutine :: Entity Routine -> [RoutineExercise] -> RoutineWithExercises
fromRoutine entity res =
    let routine = entityVal entity
        routineId = entityKey entity
     in RoutineWithExercises
            { routineId = routineId
            , topic = routineTopic routine
            , rweExercises =
                ( \re ->
                    ExerciseInRoutine
                        (routineExerciseExerciseId re)
                        (DurationMinutes $ routineExerciseDurationMin re)
                )
                    <$> List.sortOn routineExerciseOrder res
            }


toRoutine :: RoutineWithExercises -> Routine
toRoutine rwe =
    Routine
        { routineTopic = topic rwe
        }


data ImageVerificationResult = ImageVerificationResult
    { -- | images being referenced in Exercises but without corresponding file in the images directory
      invalidLinks :: [(ExerciseId, [FilePath])]
    , -- | image files in images directory, which are not linked from any exercise
      unusedImages :: [FilePath]
    , knownImages :: [FilePath]
    }
    deriving stock (Generic)


instance ToJSON ImageVerificationResult where
    toJSON (ImageVerificationResult invalidLinks_ unusedImages_ knownImages_) =
        object
            [ "invalidLinks"
                .= List.map
                    ( \(exId, imgs) ->
                        object
                            [ "exerciseId" .= exId
                            , "images" .= imgs
                            ]
                    )
                    invalidLinks_
            , "unusedImages" .= unusedImages_
            , "knownImages" .= knownImages_
            ]


createDemoData :: IO ()
createDemoData = runSqlite "sestavr.db" $ do
    runMigration migrateAll

    breathId <- insert $ Tag "Dech"
    _feetId <- insert $ Tag "Chodidla"
    hipId <- insert $ Tag "Kyčle"
    backId <- insert $ Tag "Záda"
    _headId <- insert $ Tag "Hlava"

    sitId <- insert $ Position "Vsedě"
    standId <- insert $ Position "Ve stoji"
    kneelId <- insert $ Position "V kleku"
    lyingFrontId <- insert $ Position "Na břiše"
    lyingBackId <- insert $ Position "Na zádech"
    handSupportedId <- insert $ Position "S oporou paží"

    boatId <- insert $ Exercise "Pozice loďky" (Just "Navasana") (Just "Navasana.png") "" sitId
    childId <- insert $ Exercise "Pozice dítěte" (Just "Balasana") (Just "Balasana.png") "" kneelId

    mapM_
        insert
        [ Exercise "Pes hlavou dolů / střecha" (Just "Adho Mukha Svanasana") (Just "AdhoMukhaSvanasana.png") "" handSupportedId
        , Exercise "Stojka" (Just "Adho Mukha Vrksasana") (Just "AdhoMukhaVrksasana.png") "" handSupportedId
        , Exercise "Ananda Balasana" (Just "Ananda Balasana") (Just "AnandaBalasana.png") "" lyingBackId
        , Exercise "Pozice ležícího Višny" (Just "Anantasana") (Just "Anantasana.png") "" lyingBackId
        , Exercise "Pozice Apány" (Just "Apanasana") (Just "Apanasana.png") "" lyingBackId
        , Exercise "Poloviční pozice krále rybářů" (Just "Ardha Matsyendrasana") (Just "ArdhaMatsyendrasana.png") "" sitId
        , Exercise "Ardha Padmasana" (Just "Ardha Padmasana") (Just "ArdhaPadmasana.png") "" sitId
        , Exercise "Ardha Purvottanasana" (Just "Ardha Purvottanasana") (Just "ArdhaPurvottanasana.png") "" handSupportedId
        , Exercise "Pozice osmi úhlů" (Just "Astavakrasana") (Just "Astavakrasana.png") "" handSupportedId
        , Exercise "Pozice spojeného úhlu" (Just "Baddha Konasana") (Just "BaddhaKonasana.png") "" sitId
        , Exercise "Pozice volavky" (Just "Bakasana") (Just "Bakasana.png") "" handSupportedId
        , Exercise "Pozice kobry" (Just "Bhujangasana") (Just "Bhujangasana.png") "" lyingFrontId
        , Exercise "Pozice tyče s oporou čtyř končetin" (Just "Chaturanga Dandasana") (Just "ChaturangaDandasana.png") "" handSupportedId
        , Exercise "Pozice tyče" (Just "Dandasana") (Just "Dandasana.png") "" sitId
        , Exercise "Pozice luku" (Just "Dhanurasana") (Just "Dhanurasana.png") "" lyingFrontId
        , Exercise "Dwi Pada Pitham" (Just "Dwi Pada Pitham") (Just "DwiPadaPitham.png") "" lyingBackId
        , Exercise "Pozice jednonohého královského holuba" (Just "Eka Pada Rajakapotasana") (Just "EkaPadaRajakapotasana.png") "" kneelId
        , Exercise "Pozice orla" (Just "Garudasana") (Just "Garudasana.png") "" standId
        , Exercise "Pozice kravího obličeje" (Just "Gomukhasana") (Just "Gomukhasana.png") "" sitId
        , Exercise "Pozice pluhu" (Just "Halasana") (Just "Halasana.png") "" lyingBackId
        , Exercise "Pozice opice" (Just "Hanumanasana") (Just "Hanumanasana.png") "" sitId
        , Exercise "Pozice s hlavou na koleni" (Just "Janu Sirsasana") (Just "JanuSirsasana.png") "" sitId
        , Exercise "Břišní zkrut" (Just "Jathara Parivrtti") (Just "JatharaParivrtti.png") "" lyingBackId
        , Exercise "Pozice s koleny vedle uší" (Just "Karnapidasana") (Just "Karnapidasana.png") "" lyingBackId
        , Exercise "Pozice želvy" (Just "Kurmasana") (Just "Kurmasana.png") "" sitId
        , Exercise "Pozice velké pečeti" (Just "Mahamudra Asana") (Just "MahamudraAsana.png") "" sitId
        , Exercise "Pozice ve dřepu a sedu" (Just "Malasana") (Just "Upavesasana.png") "" standId
        , Exercise "Marjariasana" (Just "Marjariasana") (Just "Marjariasana.jpg") "" kneelId
        , Exercise "Pozice ryby" (Just "Matsyasana") (Just "Matsyasana.png") "" lyingBackId
        , Exercise "Pozice páva" (Just "Mayurasana") (Just "Mayurasana.png") "" handSupportedId
        , Exercise "Pozice kořenového zámku" (Just "Mulabandhasana") (Just "Mulabandhasana.png") "" sitId
        , Exercise "Pozice krále tanečníků" (Just "Natarajasana") (Just "Natarajasana.png") "" standId
        , Exercise "Stoj na ramenou bez opory rukou" (Just "Niralamba Sarvangasana") (Just "NiralambaSarvangasana.png") "" lyingBackId
        , Exercise "Pozice lotosu" (Just "Padmasana") (Just "Padmasana.png") "" sitId
        , Exercise "Pozice petlice" (Just "Parighansana") (Just "Parighansana.png") "" kneelId
        , Exercise "Úklon do strany s otočením" (Just "Parivrtta Baddha Parsvakonasana") (Just "ParivrttaBaddhaParsvakonasana.png") "" standId
        , Exercise "Obrácená pozice s hlavou na koleni" (Just "Parivrtta Janu Sirsasana") (Just "ParivrttaJanuSirsasana.png") "" sitId
        , Exercise "Pozice otočeného trojúhelníku" (Just "Parivrtta Trikonasana") (Just "ParivrttaTrikonasana.png") "" standId
        , Exercise "Boční pozice volavky" (Just "Parsva Bakasana") (Just "ParsvaBakasana.png") "" handSupportedId
        , Exercise "Intenzivní protažení k jedné noze" (Just "Parsvottanasana") (Just "Parsvottanasana.png") "" standId
        , Exercise "Zadní (zádové) protažení" (Just "Paschimottanasana") (Just "Paschimottanasana.png") "" sitId
        , Exercise "Pozice opeřeného páva" (Just "Pincha Mayurasana") (Just "PinchaMayurasana.png") "" handSupportedId
        , Exercise "Předklon v širokém postoji" (Just "Prasarita Padottanasana") (Just "PrasaritaPadottanasana.png") "" standId
        , Exercise "Pozice vzestupného sloupu" (Just "Purvottanasana") (Just "Purvottanasana.png") "" handSupportedId
        , Exercise "Pozice kobylky" (Just "Salabhasana") (Just "Salabhasana.png") "" lyingFrontId
        , Exercise "Stoj na ramenou s oporou" (Just "Salamba Sarvangasana") (Just "SalambaSarvangasana.png") "" lyingBackId
        , Exercise "Stoj na hlavě s oporou" (Just "Salamba Sirsasana") (Just "SalambaSirsasana.png") "" sitId
        , Exercise "Samasthiti" (Just "Samasthiti") (Just "Samasthiti.png") "" standId
        , Exercise "Pozice mrtvoly" (Just "Savasana") (Just "Savasana.png") "" lyingBackId
        , Exercise "Pozice mostu" (Just "Setu Bandhasana") (Just "SetuBandhasana.png") "" lyingBackId
        , Exercise "Pozice mistra" (Just "Siddhasana") (Just "Siddhasana.png") "" sitId
        , Exercise "Pozice lva" (Just "Simhasana") (Just "Simhasana.png") "" kneelId
        , Exercise "Jednoduchá pozice" (Just "Sukhasana") (Just "Sukhasana.png") "" sitId
        , Exercise "Supta Baddha Konasana" (Just "Supta Baddha Konasana") (Just "SuptaBaddhaKonasana.png") "" lyingFrontId
        , Exercise "Supta Kurmasana" (Just "Supta Kurmasana") (Just "SuptaKurmasana.png") "" sitId
        , Exercise "Pozice ležicího hrdiny" (Just "Supta Virasana") (Just "SuptaVirasana.png") "" kneelId
        , Exercise "Šťastná pozice" (Just "Svastikasana") (Just "Svastikasana.png") "" sitId
        , Exercise "Pozice hory" (Just "Tadasana") (Just "Tadasana.png") "" standId
        , Exercise "Předklon vsedě s roztaženýma nohama" (Just "Upavistha Konasana") (Just "UpavisthaKonasana.png") "" sitId
        , Exercise "Pozice kola / horního luku" (Just "Urdhva Dhanurasana") (Just "UrdhvaDhanurasana.png") "" handSupportedId
        , Exercise "Pozice psa s obličejem nahoru" (Just "Urdhva Mukha Svanasana") (Just "UrdhvaMukhaSvanasana.png") "" handSupportedId
        , Exercise "Pozice velblouda" (Just "Ustrasana") (Just "Ustrasana.png") "" kneelId
        , Exercise "Pozice židle / neobratná pozice" (Just "Utkatasana") (Just "Utkatasana.png") "" standId
        , Exercise "Předklon ve stoji" (Just "Uttanasana") (Just "Uttanasana.png") "" standId
        , Exercise "Protažení nohy ve stoji" (Just "Utthita Hasta Padangusthasana") (Just "UtthitaHastaPadangusthasana.png") "" standId
        , Exercise "Úklon do strany s natažením" (Just "Utthita Parsvakonasana") (Just "UtthitaParsvakonasana.png") "" standId
        , Exercise "Rozšížená pozice trojúhelníku" (Just "Utthita Trikonasana") (Just "UtthitaTrikonasana.png") "" standId
        , Exercise "Diamantový sed" (Just "Vajrasana") (Just "Vajrasana.png") "" sitId
        , Exercise "Pozice mudrce Vasišthy" (Just "Vasisthasana") (Just "Vasisthasana.png") "" handSupportedId
        , Exercise "Obrácená pozice" (Just "Viparita Karani") (Just "ViparitaKarani.png") "" lyingBackId
        , Exercise "Pozice úplné kobylky" (Just "Viparita Salabhasana") (Just "ViparitaSalabhasana.png") "" lyingFrontId
        , Exercise "Pozice bojovníka III" (Just "Virabhadrasana III") (Just "VirabhadrasanaIII.png") "" standId
        , Exercise "Pozice bojovníka II" (Just "Virabhadrasana II") (Just "VirabhadrasanaII.png") "" standId
        , Exercise "Pozice bojovníka I" (Just "Virabhadrasana I") (Just "VirabhadrasanaI.png") "" standId
        , Exercise "Pozice hrdiny" (Just "Virasana") (Just "Virasana.png") "" kneelId
        , Exercise "Vjaghrasana" (Just "Vjaghrasana") (Just "Vjaghrasana.png") "" kneelId
        , Exercise "Pozice stromu" (Just "Vrksasana") (Just "Vrksasana.png") "" standId
        , Exercise "Pozice škorpiona" (Just "Vrschikasana") (Just "Vrschikasana.png") "" handSupportedId
        ]

    _ <- insert $ ExerciseTag childId hipId
    _ <- insert $ ExerciseTag childId backId
    _ <- insert $ ExerciseTag childId breathId

    routine1Id <- insert $ Routine "Moje první sestava"

    _ <- insert $ RoutineExercise routine1Id childId 5 2
    _ <- insert $ RoutineExercise routine1Id boatId 1 3

    currentTime <- liftIO getCurrentTime
    _ <- insert $ Lesson routine1Id currentTime

    mapM_ insert [Inspiration i "" | i <- [1 .. 12]]
