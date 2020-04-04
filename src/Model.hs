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

module Model
  ( Exercise,
    ExerciseId,
    ExerciseTargetId,
    ExerciseTarget (..),
    EntityField
      ( ExerciseTargetExerciseId,
        RoutineExerciseRoutineId
      ),
    Lesson,
    LessonId,
    Position,
    PositionId,
    RoutineWithExercises,
    RoutineId,
    RoutineExercise (..),
    Routine,
    RoutineExerciseId,
    Target,
    TargetId,
    ExerciseWithTargets,
    createDemoData,
    migrateAll,
    routineId,
    eirDuration,
    fromExercise,
    toExercise,
    targetIds,
    fromRoutine,
    exerciseId,
    toRoutine,
    exercises,
    getDurationMinutes,
    eirExerciseId,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List as List
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.Types (entityKey, entityVal)
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Target json
    name Text
    UniqueTargetName name
Position json
    name Text
    UniquePositionName name
Exercise json
    name Text
    sanskritName Text Maybe
    description Text
    positionId PositionId
    UniqueExerciseName name
ExerciseTarget json
    exerciseId ExerciseId
    targetId TargetId
    Primary exerciseId targetId
Routine json
    topic Text
RoutineExercise json
    routineId RoutineId
    exerciseId ExerciseId
    durationMin Int
    order Int
    UniqueExerciseRoutineOrder exerciseId routineId order
Lesson json
    routine RoutineId
    datetime UTCTime
|]

-- This is to alleviate frontend from having to join TargetIds from join table
data ExerciseWithTargets
  = ExerciseWithTargets
      { exerciseId :: ExerciseId,
        name :: Text,
        sanskritName :: Maybe Text,
        description :: Text,
        positionId :: PositionId,
        targetIds :: [TargetId]
      }
  deriving (Generic)

instance ToJSON ExerciseWithTargets

instance FromJSON ExerciseWithTargets

fromExercise :: Entity Exercise -> [TargetId] -> ExerciseWithTargets
fromExercise entity targetIds =
  let exercise = entityVal entity
      exerciseId = entityKey entity
   in ExerciseWithTargets
        { exerciseId = exerciseId,
          name = exerciseName exercise,
          sanskritName = exerciseSanskritName exercise,
          description = exerciseDescription exercise,
          positionId = exercisePositionId exercise,
          targetIds = targetIds
        }

toExercise :: ExerciseWithTargets -> Exercise
toExercise ewt =
  Exercise
    { exerciseName = name ewt,
      exerciseSanskritName = sanskritName ewt,
      exerciseDescription = description ewt,
      exercisePositionId = positionId ewt
    }

-- This is to alleviate frontend from having to join Exercises from RoutineExercises join table
data RoutineWithExercises
  = RoutineWithExercises
      { routineId :: RoutineId,
        topic :: Text,
        exercises :: [ExerciseInRoutine]
      }
  deriving (Generic)

instance ToJSON RoutineWithExercises

instance FromJSON RoutineWithExercises

data ExerciseInRoutine
  = ExerciseInRoutine
      { eirExerciseId :: ExerciseId,
        eirDuration :: DurationMinutes
      }
  deriving (Generic)

instance ToJSON ExerciseInRoutine

instance FromJSON ExerciseInRoutine

newtype DurationMinutes = DurationMinutes {getDurationMinutes :: Int}
  deriving (ToJSON, FromJSON) via Int

fromRoutine :: Entity Routine -> [RoutineExercise] -> RoutineWithExercises
fromRoutine entity res =
  let routine = entityVal entity
      routineId = entityKey entity
   in RoutineWithExercises
        { routineId = routineId,
          topic = routineTopic routine,
          exercises =
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

createDemoData :: IO ()
createDemoData = runSqlite "sestavr.db" $ do
  runMigration migrateAll

  breathId <- insert $ Target "Dech"
  _feetId <- insert $ Target "Chodidla"
  hipId <- insert $ Target "Kyčle"
  backId <- insert $ Target "Záda"
  _headId <- insert $ Target "Hlava"

  sitId <- insert $ Position "Vsedě"
  standId <- insert $ Position "Ve stoji"
  kneelId <- insert $ Position "V kleku"
  lyingFrontId <- insert $ Position "Na břiše"
  lyingBackId <- insert $ Position "Na zádech"
  handSupportedId <- insert $ Position "S oporou paží"

  boatId <- insert $ Exercise "Pozice loďky" (Just "Navasana")  "![Navasana](Navasana.png)" sitId
  childId <- insert $ Exercise "Pozice dítěte" (Just "Balasana")  "![Balasana](Balasana.png)" kneelId
  
  mapM_ insert 
        [ Exercise "Pes hlavou dolů / střecha" (Just "Adho Mukha Svanasana")  "![AdhoMukhaSvanasana](AdhoMukhaSvanasana.png)" handSupportedId
        , Exercise "Stojka" (Just "Adho Mukha Vrksasana")  "![AdhoMukhaVrksasana](AdhoMukhaVrksasana.png)" handSupportedId
        , Exercise "Ananda Balasana" (Just "Ananda Balasana")  "![AnandaBalasana](AnandaBalasana.png)" lyingBackId
        , Exercise "Pozice ležícího Višny" (Just "Anantasana")  "![Anantasana](Anantasana.png)" lyingBackId
        , Exercise "Pozice Apány" (Just "Apanasana")  "![Apanasana](Apanasana.png)" lyingBackId
        , Exercise "Poloviční pozice krále rybářů" (Just "Ardha Matsyendrasana")  "![ArdhaMatsyendrasana](ArdhaMatsyendrasana.png)" sitId
        , Exercise "Ardha Padmasana" (Just "Ardha Padmasana")  "![ArdhaPadmasana](ArdhaPadmasana.png)" sitId
        , Exercise "Ardha Purvottanasana" (Just "Ardha Purvottanasana")  "![ArdhaPurvottanasana](ArdhaPurvottanasana.png)" handSupportedId
        , Exercise "Pozice osmi úhlů" (Just "Astavakrasana")  "![Astavakrasana](Astavakrasana.png)" handSupportedId
        , Exercise "Pozice spojeného úhlu" (Just "Baddha Konasana")  "![BaddhaKonasana](BaddhaKonasana.png)" sitId
        , Exercise "Pozice volavky" (Just "Bakasana")  "![Bakasana](Bakasana.png)" handSupportedId
        , Exercise "Pozice kobry" (Just "Bhujangasana")  "![Bhujangasana](Bhujangasana.png)" lyingFrontId
        , Exercise "Pozice tyče s oporou čtyř končetin" (Just "Chaturanga Dandasana")  "![ChaturangaDandasana](ChaturangaDandasana.png)" handSupportedId
        , Exercise "Pozice tyče" (Just "Dandasana")  "![Dandasana](Dandasana.png)" sitId
        , Exercise "Pozice luku" (Just "Dhanurasana")  "![Dhanurasana](Dhanurasana.png)" lyingFrontId
        , Exercise "Dwi Pada Pitham" (Just "Dwi Pada Pitham")  "![DwiPadaPitham](DwiPadaPitham.png)" lyingBackId
        , Exercise "Pozice jednonohého královského holuba" (Just "Eka Pada Rajakapotasana")  "![EkaPadaRajakapotasana](EkaPadaRajakapotasana.png)" kneelId
        , Exercise "Pozice orla" (Just "Garudasana")  "![Garudasana](Garudasana.png)" standId
        , Exercise "Pozice kravího obličeje" (Just "Gomukhasana")  "![Gomukhasana](Gomukhasana.png)" sitId
        , Exercise "Pozice pluhu" (Just "Halasana")  "![Halasana](Halasana.png)" lyingBackId
        , Exercise "Pozice opice" (Just "Hanumanasana")  "![Hanumanasana](Hanumanasana.png)" sitId
        , Exercise "Pozice s hlavou na koleni" (Just "Janu Sirsasana")  "![JanuSirsasana](JanuSirsasana.png)" sitId
        , Exercise "Břišní zkrut" (Just "Jathara Parivrtti")  "![JatharaParivrtti](JatharaParivrtti.png)" lyingBackId
        , Exercise "Pozice s koleny vedle uší" (Just "Karnapidasana")  "![Karnapidasana](Karnapidasana.png)" lyingBackId
        , Exercise "Pozice želvy" (Just "Kurmasana")  "![Kurmasana](Kurmasana.png)" sitId
        , Exercise "Pozice velké pečeti" (Just "Mahamudra Asana")  "![MahamudraAsana](MahamudraAsana.png)" sitId
        , Exercise "Pozice ve dřepu a sedu" (Just "Malasana")  "![MalasanaUpavesasana](Upavesasana.png)" standId
        , Exercise "Marjariasana" (Just "Marjariasana")  "![Marjariasana](Marjariasana.jpg)" kneelId
        , Exercise "Pozice ryby" (Just "Matsyasana")  "![Matsyasana](Matsyasana.png)" lyingBackId
        , Exercise "Pozice páva" (Just "Mayurasana")  "![Mayurasana](Mayurasana.png)" handSupportedId
        , Exercise "Pozice kořenového zámku" (Just "Mulabandhasana")  "![Mulabandhasana](Mulabandhasana.png)" sitId
        , Exercise "Pozice krále tanečníků" (Just "Natarajasana")  "![Natarajasana](Natarajasana.png)" standId
        , Exercise "Stoj na ramenou bez opory rukou" (Just "Niralamba Sarvangasana")  "![NiralambaSarvangasana](NiralambaSarvangasana.png)" lyingBackId
        , Exercise "Pozice lotosu" (Just "Padmasana")  "![Padmasana](Padmasana.png)" sitId
        , Exercise "Pozice petlice" (Just "Parighansana")  "![Parighansana](Parighansana.png)" kneelId
        , Exercise "Úklon do strany s otočením" (Just "Parivrtta Baddha Parsvakonasana")  "![ParivrttaBaddhaParsvakonasana](ParivrttaBaddhaParsvakonasana.png)" standId
        , Exercise "Obrácená pozice s hlavou na koleni" (Just "Parivrtta Janu Sirsasana")  "![ParivrttaJanuSirsasana](ParivrttaJanuSirsasana.png)" sitId
        , Exercise "Pozice otočeného trojúhelníku" (Just "Parivrtta Trikonasana")  "![ParivrttaTrikonasana](ParivrttaTrikonasana.png)" standId
        , Exercise "Boční pozice volavky" (Just "Parsva Bakasana")  "![ParsvaBakasana](ParsvaBakasana.png)" handSupportedId
        , Exercise "Intenzivní protažení k jedné noze" (Just "Parsvottanasana")  "![Parsvottanasana](Parsvottanasana.png)" standId
        , Exercise "Zadní (zádové) protažení" (Just "Paschimottanasana")  "![Paschimottanasana](Paschimottanasana.png)" sitId
        , Exercise "Pozice opeřeného páva" (Just "Pincha Mayurasana")  "![PinchaMayurasana](PinchaMayurasana.png)" handSupportedId
        , Exercise "Předklon v širokém postoji" (Just "Prasarita Padottanasana")  "![PrasaritaPadottanasana](PrasaritaPadottanasana.png)" standId
        , Exercise "Pozice vzestupného sloupu" (Just "Purvottanasana")  "![Purvottanasana](Purvottanasana.png)" handSupportedId
        , Exercise "Pozice kobylky" (Just "Salabhasana")  "![Salabhasana](Salabhasana.png)" lyingFrontId
        , Exercise "Stoj na ramenou s oporou" (Just "Salamba Sarvangasana")  "![SalambaSarvangasana](SalambaSarvangasana.png)" lyingBackId
        , Exercise "Stoj na hlavě s oporou" (Just "Salamba Sirsasana")  "![SalambaSirsasana](SalambaSirsasana.png)" sitId
        , Exercise "Samasthiti" (Just "Samasthiti")  "![Samasthiti](Samasthiti.png)" standId
        , Exercise "Pozice mrtvoly" (Just "Savasana")  "![Savasana](Savasana.png)" lyingBackId
        , Exercise "Pozice mostu" (Just "Setu Bandhasana")  "![SetuBandhasana](SetuBandhasana.png)" lyingBackId
        , Exercise "Pozice mistra" (Just "Siddhasana")  "![Siddhasana](Siddhasana.png)" sitId
        , Exercise "Pozice lva" (Just "Simhasana")  "![Simhasana](Simhasana.png)" kneelId
        , Exercise "Jednoduchá pozice" (Just "Sukhasana")  "![Sukhasana](Sukhasana.png)" sitId
        , Exercise "Supta Baddha Konasana" (Just "Supta Baddha Konasana")  "![SuptaBaddhaKonasana](SuptaBaddhaKonasana.png)" lyingFrontId
        , Exercise "Supta Kurmasana" (Just "Supta Kurmasana")  "![SuptaKurmasana](SuptaKurmasana.png)" sitId
        , Exercise "Pozice ležicího hrdiny" (Just "Supta Virasana")  "![SuptaVirasana](SuptaVirasana.png)" kneelId
        , Exercise "Šťastná pozice" (Just "Svastikasana")  "![Svastikasana](Svastikasana.png)" sitId
        , Exercise "Pozice hory" (Just "Tadasana")  "![Tadasana](Tadasana.png)" standId
        , Exercise "Předklon vsedě s roztaženýma nohama" (Just "Upavistha Konasana")  "![UpavisthaKonasana](UpavisthaKonasana.png)" sitId
        , Exercise "Pozice kola / horního luku" (Just "Urdhva Dhanurasana")  "![UrdhvaDhanurasana](UrdhvaDhanurasana.png)" handSupportedId
        , Exercise "Pozice psa s obličejem nahoru" (Just "Urdhva Mukha Svanasana")  "![UrdhvaMukhaSvanasana](UrdhvaMukhaSvanasana.png)" handSupportedId
        , Exercise "Pozice velblouda" (Just "Ustrasana")  "![Ustrasana](Ustrasana.png)" kneelId
        , Exercise "Pozice židle / neobratná pozice" (Just "Utkatasana")  "![Utkatasana](Utkatasana.png)" standId
        , Exercise "Předklon ve stoji" (Just "Uttanasana")  "![Uttanasana](Uttanasana.png)" standId
        , Exercise "Protažení nohy ve stoji" (Just "Utthita Hasta Padangusthasana")  "![UtthitaHastaPadangusthasana](UtthitaHastaPadangusthasana.png)" standId
        , Exercise "Úklon do strany s natažením" (Just "Utthita Parsvakonasana")  "![UtthitaParsvakonasana](UtthitaParsvakonasana.png)" standId
        , Exercise "Rozšížená pozice trojúhelníku" (Just "Utthita Trikonasana")  "![UtthitaTrikonasana](UtthitaTrikonasana.png)" standId
        , Exercise "Diamantový sed" (Just "Vajrasana")  "![Vajrasana](Vajrasana.png)" sitId
        , Exercise "Pozice mudrce Vasišthy" (Just "Vasisthasana")  "![Vasisthasana](Vasisthasana.png)" handSupportedId
        , Exercise "Obrácená pozice" (Just "Viparita Karani")  "![ViparitaKarani](ViparitaKarani.png)" lyingBackId
        , Exercise "Pozice úplné kobylky" (Just "Viparita Salabhasana")  "![ViparitaSalabhasana](ViparitaSalabhasana.png)" lyingFrontId
        , Exercise "Pozice bojovníka III" (Just "Virabhadrasana III")  "![VirabhadrasanaIII](VirabhadrasanaIII.png)" standId
        , Exercise "Pozice bojovníka II" (Just "Virabhadrasana II")  "![VirabhadrasanaII](VirabhadrasanaII.png)" standId
        , Exercise "Pozice bojovníka I" (Just "Virabhadrasana I")  "![VirabhadrasanaI](VirabhadrasanaI.png)" standId
        , Exercise "Pozice hrdiny" (Just "Virasana")  "![Virasana](Virasana.png)" kneelId
        , Exercise "Vjaghrasana" (Just "Vjaghrasana")  "![Vjaghrasana](Vjaghrasana.png)" kneelId
        , Exercise "Pozice stromu" (Just "Vrksasana")  "![Vrksasana](Vrksasana.png)" standId
        , Exercise "Pozice škorpiona" (Just "Vrschikasana")  "![Vrschikasana](Vrschikasana.png)" handSupportedId
        ]

  _ <- insert $ ExerciseTarget childId hipId
  _ <- insert $ ExerciseTarget childId backId
  _ <- insert $ ExerciseTarget childId breathId

  routine1Id <- insert $ Routine "Moje první sestava"

  _ <- insert $ RoutineExercise routine1Id childId 5 2
  _ <- insert $ RoutineExercise routine1Id boatId 1 3

  currentTime <- liftIO getCurrentTime
  _ <- insert $ Lesson routine1Id currentTime

  pure ()
