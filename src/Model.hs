{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Model (createDemoData) where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Target
    name Text
Position
    name Text
Exercise
    name Text
    description Text
    positionId PositionId
ExerciseTarget
    exerciseId ExerciseId
    targetId TargetId
    UniqueExerciseTarget exerciseId targetId
Routine
    topic Text
RoutineExercise
    routineId RoutineId
    exerciseId ExerciseId
    durationMin Int
    order Int
    UniqueExerciseRoutineOrder exerciseId routineId order
Lesson
    routine RoutineId
    datetime UTCTime
|]

main :: IO ()
main = createDemoData

createDemoData :: IO ()
createDemoData = runSqlite "sestavr.db" $ do
  runMigration migrateAll
  
  breathId <- insert $ Target "Dech"
  feetId <- insert $ Target "Chodidla"
  hipId <- insert $ Target "Kyčle"
  backId <- insert $ Target "Záda"

  sitId <- insert $ Position "sed"
  standId <- insert $ Position "stoj"
  kneelId <- insert $ Position "klek"
  lyingId <- insert $ Position "leh na břiše"
  lyingBackId <- insert $ Position "leh na zádech"
  plankPositionId <- insert $ Position "vzpor na rukou"

  plankId <- insert $ Exercise "prkno" "popis prkna ... dlouhý text" plankPositionId
  boatId <- insert $ Exercise "loďka (navasana)" "..." sitId
  childId <- insert $ Exercise "pozice dítě (balasana)" "nejaky textovy popis" kneelId

  _ <- insert $ ExerciseTarget childId hipId
  _ <- insert $ ExerciseTarget childId backId
  _ <- insert $ ExerciseTarget childId breathId

  routine1Id <- insert $ Routine "Moje první sestava"

  _ <- insert $ RoutineExercise routine1Id plankId 2 1
  _ <- insert $ RoutineExercise routine1Id childId 5 2
  _ <- insert $ RoutineExercise routine1Id boatId 1 3
  
  currentTime <- liftIO getCurrentTime
  _ <- insert $ Lesson routine1Id currentTime

  pure ()
