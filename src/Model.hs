{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Model
    ( Exercise
    , ExerciseId
    , ExerciseTargetId
    , Lesson
    , LessonId
    , Position 
    , PositionId
    , RoutineId
    , RoutineExerciseId
    , Target
    , TargetId
    , createDemoData
    , migrateAll
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sqlite
import Database.Persist.TH

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
    UniqueExerciseTarget exerciseId targetId
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

createDemoData :: IO ()
createDemoData = runSqlite "sestavr.db" $ do
  runMigration migrateAll
  
  breathId <- insert $ Target "Dech"
  _feetId <- insert $ Target "Chodidla"
  hipId <- insert $ Target "Kyčle"
  backId <- insert $ Target "Záda"
  _headId <- insert $ Target "Hlava"

  sitId <- insert $ Position "sed"
  _standId <- insert $ Position "stoj"
  kneelId <- insert $ Position "klek"
  _lyingId <- insert $ Position "leh na břiše"
  lyingBackId <- insert $ Position "leh na zádech"
  plankPositionId <- insert $ Position "vzpor na rukou"

  plankId <- insert $ Exercise "Prkno" Nothing "popis prkna ... dlouhý text" plankPositionId
  boatId <- insert $ Exercise "Loďka" (Just "Navasana") "..." sitId
  childId <- insert $ Exercise "Pozice dítěte" (Just "Balasana") "nejaky textovy popis" kneelId
  _corpseId <- insert $ Exercise "Mrtvola" (Just "Shavasana") "Shavasana (Sanskrit: शवासन; IAST: śavāsana), Corpse Pose, or Mrtasana,\n\
\is an asana in hatha yoga and modern yoga as exercise, often used for relaxation at the end of a session.\n\
\It is the usual pose for the practice of yoga nidra meditation.\n\
\![savasana](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Shavasana.jpg/280px-Shavasana.jpg)" lyingBackId

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
