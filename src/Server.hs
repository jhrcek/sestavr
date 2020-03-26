{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( run,
  )
where

import Api (SestavrAPI, sestavrApi)
import Control.Exception.Safe (catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Database.Persist.Class
  ( delete,
    get,
    insert,
    insertEntity,
    replace,
    selectList,
  )
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.Sqlite
  ( ConnectionPool,
    createSqlitePool,
    runMigration,
    runSqlPersistMPool,
    runSqlPool,
  )
import Database.Persist.Types (entityKey, entityVal)
import Database.Persist.Types (Entity)
import Database.Sqlite
  ( Error (ErrorConstraint),
    SqliteException,
    seError,
  )
import Model
  ( Exercise,
    ExerciseId,
    ExerciseTarget (..),
    ExerciseWithTargets,
    Lesson,
    Position,
    PositionId,
    Target,
    TargetId,
    exerciseId,
    exerciseTargetExerciseId,
    exerciseTargetTargetId,
    fromExercise,
    migrateAll,
    targetIds,
    toExercise,
  )
import qualified Network.Wai.Handler.Warp as Warp
import Servant

run :: IO ()
run = do
  app <- mkApp "sestavr.db"
  putStrLn "Running on http://localhost:3000"
  Warp.run 3000 app

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (Text.pack sqliteFile) 3
  runSqlPool (runMigration migrateAll) pool
  pure $ serveApp pool

serveApp :: ConnectionPool -> Application
serveApp pool = serve sestavrApi $ apiServer pool

apiServer :: ConnectionPool -> Server SestavrAPI
apiServer pool =
  getIndex
    :<|> getElmApp
    :<|> getLessons
    -- Target
    :<|> getTargets
    :<|> createTarget
    :<|> deleteTarget
    :<|> updateTarget
    -- Position
    :<|> getPositions
    :<|> createPosition
    :<|> getPosition
    :<|> deletePosition
    :<|> updatePosition
    -- Exercise
    :<|> getExercises
    :<|> createExercise
    :<|> updateExercise
  where
    runPool :: MonadIO m => SqlPersistM a -> m a
    runPool action = liftIO $ runSqlPersistMPool action pool
    --
    getLessons :: Handler [Entity Lesson]
    getLessons = runPool $ selectList [] []
    --
    handleConstraintError :: Handler x -> LBS.ByteString -> Handler x
    handleConstraintError action err =
      action `catch` handler
      where
        handler :: SqliteException -> Handler x
        handler ex = case seError ex of
          ErrorConstraint -> throw409 ex err
          _ -> throwM ex
    -- TARGET
    getTargets :: Handler [Entity Target]
    getTargets = runPool $ selectList [] []
    --
    createTarget :: Target -> Handler (Entity Target)
    createTarget target =
      runPool (insertEntity target)
        `handleConstraintError` "Target area with this name already exists"
    --
    deleteTarget :: TargetId -> Handler ()
    deleteTarget targetId =
      runPool (delete targetId)
        `handleConstraintError` "Target area can't be deleted, it is used in some Exercises"
    --
    updateTarget :: TargetId -> Target -> Handler ()
    updateTarget targetId target =
      runPool (replace targetId target)
        `handleConstraintError` "Target area with this name already exists"
    -- POSITION
    getPositions :: Handler [Entity Position]
    getPositions = runPool $ selectList [] []
    --
    getPosition :: PositionId -> Handler Position
    getPosition positionId = do
      mPosition <- runPool $ get positionId
      case mPosition of
        Nothing -> throwError $ err404 {errBody = "Pozice neexistuje : " <> LBS.pack (show positionId)}
        Just p -> pure p
    --
    createPosition :: Position -> Handler (Entity Position)
    createPosition position =
      runPool (insertEntity position)
        `handleConstraintError` "Position with this name already exists"
    --
    deletePosition :: PositionId -> Handler ()
    deletePosition positionId =
      runPool (delete positionId)
        `handleConstraintError` "Position can't be deleted, it is used in some Exercises"
    --
    updatePosition :: PositionId -> Position -> Handler ()
    updatePosition positionId position =
      runPool (replace positionId position)
        `handleConstraintError` "Position with this name already exists"
    -- EXERCISE
    getExercises :: Handler [ExerciseWithTargets]
    getExercises = runPool $ do
      exerciseToTargets <- selectList [] [] :: SqlPersistM [Entity ExerciseTarget]
      exerciseEntities <- selectList [] [] :: SqlPersistM [Entity Exercise]
      let eidToTargets :: Map.Map ExerciseId [TargetId]
          eidToTargets =
            Map.fromListWith (<>) $
              fmap
                ( \entity ->
                    let val = entityVal entity
                     in ( exerciseTargetExerciseId val,
                          [exerciseTargetTargetId val]
                        )
                )
                exerciseToTargets
      pure $
        fmap
          ( \exEntity ->
              let targets = Map.findWithDefault [] (entityKey exEntity) eidToTargets
               in fromExercise exEntity targets
          )
          exerciseEntities
    --
    createExercise :: ExerciseWithTargets -> Handler ExerciseWithTargets
    createExercise exerciseWithTargets =
      do
        let exercise = toExercise exerciseWithTargets
            tids = targetIds exerciseWithTargets
        runPool $ do
          eid <- insert exercise
          mapM_ (\tid -> insert $ ExerciseTarget eid tid) tids
          pure $ exerciseWithTargets {exerciseId = eid}
        `handleConstraintError` "Exercise with this name already exists"
    --
    updateExercise :: ExerciseId -> Exercise -> Handler ()
    updateExercise eid exercise =
      runPool (replace eid exercise)

throw409 :: SqliteException -> LBS.ByteString -> Handler a
throw409 e detail = throwError $ err409 {errBody = detail <> "; " <> LBS.pack (show e)}

getIndex :: Handler ByteString
getIndex = pure indexHtml

getElmApp :: Handler ByteString
getElmApp = pure elmApp

indexHtml :: ByteString
indexHtml = $(embedFile "client/dist/index.html")

elmApp :: ByteString
elmApp = $(embedFile "client/dist/main.js")
