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
import qualified Data.Text as Text
import Database.Persist.Class (delete, get, insertEntity, replace, selectList)
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, runMigration, runSqlPersistMPool, runSqlPool)
import Database.Persist.Types (Entity)
import Database.Sqlite (Error (ErrorConstraint), SqliteException, seError)
import Model (Exercise, ExerciseId, Lesson, Position, PositionId, Target, TargetId, migrateAll)
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
    :<|> getPosition
    :<|> getPositions
    :<|> getLessons
    :<|> getTargets
    -- Target
    :<|> createTarget
    :<|> deleteTarget
    :<|> updateTarget
    -- Exercise
    :<|> getExercises
    :<|> updateExercise
  where
    runPool :: MonadIO m => SqlPersistM a -> m a
    runPool action = liftIO $ runSqlPersistMPool action pool
    --
    getPosition :: PositionId -> Handler Position
    getPosition positionId = do
      mPosition <- runPool $ get positionId
      case mPosition of
        Nothing -> throwError $ err404 {errBody = "Pozice neexistuje : " <> LBS.pack (show positionId)}
        Just p -> pure p
    --
    getPositions :: Handler [Entity Position]
    getPositions = runPool $ selectList [] []
    --
    getExercises :: Handler [Entity Exercise]
    getExercises = runPool $ selectList [] []
    --
    getLessons :: Handler [Entity Lesson]
    getLessons = runPool $ selectList [] []
    --
    getTargets :: Handler [Entity Target]
    getTargets = runPool $ selectList [] []
    --
    handleConstraintError :: Handler x -> LBS.ByteString -> Handler x
    handleConstraintError action err =
      action `catch` handler
      where
        handler :: SqliteException -> Handler x
        handler ex = case seError ex of
          ErrorConstraint -> throw409 ex err
          _ -> throwM ex
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
    --
    updateExercise :: ExerciseId -> Exercise -> Handler ()
    updateExercise exerciseId exercise =
      runPool (replace exerciseId exercise)

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
