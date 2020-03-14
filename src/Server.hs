{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Server
  ( run,
  )
where

import Api (SestavrAPI, sestavrApi)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as Text
import Database.Persist.Class (get, selectList)
import Database.Persist.Sqlite
  ( ConnectionPool,
    createSqlitePool,
    runMigration,
    runSqlPersistMPool,
    runSqlPool,
  )
import Database.Persist.Types (Entity)
import Model (Exercise, Lesson, Position, PositionId, Target, migrateAll)
import Network.Wai
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
  getPosition
    :<|> getPositions
    :<|> getExercises
    :<|> getLessons
    :<|> getTargets
  where
    runPool action = liftIO $ runSqlPersistMPool action pool
    --
    getPosition :: PositionId -> Handler Position
    getPosition positionId = do
      mPosition <- runPool $ get positionId
      case mPosition of
        Nothing -> throwError $ err404 {errBody = "Position doesn't exist : " <> pack (show positionId)}
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
