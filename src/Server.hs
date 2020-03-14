{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Text as Text
import Database.Persist.Class (get)
import Database.Persist.Sqlite
  ( ConnectionPool,
    createSqlitePool,
    runMigration,
    runSqlPersistMPool,
    runSqlPool,
  )
import Model (Position, PositionId, migrateAll)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant

run :: IO ()
run = do
  app <- mkApp "sestavr.db"
  Warp.run 3000 app

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (Text.pack sqliteFile) 3
  runSqlPool (runMigration migrateAll) pool
  pure $ serveApp pool

serveApp :: ConnectionPool -> Application
serveApp pool = serve sestavrApi $ apiServer pool

apiServer :: ConnectionPool -> Server SestavrAPI
apiServer pool = getPositionH
  where
    getPositionH :: PositionId -> Handler Position
    getPositionH positionId = do
      mPosition <- liftIO $ flip runSqlPersistMPool pool $ get positionId
      case mPosition of
        -- TODO which position
        Nothing -> throwError $ err404 {errBody = "Position doesn't exist"}
        Just p -> pure p
