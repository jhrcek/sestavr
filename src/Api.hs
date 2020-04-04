{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( SestavrAPI,
    sestavrApi,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (Proxy))
import Database.Persist.Types (Entity)
import Model
import Network.HTTP.Media ((//), MediaType)
import Servant.API
  ( (:<|>) (..),
    (:>),
    Accept,
    Capture,
    Delete,
    Get,
    JSON,
    MimeRender,
    Post,
    Raw,
    ReqBody,
    contentType,
    mimeRender,
  )

type SestavrAPI =
  Get '[HTML] ByteString -- index.html
    :<|> "main.js" :> Get '[JS] ByteString
    -- Target
    :<|> "target" :> Get '[JSON] [Entity Target]
    :<|> "target" :> ReqBody '[JSON] Target :> Post '[JSON] (Entity Target)
    :<|> "target" :> Capture "targetId" TargetId :> Delete '[JSON] ()
    :<|> "target" :> Capture "targetId" TargetId :> ReqBody '[JSON] Target :> Post '[JSON] ()
    -- Position
    :<|> "position" :> Get '[JSON] [Entity Position]
    :<|> "position" :> ReqBody '[JSON] Position :> Post '[JSON] (Entity Position)
    :<|> "position" :> Capture "positionId" PositionId :> Get '[JSON] Position
    :<|> "position" :> Capture "positionId" PositionId :> Delete '[JSON] ()
    :<|> "position" :> Capture "positionId" PositionId :> ReqBody '[JSON] Position :> Post '[JSON] ()
    -- Exercise
    :<|> "exercise" :> Get '[JSON] [ExerciseWithTargets]
    :<|> "exercise" :> ReqBody '[JSON] ExerciseWithTargets :> Post '[JSON] ExerciseWithTargets
    :<|> "exercise" :> Capture "exerciseId" ExerciseId :> ReqBody '[JSON] ExerciseWithTargets :> Post '[JSON] ExerciseWithTargets
    :<|> "exercise" :> Capture "exerciseId" ExerciseId :> Delete '[JSON] ()
    -- Routine
    :<|> "routine" :> Get '[JSON] [RoutineWithExercises]
    :<|> "routine" :> ReqBody '[JSON] RoutineWithExercises :> Post '[JSON] RoutineWithExercises
    :<|> "routine" :> Capture "routineId" RoutineId :> ReqBody '[JSON] RoutineWithExercises :> Post '[JSON] RoutineWithExercises
    :<|> "routine" :> Capture "routineId" RoutineId :> Delete '[JSON] ()
    -- Lesson
    :<|> "lesson" :> Get '[JSON] [Entity Lesson]
    -- Static files
    :<|> Raw

sestavrApi :: Proxy SestavrAPI
sestavrApi = Proxy

data JS

instance Accept JS where
  contentType :: Proxy JS -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JS ByteString where
  mimeRender :: Proxy JS -> ByteString -> LBS.ByteString
  mimeRender _ = LBS.fromStrict

data HTML

instance Accept HTML where
  contentType :: Proxy HTML -> MediaType
  contentType _ = "text" // "html"

instance MimeRender HTML ByteString where
  mimeRender :: Proxy HTML -> ByteString -> LBS.ByteString
  mimeRender _ = LBS.fromStrict
