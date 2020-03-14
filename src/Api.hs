{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( SestavrAPI,
    sestavrApi,
  )
where

import Data.Proxy (Proxy (Proxy))
import Database.Persist.Types (Entity)
import Model
import Servant.API

type SestavrAPI =
  "position" :> Capture "positionId" PositionId :> Get '[JSON] Position
    :<|> "position" :> Get '[JSON] [Entity Position]
    :<|> "exercise" :> Get '[JSON] [Entity Exercise]
    :<|> "lesson" :> Get '[JSON] [Entity Lesson]
    :<|> "target" :> Get '[JSON] [Entity Target]
    

sestavrApi :: Proxy SestavrAPI
sestavrApi = Proxy
