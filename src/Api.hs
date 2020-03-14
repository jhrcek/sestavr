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
  "position" :> Get '[JSON] [Entity Position]
    :<|> "position" :> Capture "positionId" PositionId :> Get '[JSON] Position

sestavrApi :: Proxy SestavrAPI
sestavrApi = Proxy
