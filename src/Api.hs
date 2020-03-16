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

import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (Proxy))
import Database.Persist.Types (Entity)
import Model
import Network.HTTP.Media ((//), MediaType)
import Servant.API

type SestavrAPI =
  Get '[HTML] ByteString -- index.html
    :<|> "main.js" :> Get '[JS] ByteString
    :<|> "position" :> Capture "positionId" PositionId :> Get '[JSON] Position
    :<|> "position" :> Get '[JSON] [Entity Position]
    :<|> "exercise" :> Get '[JSON] [Entity Exercise]
    :<|> "lesson" :> Get '[JSON] [Entity Lesson]
    :<|> "target" :> Get '[JSON] [Entity Target]
    --
    :<|> "target" :> ReqBody '[JSON] Target :> Post '[JSON] (Entity Target)
    :<|> "target" :> Capture "targetId" TargetId :> Delete '[JSON] ()
    :<|> "target" :> Capture "targetId" TargetId :> ReqBody '[JSON] Target :> Post '[JSON] ()

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
