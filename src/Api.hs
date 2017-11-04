{-{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import System.Directory
import qualified Data.Aeson.Parser
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( app
  ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai
import Servant (Server, serve)
import Servant.API

type CoorEndpoint = "" :> Get '[ PlainText] Tuple

beeh :: Server CoorEndpoint
beeh = return
type GetEndpoint = "/getBlocks" :> Get '[ PlainText] Text

geth :: FlowerMap -> Server GetEndpoint
geth flowerMap = do
  coords <- liftIO $ atomically $ getCoordinates flowerMap
  return $ show coords

type PostEndpoint = "/post" :> Post '[ PlainText] Text
posth :: Server PostEndpoint

posth = return "post"

type API =
        GetEndpoint
    :<|> PostEndpoint

handler :: FlowerMap -> Server API
handler flowerMap =
         geth
    :<|> posth
    :<|> beeh


api :: Proxy API
api = Proxy

app :: FlowerMap -> Application
app flowerMap = serve api $ handler flowerMap
