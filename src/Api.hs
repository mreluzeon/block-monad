{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( app
  , api
  ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai
import Servant (Server, serve)
import Servant.API
import FlowerMap
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
type GetEndpoint = "getBlocks" :> Get '[ PlainText] Text

geth :: FlowerMap -> Server GetEndpoint
geth flowerMap = do
  coords <- liftIO $ atomically $ getCoordinates flowerMap
  return $ T.pack $ show coords
  --return "SSS"

type PostEndpoint = "post" :> Post '[ PlainText] Text
posth :: Server PostEndpoint

posth = return "post"

type API =
        GetEndpoint
    :<|> PostEndpoint

handler :: FlowerMap -> Server API
handler flowerMap =
         (geth flowerMap)
    :<|> posth

api :: Proxy API
api = Proxy

app :: FlowerMap -> Application
app flowerMap = serve api $ handler flowerMap
