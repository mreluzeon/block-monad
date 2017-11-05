{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app
  , api
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import FlowerMap
import Network.Wai
import Servant (Server, serve)
import Servant.API

type GetEndpoint = "getBlocks" :> Get '[ PlainText] Text

geth :: FlowerMap -> Server GetEndpoint
geth flowerMap = do
  coords <- liftIO $ atomically $ getCoordinates flowerMap
  return $ T.pack $ show coords
  --return "SSS"

type AddEndpoint = "add" :> ReqBody '[ JSON] Coordinate :> Put '[ PlainText] NoContent

addh flowerMap coord = do
  liftIO . atomically $ addCoordinate flowerMap coord
  return NoContent

type API =
        GetEndpoint
    :<|> AddEndpoint

handler :: FlowerMap -> Server API
handler flowerMap =
         (geth flowerMap)
    :<|> (addh flowerMap)

api :: Proxy API
api = Proxy

app :: FlowerMap -> Application
app flowerMap = serve api $ handler flowerMap
