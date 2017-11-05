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
import Network.Wai
import Servant (Server, serve)
import Servant.API
import Data.Set (toList)

import Types
import STMSet as S

type GetEndpoint = "getBlocks" :> Get '[ PlainText] Text

geth :: (Show a, Ord a) => S.STMSet a -> Server GetEndpoint
geth set = do
  (Elems myElems) <- liftIO $ atomically $ getElems set
  return $ T.pack $ show $ toList myElems

type AddEndpoint = "add" :> ReqBody '[ JSON] Flower :> Put '[ PlainText] NoContent

addh set rawElem = do
  liftIO . atomically $ addElem set (Elem rawElem)
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
