{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import System.Environment (getArgs)
import Control.Monad
import Network.HTTP.Client
import Servant.Client
import Servant
import Api
import STMSet
import Types
import Servant.API
import Data.Proxy

get :<|> add = client api

main = do
  [port] <- getArgs
  manager <- newManager defaultManagerSettings
  let r = flip runClientM (ClientEnv manager (BaseUrl Http "localhost" (read port) ""))
  a <- getLine
  let b = read a :: Flower
  void $ r $ add b
