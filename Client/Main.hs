{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Control.Monad
import Network.HTTP.Client
import Servant.Client
import Servant
import Api
import Servant.API
import Data.Proxy

get :<|> add = client api

main = do
  manager <- newManager defaultManagerSettings
  let r = flip runClientM (ClientEnv manager (BaseUrl Http "localhost" 8000 ""))
  void $ r $ add (1, 1)
  print =<< r get
