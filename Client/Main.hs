{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Network.HTTP.Client
import Servant.Client
import Servant
import Api
import Servant.API
import Data.Proxy

--post :<|> get = client (api :: Data.Proxy.Proxy API)
post :<|> get = client api  
main = do
  manager <- newManager defaultManagerSettings
  print =<< flip runClientM (ClientEnv manager (BaseUrl Http "localhost" 8000 "")) post
