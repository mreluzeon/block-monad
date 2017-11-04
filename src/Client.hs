{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Client where
{-import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

--type ClientApi = "/post" :> ReqBody '[JSON] '' :> Put '[]
newtype Message = Message {msg :: String}
  deriving (Show, Generic)
instance FromJSON Message
queries :: ClientM Message
queries = return "post"
run :: IO()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8000))
  case res of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right print Messange
-}
