{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Motivators where

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Binary
import GHC.Generics (Generic)
import Data.Typeable


data Action = ShareState 
  deriving (Generic, Typeable)

instance Binary Action


shareStateMotivator :: ProcessId -> Process ()
shareStateMotivator parent = do
  forever $ do
    send parent ShareState
    liftIO $ threadDelay 1000000 -- wait 0.1s
