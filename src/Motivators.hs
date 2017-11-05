{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Motivators where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Monad (forever)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Action =
  ShareState
  deriving (Generic, Typeable)

instance Binary Action

shareStateMotivator :: ProcessId -> Process ()
shareStateMotivator parent =
  forever $ do
    send parent ShareState
    liftIO $ threadDelay 1000000 -- wait 0.1s
