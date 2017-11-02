{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (threadDelay)
import Control.Distributed.Process.Node (initRemoteTable)

main = do
  P2P.bootstrap "127.0.0.1" "9001" [P2P.makeNodeId "seedhost:9000"] initRemoteTable $ do
    liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
    P2P.nsendPeers "myService" ("some", "message")