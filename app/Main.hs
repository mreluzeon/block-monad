{-# LANGUAGE TemplateHaskell #-}
module Main where

import           System.Environment (getArgs)
import           Control.Distributed.Process
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           Control.Monad (forever)
import           Control.Concurrent (threadDelay)
import           Control.Distributed.Process.Node (initRemoteTable)

main = do
  [from, to] <- getArgs
  P2P.bootstrap "127.0.0.1" from 
    [P2P.makeNodeId ("127.0.0.1:" ++ to)
    -- , P2P.makeNodeId "127.0.0.1:8999"
    -- , P2P.makeNodeId "127.0.0.1:8888"
    ] initRemoteTable $ do
    liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
    P2P.nsendPeers ("dver " ++ from) "H - водород"
    -- say "sth"
    P2P.nsendPeers ("dver " ++ from) "H - водород"
    P2P.nsendPeers ("dver " ++ from) "H - водород"
    P2P.nsendPeers ("dver " ++ from) "H - водород"
    forever $ do
      sth <- expect :: Process String
      say sth
      -- liftIO $ putStrLn "pidSkaz"
    -- liftIO $ threadDelay 1000000
    -- sth <- expect :: Process String
    -- liftIO $ putStrLn "pidor skazal: "

-- monabλock