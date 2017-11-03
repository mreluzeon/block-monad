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
    ] initRemoteTable $ do
    liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes

    self <- getSelfPid
    say $ "Me: " ++ show self

    let myName = "dver"
    register myName self
    say "Registered!!!"

    P2P.nsendPeers myName "H - водород"

    forever $ do
      say "Waiting... <<<<<<<<"
      receiveWait [ matchAny (\m -> say "asjf;sadjfk dsa;djf") ]
      say "ASDFSDFSDF"

-- monabλock
