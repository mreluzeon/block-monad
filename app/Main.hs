{-# LANGUAGE TemplateHaskell #-}
module Main where

import           System.Environment (getArgs)
import           Control.Concurrent (forkIO)

import           Control.Concurrent.STM
import           Control.Distributed.Process
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           System.IO (isEOF)
import           Control.Monad (forever, unless)
import           Control.Concurrent (threadDelay)
import           Control.Distributed.Process.Node (initRemoteTable)

import FlowerMap
import Motivators
import Lib
import Api
import Network.Wai
import Network.Wai.Handler.Warp
--import Client


stateService = "bees"

shareState :: FlowerMap -> Process()
shareState flowerMap = do
  flowers <- liftIO $ atomically $ getCoordinates flowerMap
  P2P.nsendPeers stateService flowers

updateState :: FlowerMap -> Coordinates -> Process ()
updateState flowerMap newFlowers = do
  liftIO $ atomically $ flowerMap `addCoordinates` newFlowers
  myFlowers <- liftIO $ atomically $ getCoordinates flowerMap
  say $ show myFlowers

waitInput :: FlowerMap -> IO ()
waitInput flowerMap = do
  done <- isEOF
  unless done $ do
    line <- getLine
    let coord = read line :: Coordinate
    atomically $ do
      flowerMap `addCoordinate` coord
    waitInput flowerMap

main = do
  [from, to] <- getArgs
  flowerMap <- atomically makeMap
  print "Launching server"
  forkIO $ run 8000 $ app flowerMap
  print "Launched!"
  P2P.bootstrap "127.0.0.1" from [P2P.makeNodeId ("127.0.0.1:" ++ to)] initRemoteTable $ do
    liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes

    self <- getSelfPid
    say $ "Me: " ++ show self

    register stateService self
    say "Registered"
    liftIO $ threadDelay 3000000 -- give other nodes time to register

    P2P.nsendPeers stateService $ "Hi! My port is " ++ (show from)

    spawnLocal $ shareStateMotivator self
    liftIO $ forkIO $ waitInput flowerMap

    let actionHandler :: Action -> Process ()
        actionHandler _ = shareState flowerMap
    forever $ do
      --receiveWait [ match (\(_ :: Action) -> shareState self flowerMap)
      receiveWait [ match actionHandler
                  , match $ updateState flowerMap
                  ]

-- monabλock
