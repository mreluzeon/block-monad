module Main where

import Control.Concurrent (forkIO, threadDelay)
import System.Environment (getArgs)
import System.IO ( openFile 
                 , readFile 
                 , isEOF
                 , readFile
                 , IOMode(ReadMode)
                 , hGetLine
                 )

import Control.Concurrent.STM
import qualified Control.Distributed.Backend.P2P as P2P
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp

import Api
import Crypto
import FlowerMap
import Motivators
import STMSet
import States
--import Client

type Flower = (Int, Int)

type FlowerMap = STMSet Flower

type FlowerState = State Flower

waitInput :: (Ord a, Read a) => STMSet a -> IO ()
waitInput set = do
  done <- isEOF
  unless done $ do
    line <- getLine
    let elem = read line
    atomically $ set `addElem` Elem elem
    waitInput set

main = do
  [configName] <- getArgs
  h <- openFile ("configs/" ++ configName) ReadMode

  from <- hGetLine h
  tos <- hGetLine h 
  let tosNodes = [(P2P.makeNodeId ("127.0.0.1:" ++ to)) | to <- (words tos)]

  flowerMap <- atomically makeSet
  let flowerState = State {state = flowerMap, serviceName = "bees"} :: FlowerState

  keys <- newKeyPair

  print "Launching server"
  --forkIO $ run 8000 $ app flowerMap
  print "Launched!"

  P2P.bootstrap "127.0.0.1" from tosNodes initRemoteTable $ do
    liftIO $ threadDelay 3000000 -- give dispatcher a second (or 2) to discover other nodes

    self <- getSelfPid
    say $ "Me: " ++ show self

    registerStates self [flowerState]
    say "Registered"
    liftIO $ threadDelay 3000000 -- give other nodes time to register

    --P2P.nsendPeers stateService $ "Hi! My port is " ++ (show from)

    spawnLocal $ shareStateMotivator self
    liftIO $ forkIO $ waitInput flowerMap

    let actionHandler :: Action -> Process ()
        actionHandler _ = shareState keys flowerState

    forever $ receiveWait [match actionHandler, match $ updateState flowerMap]
-- monabλock


