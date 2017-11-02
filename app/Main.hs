{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import qualified Data.Text.Lazy as L

import Chain
import Lib

main = do
  blockchain <- atomically $ makeChain
  scotty 8080 $ do
    get "/getBlocks" $ do
      transactions <- liftIO $ atomically $ getTransactions $ blockchain
        -- blocks <- getTransactions blockchain
        -- return blocks
      text $ L.pack $ show $ transactions
    post "/post" $ do
      value <- param "value"
      liftIO $ atomically $ addTransaction blockchain value
      text $ "Success"

  -- scotty 3000 $ do
  -- get "/getBlocks/:quantity" $ do
  --   quantity <- param "quantity"
  --   html $ "Sorry, this function is unavailable now."
  --   -- html $ mconcat ["<h1>Scotty, ", quantity, " me up!</h1>"]
  
  -- post "/post" $ do
  --   string <- param "value"
  --   atomically $ addTransaction blockchain string
  --   html $ mconcat ["You have wirtten ", string, " congrats"]
  -- putStrLn