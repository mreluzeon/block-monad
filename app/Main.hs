{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)

import Lib

main :: IO()
main = do
  scotty 3000 $ do
  get "/getBlocks/:quantity" $ do
    quantity <- param "quantity"
    html $ mconcat ["<h1>Scotty, ", quantity, " me up!</h1>"]

  get "/getBlocks" $ do
    html $ mconcat ["all blocks"]

  post "/post" $ do
    string <- param "value"
    html $ mconcat ["kjdgf", string, "yetr"]
  -- putStrLn