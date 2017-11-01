{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Web.Spock
import Web.Spock.Config

import Data.Aeson       hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import GHC.Generics

-- data Person = Person
--   { name :: Text
--   , age  :: Int
--   } deriving (Generic, Show)
-- instance ToJSON Person
-- instance FromJSON Person

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

data JsonObject = JsonObject {firstField :: Int, secondField :: Text} deriving(Show, Generic)
instance ToJSON JsonObject
instance FromJSON JsonObject

  -- makeChain
main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "ping" $ do
    text "  {\"state\": \"ok\", \"answer\": \"pong\"}  "

  get ("fuck" <//> var) $ \name -> do
    text $ "Fuck you, " <> name <> "!"

  post "ppc" $ do 
    body <- jsonBody' :: ApiAction JsonObject
    text $ "ich habe win" <> pack (show body)

  -- post ("send" <//> var) $ \value -> do

  -- get ("take" <//> var) $ \howmany -> do
    

    -- "hello f world"

-- main :: IO ()
-- main = someFunc


{-

GET  /ping                 - sends pong
GET  /take?[howmany=[0..]] - give your blocks
GET  /nodes                - give your nodes

POST /send?value={sth}     - sends string to block-monad

-}
