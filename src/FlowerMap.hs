module FlowerMap where

import Control.Monad (mapM_)

import Control.Concurrent.STM

import qualified CRDT.Cv.GSet as S

type Coordinate = (Int, Int)
type Coordinates = S.GSet Coordinate
type FlowerMap = TVar Coordinates

makeMap :: STM FlowerMap
makeMap = do
  --flowerMap <- newTVar $ (initial :: GSet Coordinate)
  flowerMap <- newTVar S.initial 
  return flowerMap

addCoordinate :: FlowerMap -> Coordinate -> STM ()
addCoordinate flowerMap coordinate = modifyTVar flowerMap $ S.add coordinate

addCoordinates :: FlowerMap -> Coordinates -> STM ()
addCoordinates flowerMap = mapM_ (\coord -> flowerMap `addCoordinate` coord)

getCoordinates :: FlowerMap -> STM Coordinates
getCoordinates = readTVar
