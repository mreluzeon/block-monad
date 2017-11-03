import FlowerMap

import Control.Concurrent.STM


main :: IO ()
main = do
  flowerMap <- atomically $ makeMap
  atomically $ addCoordinate flowerMap (1, 1)
  atomically $ flowerMap `addCoordinate` (2, 3)
  coordinates <- atomically $ readTVar flowerMap
  print coordinates

  flowerMap' <- atomically $ makeMap
  atomically $ addCoordinate flowerMap' (14, 13)
  atomically $ addCoordinate flowerMap' (44, 44)
  coordinates' <- atomically $ getCoordinates flowerMap'
  print coordinates'
