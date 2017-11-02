import Chain

import Control.Concurrent.STM


main :: IO ()
main = do
  chain <- newTVarIO ([] :: [String])
  atomically $ addTransaction chain "My transaction"
  atomically $ chain `addTransaction` "My transaction1"
  atomically $ addTransaction chain "My transaction2"
  transactions <- atomically $ readTVar chain
  print transactions

  chain' <- atomically $ makeChain
  atomically $ addTransaction chain' "My transaction"
  atomically $ addTransaction chain' "My transaction1"
  transactions' <- atomically $ viewChain chain
  print transactions'
