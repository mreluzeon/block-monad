module Chain where

import Control.Concurrent.STM

type Transaction = String
type Chain = TVar Block
type Block = [Transaction]

makeChain :: STM Chain
makeChain = do
  chain <- newTVar []
  return chain

addTransaction :: Chain -> Transaction -> STM ()
addTransaction chain transaction =
  modifyTVar chain $ (++[transaction])

getTransactions :: Chain -> STM Block
getTransactions = readTVar
