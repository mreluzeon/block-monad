module Chain where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map.Strict as Map

type Transaction = String
type Chain = Block
type Block = TVar [Transaction]

makeChain :: STM Chain
makeChain = newTVarIO []

addTransaction :: Chain -> Transaction -> STM ()
addTransaction chain transaction = do

