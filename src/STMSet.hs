{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module STMSet where

import Control.Monad (mapM_)
import Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import Control.Concurrent.STM
import qualified CRDT.Cv.GSet as S

data Elem a = Elem a
  deriving (Show, Read, Eq, Ord, Typeable, Generic)
instance (Binary a) => Binary (Elem a)

data Elems a = Elems (S.GSet (Elem a))
  deriving (Show, Typeable, Generic)
instance (Ord a, Binary a) => Binary (Elems a)

data STMSet a = STMSet (TVar (Elems a))

makeSet :: (Ord a) => STM (STMSet a)
makeSet = do
  let elems :: Elems a
      elems = Elems S.initial
  set <- newTVar elems
  return $ STMSet set

addElem :: (Ord a) => STMSet a -> Elem a -> STM ()
addElem (STMSet set) elem = do
  let modify :: (Ord b) => Elem b -> Elems b -> Elems b
      modify val (Elems gset) = Elems (val `S.add` gset)
  modifyTVar set $ modify elem

addElems :: (Ord a) => STMSet a -> Elems a -> STM ()
addElems set (Elems elems) =
  mapM_ (\elem -> set `addElem` elem) elems

getElems :: (Ord a) => STMSet a -> STM (Elems a)
getElems (STMSet set) = do
  readTVar set
