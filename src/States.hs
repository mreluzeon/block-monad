module States where

import Control.Monad (when, mapM_)
import Data.Binary
import Data.Set (toList)
import GHC.Generics (Generic)
import Data.Typeable
import Control.Distributed.Process
import qualified Control.Distributed.Backend.P2P as P2P

import Crypto
import STMSet
import Control.Concurrent.STM

data State a = State { state :: STMSet a
                     , serviceName :: String
                     }

shareState :: (Binary a, Ord a, Typeable a) => KeyPair -> State a -> Process()
shareState (pubk, pk) State{state=set, serviceName=service} = do
  elems <- liftIO $ atomically $ getElems set
  sig <- liftIO $ signMsg pk elems
  P2P.nsendPeers service SignedMessage{signature=sig, msg=elems, pubk=pubk}

updateState :: (Binary a, Ord a, Show a) => STMSet a -> SignedMessage (Elems a) -> Process ()
updateState set SignedMessage{signature=sig, msg=newElems, pubk=pubk} =
  when (verifyMsg pubk sig newElems) $ do
    liftIO $ atomically $ set `addElems` newElems
    (Elems myElems) <- liftIO $ atomically $ getElems set
    say $ show $ toList myElems

registerStates :: ProcessId -> [State a] -> Process ()
registerStates self states = do
  let services = map (\State{state=_, serviceName=service} -> service) states
  mapM_ (flip register self) services
