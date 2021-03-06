{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Crypto
  ( newKeyPair
  , KeyPair
  , sign
  , verify
  , signMsg
  , verifyMsg
  , SignedMessage(..)
  ) where

import Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import Data.Data


deriving instance Generic ECDSA.Signature
instance Binary ECDSA.Signature

deriving instance Generic ECC.Point
instance Binary ECC.Point

deriving instance Generic ECC.CurveCommon
instance Binary ECC.CurveCommon

deriving instance Generic ECC.CurvePrime
instance Binary ECC.CurvePrime

deriving instance Generic ECC.CurveBinary
instance Binary ECC.CurveBinary

deriving instance Generic ECC.Curve
instance Binary ECC.Curve

deriving instance Generic ECDSA.PublicKey
instance Binary ECDSA.PublicKey

type KeyPair = (ECDSA.PublicKey, ECDSA.PrivateKey)

data SignedMessage a = SignedMessage { signature :: ECDSA.Signature
                                     , msg :: a
                                     , pubk :: ECDSA.PublicKey
                                     } deriving (Show,Read,Eq,Data,Generic,Typeable)

instance (Binary a) => Binary (SignedMessage a)

sec_p256k1 :: ECC.Curve
sec_p256k1 = ECC.getCurveByName ECC.SEC_p256k1

newKeyPair :: IO KeyPair
newKeyPair = ECC.generate sec_p256k1

sign :: ECDSA.PrivateKey -> B.ByteString -> IO ECDSA.Signature
sign pk = ECDSA.sign pk SHA3_256

signMsg :: (Binary a) => ECDSA.PrivateKey -> a -> IO ECDSA.Signature
signMsg pk msg = ECDSA.sign pk SHA3_256 $ BL.toStrict $ encode msg

verify :: ECDSA.PublicKey -> ECDSA.Signature -> B.ByteString -> Bool
verify = ECDSA.verify SHA3_256

verifyMsg :: (Binary a) => ECDSA.PublicKey -> ECDSA.Signature -> a -> Bool
verifyMsg pubk sig msg = verify pubk sig $ BL.toStrict $ encode msg
