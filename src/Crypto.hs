module Crypto
  ( newKeyPair
  , sign
  , verify
  ) where

import Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.ByteString as B

sec_p256k1 :: ECC.Curve
sec_p256k1 = ECC.getCurveByName ECC.SEC_p256k1

type KeyPair = (ECDSA.PublicKey, ECDSA.PrivateKey)

newKeyPair :: IO KeyPair
newKeyPair = ECC.generate sec_p256k1

sign :: ECDSA.PrivateKey -> B.ByteString -> IO ECDSA.Signature
sign pk = ECDSA.sign pk SHA3_256

verify :: ECDSA.PublicKey -> ECDSA.Signature -> B.ByteString -> Bool
verify = ECDSA.verify SHA3_256
