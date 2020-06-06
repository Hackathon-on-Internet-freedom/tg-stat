-- | Generate key pair

module CEC.Keys
  ( Nonce
  , Key
  , generateKey
  , initNonce
  , C.incrementNonce
  , encrypt
  , Ed.PublicKey
  , Ed.SecretKey
  , Ed.sign
  , Ed.verify
  , generateKeyPair
  , hash
  ) where

import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Crypto.Error
import qualified Crypto.Hash as H
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Random.Types
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding

newtype Key = Key ByteString

generateKey :: IO Key
generateKey = Key <$> getRandomBytes 32

initNonce :: IO Nonce
initNonce = do
  bs <- getRandomBytes 12 :: IO ByteString
  throwCryptoErrorIO $ C.nonce12 bs

encrypt :: Nonce -> Key -> Text -> Either CryptoError Text
encrypt nonce (Key key) plaintext = eitherCryptoError $ do
  let plain = encodeUtf8 plaintext
  st1 <- C.initialize key nonce
  let (out,st2) = C.encrypt plain st1
      auth = C.finalize st2
  pure $ decodeUtf8 $ convertToBase Base64 $ out `B.append` BA.convert auth

generateKeyPair :: IO (Ed.PublicKey, Ed.SecretKey)
generateKeyPair = do
  sk <- Ed.generateSecretKey
  pure (Ed.toPublic sk, sk)

hash :: Text -> Text
hash t = let
  digest = H.hash $ encodeUtf8 t :: H.Digest H.SHA256
  in decodeUtf8 $ convertToBase Base64 digest
