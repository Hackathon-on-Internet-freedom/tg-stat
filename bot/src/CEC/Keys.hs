-- | Generate key pair

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , bs2text64
  , text2bs
  ) where

import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Crypto.Error
import qualified Crypto.Hash as H
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Random.Types
import Data.Aeson
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as ET
import GHC.Generics

bs2text64 :: BA.ByteArrayAccess ba => ba -> Text
bs2text64 k = let
  b64 = convertToBase Base64 k :: ByteString
  in ET.decodeUtf8 b64

text2bs :: BA.ByteArray ba => Text -> Either String ba
text2bs t = let
  b64 = ET.encodeUtf8 t :: ByteString
  in convertFromBase Base64 b64

newtype Key = Key ByteString deriving (Eq,Show,Read,Generic)

instance FromJSON Key where
  parseJSON = withText "Key" $ \t -> case text2bs t of
    Left e -> fail e
    Right b -> pure $ Key b
instance ToJSON Key where
  toJSON (Key k) = toJSON $ bs2text64 k
  toEncoding (Key k) = toEncoding $ bs2text64 k

generateKey :: IO Key
generateKey = Key <$> getRandomBytes 32

initNonce :: IO Nonce
initNonce = do
  bs <- getRandomBytes 12 :: IO ByteString
  throwCryptoErrorIO $ C.nonce12 bs

encrypt :: Nonce -> Key -> Text -> Either CryptoError Text
encrypt nonce (Key key) plaintext = eitherCryptoError $ do
  let plain = ET.encodeUtf8 plaintext
  st1 <- C.initialize key nonce
  let (out,st2) = C.encrypt plain st1
      auth = C.finalize st2
  pure $ ET.decodeUtf8 $ convertToBase Base64 $ out `B.append` BA.convert auth


instance FromJSON Ed.PublicKey where
  parseJSON = withText "PublicKey" $ \t -> case text2bs t of
    Left e -> fail e
    Right b -> pure $ throwCryptoError $ Ed.publicKey (b :: ByteString)
instance ToJSON Ed.PublicKey where
  toJSON pk = toJSON $ bs2text64 pk
  toEncoding pk = toEncoding $ bs2text64 pk

instance FromJSON Ed.SecretKey where
  parseJSON = withText "SecretKey" $ \t -> case text2bs t of
    Left e -> fail e
    Right b -> pure $ throwCryptoError $ Ed.secretKey (b :: ByteString)
instance ToJSON Ed.SecretKey where
  toJSON sk = toJSON $ bs2text64 sk
  toEncoding sk = toEncoding $ bs2text64 sk


generateKeyPair :: IO (Ed.PublicKey, Ed.SecretKey)
generateKeyPair = do
  sk <- Ed.generateSecretKey
  pure (Ed.toPublic sk, sk)

hash :: Text -> Text
hash t = let
  digest = H.hash $ ET.encodeUtf8 t :: H.Digest H.SHA256
  in ET.decodeUtf8 $ convertToBase Base64 digest
