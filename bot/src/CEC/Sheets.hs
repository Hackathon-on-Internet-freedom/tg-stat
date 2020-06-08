-- | Interacting with Google sheets

{-# LANGUAGE OverloadedStrings #-}

module CEC.Sheets where

import CEC.Types
import CEC.Keys

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as ET
import Network.Google.Sheets
import Network.Google
import System.IO (stdout)

type CC = (TMVar Nonce, PublicKey, SecretKey, Key, TMVar Text)

sheetWorker :: Config -> TBQueue MsgItem -> IO ()
sheetWorker cfg mq = do
  iv <- initNonce >>= newTMVarIO
  ph <- newTMVarIO ""
  forever $ do
    let sheetId = tcSheets $ cfgTargets cfg
        theBot = cfgBot cfg
        pk = bcPublicKey theBot
        sk = bcSecretKey theBot
        ek = bcEncryptKey theBot
        cc = (iv,pk,sk,ek,ph)
    msg <- atomically $ readTBQueue mq
    case msg of
      MsgInfo ans -> appendRow cfg sheetId cc ans
      MsgTrust (ts,src,tgt) -> appendGS sheetId "trust" cc (cfgSourceType cfg)
        [ ValTime ts
        , ValUser src
        , ValUser tgt
        ]

appendRow :: Config -> Text -> CC -> Map Text FieldVal -> IO Text
appendRow cfg sheetId cc ans = do
  let fields = getFieldNames cfg
      orderedAns = map (ans M.!) fields
      srcType = cfgSourceType cfg
  appendGS sheetId "raw" cc srcType orderedAns

appendGS :: Text -> Text -> CC -> SourceType -> [FieldVal] -> IO Text
appendGS sheetId name cc srcType vals = do
  raws <- encodeVals cc srcType vals
  let range = name <> "!A:" <> (T.pack $ pure $ chr $ ord 'A' + length raws - 1)
      values = valueRange
        & vrValues .~ [raws]
        & vrRange .~ Just range
        & vrMajorDimension .~ Just VRMDRows
      req = spreadsheetsValuesAppend sheetId values range
        & svaValueInputOption .~ Just "USER_ENTERED"
  lgr <- newLogger Debug stdout
  env <- newEnv
         <&> (envLogger .~ lgr)
         . (envScopes .~ spreadsheetsScope)
  resp <- runResourceT . runGoogle env $ send req
  pure $ T.pack $ show resp

encodeVals :: CC -> SourceType -> [FieldVal] -> IO [Value]
encodeVals cc@(_,pk,sk,_,ph) srcType vals = do
  rs <- mapM (toJV cc srcType) vals
  h <- atomically $ do
    phv <- takeTMVar ph
    let v = ET.decodeUtf8 $ encode $ concat $ rs ++ [[toJSON phv]]
        hv = hash $ LT.toStrict v
    putTMVar ph hv
    pure hv
  let raws = concat rs ++ [toJSON h]
      ds = sign sk pk $ LB.toStrict $ encode raws
  pure $ raws ++ [toJSON ds]

mkNonce :: TMVar Nonce -> STM Nonce
mkNonce iv = do
  old <- takeTMVar iv
  let new = incrementNonce old
  putTMVar iv new
  pure old

encJV :: TMVar Nonce -> Key -> Text -> IO [Value]
encJV iv ek t = do
  nonce <- atomically $  mkNonce iv
  let ecipher = encrypt nonce ek t
  case ecipher of
    Left e -> pure [toJSON nonce, toJSON $ show e]
    Right cipher -> pure [toJSON nonce, toJSON cipher]

toJV :: CC -> SourceType -> FieldVal -> IO [Value]
toJV _ _ (ValInt n) = pure $ pure $ toJSON n
toJV _ _ (ValFloat d) = pure $ pure $ toJSON d
toJV _ _ (ValText t) = pure $ pure $ toJSON t
toJV (iv,_,_,ek,_) _ (ValEncrypt t) = encJV iv ek t
toJV _ _ (ValTime ts) = pure $ pure $ toJSON ts
toJV _ _ (ValLoc loc) = pure $ map (toJSON . ($ loc))
  [ locCity
  , fromMaybe "" . locMunicip
  , fromMaybe "" . locRegion
  , locSubject
  ]
toJV (iv,_,_,ek,_) srcType (ValUser u) = case srcType of
  SrcOpen -> pure $ pure $ toJSON u
  SrcHashed -> pure $ pure $ toJSON $ hash u
  SrcEncrypted -> encJV iv ek u
