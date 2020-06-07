-- | Interacting with Google sheets

{-# LANGUAGE OverloadedStrings #-}

module CEC.Sheets where

import CEC.Types

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.Google.Sheets
import Network.Google
import System.IO (stdout)

sheetWorker :: Config -> TBQueue MsgItem -> IO ()
sheetWorker cfg mq = forever $ do
  let sheetId = tcSheets $ cfgTargets cfg
  msg <- atomically $ readTBQueue mq
  case msg of
    MsgInfo ans -> appendRow cfg sheetId ans
    MsgTrust (ts,src,tgt) -> appendGS sheetId "trust" SrcOpen
      [ ValTime ts
      , ValUser src
      , ValUser tgt
      ]

appendRow :: Config -> Text -> Map Text FieldVal -> IO Text
appendRow cfg sheetId ans = do
  let fields = getFieldNames cfg
      orderedAns = map (ans M.!) fields
      srcType = cfgSourceType cfg
  appendGS sheetId "raw" srcType orderedAns

appendGS :: Text -> Text -> SourceType -> [FieldVal] -> IO Text
appendGS sheetId name srcType vals = do
  let raws = concatMap (toJV srcType) vals
      range = name <> "!A:" <> (T.pack $ pure $ chr $ ord 'A' + length raws - 1)
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

toJV :: SourceType -> FieldVal -> [Value]
toJV _ (ValInt n) = pure $ toJSON n
toJV _ (ValFloat d) = pure $ toJSON d
toJV _ (ValText t) = pure $ toJSON t
toJV _ (ValEncrypt t) = ["nonce", toJSON $ "ENC(" <> t <> ")"]
toJV _ (ValTime ts) = pure $ toJSON ts
toJV _ (ValLoc loc) = map (toJSON . ($ loc))
  [ locCity
  , fromMaybe "" . locMunicip
  , fromMaybe "" . locRegion
  , locSubject
  ]
toJV srcType (ValUser u) = case srcType of
  SrcOpen -> pure $ toJSON u
  SrcHashed -> pure $ toJSON $ "HASH" <> "(" <> u <> ")"
  SrcEncrypted -> [ toJSON ("nonce" :: Text), toJSON $ "ENCRYPT(" <> u <> ")" ]
