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

sheetWorker :: Config -> TBQueue (Map Text FieldVal) -> IO ()
sheetWorker cfg mq = forever $ do
  ans <- atomically $ readTBQueue mq
  appendRow cfg ans

appendRow :: Config -> Map Text FieldVal -> IO Text
appendRow cfg ans = do
  let sheetId = tcSheets $ cfgTargets cfg
      fields = getFieldNames cfg
      orderedAns = map (ans M.!) fields
      srcType = cfgSourceType cfg
  appendGS sheetId srcType orderedAns

appendGS :: Text -> SourceType -> [FieldVal] -> IO Text
appendGS sheetId srcType vals = do
  let range = "raw!A:" <> (T.pack $ pure $ chr $ ord 'A' + length vals - 1)
      values = valueRange
        & vrValues .~ [map (toJV srcType) vals]
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

toJV :: SourceType -> FieldVal -> Value
toJV _ (ValInt n) = toJSON n
toJV _ (ValFloat d) = toJSON d
toJV _ (ValText t) = toJSON t
toJV _ (ValEncrypt t) = toJSON $ "ENC(" <> t <> ")"
toJV _ (ValTime ts) = toJSON ts
toJV _ (ValLoc loc) = toJSON $
  T.intercalate "/" $
  map ($ loc) [ locCity
              , fromMaybe "" . locMunicip
              , fromMaybe "" . locRegion
              , locSubject
              ]
toJV srcType (ValUser u) = toJSON $ f <> "(" <> u <> ")"
  where f = case srcType of
          SrcOpen -> ""
          SrcHashed -> "HASH"
          SrcEncrypted -> "ENCRYPT"
