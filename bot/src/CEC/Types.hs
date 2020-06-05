-- | Types of all entities

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module CEC.Types where

import Data.Aeson
import Data.Map.Strict (Map,empty)
import Data.Text (Text)
import GHC.Generics

data OpType
  = OpSum
  | OpProd
  | OpMin
  | OpMax
  deriving (Eq,Show,Read,Generic)

instance FromJSON OpType where
  parseJSON = genericParseJSON $ jsonOpts 2 0
instance ToJSON OpType where
  toEncoding = genericToEncoding $ jsonOpts 2 0

data SourceType
  = SrcOpen
  | SrcHashed
  | SrcEncrypted
  deriving (Eq,Show,Read,Generic)

instance FromJSON SourceType where
  parseJSON = genericParseJSON $ jsonOpts 3 0
instance ToJSON SourceType where
  toEncoding = genericToEncoding $ jsonOpts 3 0

data FieldType
  = FieldInt -- OpType
  | FieldFloat -- OpType
  | FieldText
  | FieldEncrypt
  deriving (Eq,Show,Read,Generic)

instance FromJSON FieldType where
  parseJSON = genericParseJSON $ jsonOpts 5 0
instance ToJSON FieldType where
  toEncoding = genericToEncoding $ jsonOpts 5 0

data FieldVal
  = ValInt Int
  | ValFloat Double
  | ValText Text
  | ValEncrypt Text
  deriving (Eq,Show,Read,Generic)

instance FromJSON FieldVal where
  parseJSON = genericParseJSON $ jsonOpts 3 3
instance ToJSON FieldVal where
  toEncoding = genericToEncoding $ jsonOpts 3 3

data FieldDef = FieldDef
  { fdName :: Text
  , fdDesc :: Text
  , fdType :: FieldType
  } deriving (Eq,Show,Read,Generic)

instance FromJSON FieldDef where
  parseJSON = genericParseJSON $ jsonOpts 5 2
instance ToJSON FieldDef where
  toEncoding = genericToEncoding $ jsonOpts 5 2

data QuestionDef = QuestionDef
  { qdText :: Text
  , qdAnswer :: [Text]
  , qdError :: Maybe Text
  } deriving (Eq,Show,Read,Generic)

instance FromJSON QuestionDef where
  parseJSON = genericParseJSON $ jsonOpts 8 2
instance ToJSON QuestionDef where
  toEncoding = genericToEncoding $ jsonOpts 8 2

data BotCfg = BotCfg
  { bcToken :: Text
  , bcPublicKey :: Text
  , bcSecretKey :: Text
  -- , bcRemind :: Maybe Text
  } deriving (Eq,Show,Read,Generic)

instance FromJSON BotCfg where
  parseJSON = genericParseJSON $ jsonOpts 3 2
instance ToJSON BotCfg where
  toEncoding = genericToEncoding $ jsonOpts 3 2

data TargetCfg = TargetCfg
  { tcSheets :: Text
  -- , tcSsb :: Text
  -- , tcIpfs :: Text
  } deriving (Eq,Show,Read,Generic)

instance FromJSON TargetCfg where
  parseJSON = genericParseJSON $ jsonOpts 6 2
instance ToJSON TargetCfg where
  toEncoding = genericToEncoding $ jsonOpts 6 2

data Config = Config
  { cfgTimeField :: Text
  , cfgGeoField :: Text
  , cfgSourceField :: Text
  , cfgSourceType :: SourceType
  , cfgFields :: [FieldDef]
  , cfgGeoFile :: FilePath
  , cfgWelcome :: Text
  , cfgRegisterButton :: Maybe Text
  , cfgRegisterAnswer :: Maybe Text
  , cfgQuestions :: [QuestionDef]
  , cfgBot :: BotCfg
  , cfgTargets :: TargetCfg
  } deriving (Eq,Show,Read,Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ jsonOpts 6 3
instance ToJSON Config where
  toEncoding = genericToEncoding $ jsonOpts 6 3

data State
  = NotStarted
  | RegisteredGeo
    { stLat :: Double
    , stLon :: Double
    }
  | Answered
    { stLat :: Double
    , stLon :: Double
    , stCurrent :: Int
    , stAnswers :: Map Text FieldVal
    }
  deriving (Eq,Show,Read,Generic)

getCurrent :: State -> Int
getCurrent Answered{ stCurrent=n } = n
getCurrent _ = 0

getAnswers :: State -> Map Text FieldVal
getAnswers Answered{ stAnswers=m } = m
getAnswers _ = empty

jsonOpts :: Int -> Int -> Options
jsonOpts m k = defaultOptions
  { fieldLabelModifier = camelTo2 '-' . drop k
  , constructorTagModifier = camelTo2 '-' . drop m
  }
