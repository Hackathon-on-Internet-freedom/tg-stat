-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module CEC.Bot where

import CEC.Types
import CEC.Geo

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import GHC.Float (float2Double)
import System.Exit
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Action
  = NoOp
  | Start
  | Register UserId Location
  | Ans Text
  -- | Trust Text
  deriving (Show)

initialModel :: State
initialModel = NotStarted

location :: UpdateParser Location
location = mkParser $ updateMessage >=> messageLocation

user :: UpdateParser UserId
user = mkParser $ fmap (fmap userId) $ updateMessage >=> messageFrom

updateToAction :: Config -> State -> Update -> Maybe Action
updateToAction cfg NotStarted = parseUpdate $
      Start <$ command "start"
  <|> Register <$> user <*> location
updateToAction cfg _st = parseUpdate $
      Register <$> user <*> location
  <|> Ans <$> text

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) n = xs !? (n-1)

mkErrorMsg :: [Text] -> String -> Maybe QuestionDef -> Text
mkErrorMsg allFields err mqd = case qdError =<< mqd of
  Just t -> t <> "\n" <> T.pack err
  Nothing -> T.concat
    [ "Parse error: "
    , T.pack err
    , ". Please write following fields: "
    , T.intercalate ", " (maybe allFields qdAnswer mqd)
    , " delimiting them by space" ]

runParser :: Reader a -> Text -> Either String a
runParser p t = fst <$> p t

parseFieldVal :: Config -> (FieldDef, Text) -> Either String (Text, FieldVal)
parseFieldVal cfg (field, text) = (fdName field,) <$> case fdType field of
  FieldInt -> ValInt <$> runParser decimal text
  FieldFloat -> ValFloat <$> runParser double text
  FieldText -> Right $ ValText text
  FieldEncrypt -> Right $ ValEncrypt $ "ENCRYPT(" <> text <> ")"

addAnswer :: Config -> Int -> Map Text FieldVal -> Text -> Either String (Map Text FieldVal)
addAnswer cfg idx olds text = let
  mquestion = cfgQuestions cfg !? idx
  fields = case mquestion of
    Nothing -> cfgFields cfg
    Just question -> map (\name -> fromJust . find ((==name) . fdName) $ cfgFields cfg) $ qdAnswer question
  ws = T.words text
  in if length ws == length fields
     then (olds `M.union`) . M.fromList <$> mapM (parseFieldVal cfg) (zip fields ws)
     else if length fields == 1 && fdType (head fields) `elem` [FieldText, FieldEncrypt]
          then (olds `M.union`) . uncurry M.singleton <$> (parseFieldVal cfg) (head fields, text)
          else Left "Incorrect number of fields"

addRequiredFields :: Config -> Text -> Loc -> Map Text FieldVal -> IO (Map Text FieldVal)
addRequiredFields cfg user loc olds = pure $
                                      M.insert "user" (ValUser user) $
                                      M.insert "loc" (ValLoc loc) $
                                      olds

saveRow :: Config -> Map Text FieldVal -> IO Text
saveRow cfg res = pure $ "saved! " <> T.intercalate ", " (map (\(k,v) -> k <> ": " <> pp v) $ M.assocs res)
  where pp (ValInt x) = T.pack $ show x
        pp (ValFloat x) = T.pack $ show x
        pp (ValText t) = t
        pp (ValEncrypt t) = t
        pp (ValLoc loc) = T.intercalate "/" $ map ($ loc) [locCity, fromMaybe "" . locMunicip, fromMaybe "" . locRegion, locSubject]
        pp (ValUser u) = "ENC(" <> u <> ")"

handleAction :: Config -> GeoDb -> Action -> State -> Eff Action State
handleAction cfg geoDb act st = case act of
  NoOp -> pure st
  Start -> st <# do
    reply (toReplyMessage $ cfgWelcome cfg)
      { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup regKeyboard) }
    pure NoOp
  Register (UserId userId) geo -> let
    loc = findNearest geoDb (float2Double $ locationLatitude geo) (float2Double $ locationLongitude geo)
    src = T.pack $ show userId
    st' = RegisteredGeo src loc
    in st' <# do
    reply (toReplyMessage $ regAnswer loc)
    pure NoOp
  Ans t -> case addAnswer cfg (getCurrent st) (getAnswers st) t of
    Left err -> st <# do
      let fieldNames = map fdName $ cfgFields cfg
      reply (toReplyMessage $ mkErrorMsg fieldNames err $ cfgQuestions cfg !? getCurrent st)
      pure NoOp
    Right ans -> let
      idx = getCurrent st + 1
      nextQuestion = qdText <$> cfgQuestions cfg !? idx
      st'' = case nextQuestion of
        Nothing -> RegisteredGeo (stSrc st) (stLoc st)
        Just nq -> Answered
          { stSrc = stSrc st
          , stLoc = stLoc st
          , stCurrent = idx
          , stAnswers = ans
          }
      in st'' <# case nextQuestion of
                   Nothing -> do
                     res <- liftIO $ addRequiredFields cfg (stSrc st) (stLoc st) ans
                     msg <- liftIO $ saveRow cfg res
                     reply (toReplyMessage msg)
                     pure NoOp
                   Just nq -> do
                     reply (toReplyMessage nq)
                     pure NoOp
  where
    regKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
        [ [ KeyboardButton regButtonName Nothing (Just True) ] ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }
    regButtonName = fromMaybe "Register location" $ cfgRegisterButton cfg
    regAnswer loc = T.concat
      [ fromMaybe "Thank you!" (cfgRegisterAnswer cfg), "\n"
      , fromMaybe "Location: " (cfgLocationText cfg), locCity loc
      , maybe "" (("\n" <>) . qdText) $ listToMaybe $ cfgQuestions cfg
      ]

collectBot :: Config -> GeoDb -> BotApp State Action
collectBot cfg geoDb = BotApp
  { botInitialModel = initialModel
  , botAction = flip $ updateToAction cfg
  , botHandler = handleAction cfg geoDb
  , botJobs = []
  }

run :: Config -> IO ()
run cfg@Config{ cfgBot = BotCfg{ bcToken = token } } = do
  geo <- loadGeoData $ cfgGeoFile cfg
  env <- defaultTelegramClientEnv $ Token token
  startBot_ (conversationBot updateChatId $ collectBot cfg geo) env

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie cfgFile = do
  ecfg <- eitherDecodeFileStrict cfgFile
  case ecfg of
    Left err -> die err
    Right cfg -> pure cfg
