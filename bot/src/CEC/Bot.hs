-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module CEC.Bot where

import CEC.Types

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
  | Register Location
  | Ans Text
  -- | Trust Text
  deriving (Show)

initialModel :: State
initialModel = NotStarted

location :: UpdateParser Location
location = mkParser $ updateMessage >=> messageLocation

updateToAction :: Config -> State -> Update -> Maybe Action
updateToAction cfg NotStarted = parseUpdate $
      Start <$ command "start"
  <|> Register <$> location
updateToAction cfg _st = parseUpdate $
      Register <$> location
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

saveRow :: Config -> Map Text FieldVal -> IO Text
saveRow cfg ans = pure $ "saved!" <> T.concat (map (\(k,v) -> k <> ": " <> pp v) $ M.assocs ans)
  where pp (ValInt x) = T.pack $ show x
        pp (ValFloat x) = T.pack $ show x
        pp (ValText t) = t
        pp (ValEncrypt t) = t

handleAction :: Config -> Action -> State -> Eff Action State
handleAction cfg act st = case act of
  NoOp -> pure st
  Start -> st <# do
    reply (toReplyMessage $ cfgWelcome cfg)
      { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup regKeyboard) }
    pure NoOp
  Register loc -> let
    st' = RegisteredGeo (float2Double $ locationLatitude loc) (float2Double $ locationLongitude loc)
    in st' <# do
    reply (toReplyMessage regAnswer)
    liftIO $ putStrLn $ show st'
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
        Nothing -> RegisteredGeo (stLat st) (stLon st)
        Just nq -> Answered
          { stLat = stLat st
          , stLon = stLon st
          , stCurrent = idx
          , stAnswers = ans
          }
      in st'' <# case nextQuestion of
                   Nothing -> do
                     msg <- liftIO $ saveRow cfg ans
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
    regAnswer = T.concat
      [ fromMaybe "Thank you!" (cfgRegisterAnswer cfg)
      , maybe "" (("\n" <>) . qdText) $ listToMaybe $ cfgQuestions cfg
      ]

collectBot :: Config -> BotApp State Action
collectBot cfg = BotApp
  { botInitialModel = initialModel
  , botAction = flip $ updateToAction cfg
  , botHandler = handleAction cfg
  , botJobs = []
  }

run :: Config -> IO ()
run cfg@Config{ cfgBot = BotCfg{ bcToken = token } } = do
  env <- defaultTelegramClientEnv $ Token token
  startBot_ (conversationBot updateChatId $ collectBot cfg) env

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie cfgFile = do
  ecfg <- eitherDecodeFileStrict cfgFile
  case ecfg of
    Left err -> die err
    Right cfg -> pure cfg
