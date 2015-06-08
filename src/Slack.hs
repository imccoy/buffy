module Slack where

import qualified Config
import           Control.Exception
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens (_Bool, _String, AsValue, key)
import           Data.Text (Text)
import           Data.Typeable
import           Network.Wreq
import           Safe

data SlackException = SlackException Text
  deriving (Show, Typeable)

instance Exception SlackException

data User = User { realName :: Text }

slackReturn :: (AsValue r, Monad m) => Text -> (Response r) -> m Value
slackReturn k response = case response ^? responseBody . key "ok" . _Bool of
  Just True -> case response ^? responseBody . key k of
    Just v -> return v
    _      -> throw $ SlackException "no user"
  _ -> throw $ SlackException $ fromJustDef "no error" (response ^? responseBody . key "error" ._String)

postMessage :: Text -> Text -> IO Value 
postMessage channel text = do
  message <- post "https://slack.com/api/chat.postMessage" [
               "channel" := channel,
               "text" := text,
               "as_user" := ("true" :: Text),
               "token" := Config.slack_oauth_token
             ] >>= slackReturn "message"
  return message
    
userDetails :: Text -> IO User
userDetails userId = do
  user <- post "https://slack.com/api/users.info" [
            "user" := userId,
            "token" := Config.slack_oauth_token
          ] >>= slackReturn "user"
  let (Just name) = user ^? key "real_name" . _String
  return $ User { realName = name }
  
