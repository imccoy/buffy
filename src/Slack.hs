module Slack where

import qualified Config
import Control.Lens
import Control.Monad (void)
import Data.Text (Text)
import Network.Wreq

postMessage :: Text -> Text -> IO ()
postMessage channel text = void $ post "https://slack.com/api/chat.postMessage/" [
                                       "channel" := channel,
                                       "text" := text,
                                       "token" := Config.slack_oauth_token]

