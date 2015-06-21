module ImagesSession where
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector
import Snap
import Snap.Snaplet
import System.Random (randomIO)
import System.IO.Unsafe (unsafePerformIO)

import qualified ImageSearch as ImageSearch


data ImageSearchSession = ImageSearchSession { _query :: Text, _channel :: Text, _userId :: Text, _images :: Vector ImageSearch.Result }
makeLenses ''ImageSearchSession

imageSearchSessions :: MVar (Map Text ImageSearchSession)
imageSearchSessions = unsafePerformIO $ newMVar Map.empty

randomInt :: IO Int
randomInt = randomIO

generateSessionId :: IO Text
generateSessionId = do
  randomInt >>= return . T.pack . show

generateUnusedSessionId :: Map Text v -> IO Text
generateUnusedSessionId map = do
  candidateSessionId <- generateSessionId
  if candidateSessionId `Map.member` map
    then generateUnusedSessionId map
    else return candidateSessionId

saveSession :: ImageSearchSession -> IO Text
saveSession session = modifyMVar imageSearchSessions $ \sessions -> do
  sessionId <- generateUnusedSessionId sessions
  return (Map.insert sessionId session sessions, sessionId)

loadSession :: Text -> IO (Maybe ImageSearchSession)
loadSession sessionId = readMVar imageSearchSessions >>= return . Map.lookup sessionId

remember :: Text -> Text -> Text -> Vector (ImageSearch.Result) -> Handler b s Text 
remember query channelId userId searchResults = do
  liftIO $ saveSession (ImageSearchSession query channelId userId searchResults)

fetch :: Text -> Handler b s (Maybe ImageSearchSession)
fetch sessionId = do
  liftIO $ loadSession sessionId
