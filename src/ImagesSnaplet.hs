module ImagesSnaplet where

import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector
import Snap
import Snap.Snaplet

import qualified ImageSearch as ImageSearch

type ImageSession = (Vector ImageSearch.Result)
data Images = Images { imagesMVar :: MVar (Map (Text, Text) ImageSession) }

imagesInit :: SnapletInit b Images
imagesInit = makeSnaplet "Images" "A snaplet providing some images to play with" Nothing $ liftIO $ do 
  mvar <- liftIO $ newMVar Map.empty
  return $ Images mvar

remember' :: Text -> Text -> ImageSession -> Map (Text, Text) ImageSession -> Map (Text, Text) ImageSession
remember' user channel session = Map.insert (user, channel) session

remember user channel session = do 
  mvar <- get >>= return . imagesMVar
  liftIO $ modifyMVar_ mvar $ return . remember' user channel session

fetch' :: Text -> Text -> Int -> Map (Text, Text) ImageSession -> Maybe Text
fetch' user channel n sessions = do
  session <- Map.lookup (user, channel) sessions
  searchResult <- session Data.Vector.!? (n - 1)
  return $ ImageSearch.link searchResult

fetch user channel n = do
  mvar <- get >>= return . imagesMVar
  usersMap <- liftIO $ readMVar mvar
  let link = fetch' user channel n usersMap
  return link 
