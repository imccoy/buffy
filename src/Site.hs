module Site
  ( app
  ) where

import           Blaze.ByteString.Builder.ByteString as BB
import           Blaze.ByteString.Builder.Char.Utf8 as BBC
import           Control.Applicative
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Attoparsec.Text (decimal, endOfInput, parseOnly)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import           Safe
import           Snap.Core
import           Snap.Snaplet

import           Application
import qualified ImageSearch
import qualified ImagesSnaplet
import qualified Slack

data Command = ImageSearchCommand Text
             | ImageShowCommand Int
             | CommandParseError Text


parseCommand :: ByteString -> Command
parseCommand incoming = parse' cmd args
  where 
    cmd = C.takeWhile (not . isSpace) incoming
    args = T.strip . decodeUtf8 $ C.drop (C.length cmd) incoming
    parse' "image" q = ImageSearchCommand q
    parse' "send" q = case parseOnly (decimal <* endOfInput) q
      of (Right n) -> ImageShowCommand n
         _ -> CommandParseError $ "Can't send image '" `T.append` q `T.append` "', it's not a number"

handleImageSearchResults :: (Maybe (Vector.Vector ImageSearch.Result)) -> Text -> Text -> Handler App App ()
handleImageSearchResults Nothing _ _ = writeBS "oh no, no image response"
handleImageSearchResults (Just imageResults) channel user = do
  with images $ ImagesSnaplet.remember channel user imageResults
  sendImageSearchResponse imageResults channel user

handleImageSearch :: Text -> Text -> Text -> Handler App App ()
handleImageSearch q channel user = do
  writeBuilder $ BB.fromByteString "searching '" <> BBC.fromText q <> BB.fromByteString "'\n"
  results <- liftIO $ ImageSearch.search q
  handleImageSearchResults results channel user

sendImageSearchResponse :: Vector.Vector ImageSearch.Result -> Text -> Text -> Handler App App ()
sendImageSearchResponse results channel user = do
  forM_ (zip [1..] $ Vector.toList results) $ \(n, result) -> do
    let ss = [C.pack (show n), 
              " ",
              encodeUtf8 $ ImageSearch.thumbnail result,
              "\n"]
    writeBuilder $ mconcat [ BB.fromByteString s | s <- ss]

handleImageShow :: Int -> Text -> Text -> Handler App App ()
handleImageShow n channel user = do
  url <- with images $ ImagesSnaplet.fetch channel user n
  case url of
    Nothing -> writeBuilder $ BBC.fromText (fromJustDef "No images here" url)
    (Just u) -> liftIO (Slack.postMessage channel u) >> writeLBS "Posted"

handleIncomingCommand :: Handler App App ()
handleIncomingCommand = method POST $ do
  channelId <- getPostParam "channel_id" >>= return . fmap decodeUtf8
  userId <- getPostParam "user_id" >>= return . fmap decodeUtf8
  unparsedCommand <- getPostParam "text"
  go (fmap parseCommand unparsedCommand) channelId userId
  where
    go Nothing _ _ = writeBS "no command understood"
    go _ Nothing _ = writeBS "where's my channel?"
    go _ _ Nothing = writeBS "where's my user?"
    go (Just cmd) (Just channelId) (Just userId) = go' cmd channelId userId
    go' (ImageSearchCommand q) = handleImageSearch q
    go' (ImageShowCommand n) = handleImageShow n

routes :: [(ByteString, Handler App App ())]
routes = [ ("/incoming_command", handleIncomingCommand)]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    is <- nestSnaplet "images" images $ ImagesSnaplet.imagesInit
    addRoutes routes
    return $ App is

