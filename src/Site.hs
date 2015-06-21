module Site
  ( app
  ) where

import           Blaze.ByteString.Builder.ByteString as BB
import           Blaze.ByteString.Builder.Char.Utf8 as BBC
import           Control.Applicative
import           Control.Lens
import           Control.Monad (join, forM_, liftM, liftM2, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Attoparsec.Text (decimal, endOfInput, parseOnly)
import           Data.Aeson
import           Data.Aeson.Encode (encodeToByteStringBuilder)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.Either.Combinators (rightToMaybe)
import           Data.Maybe (fromJust)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import           Safe
import           Snap.Core
import           Snap.Snaplet
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import           Application
import qualified ImageSearch
import qualified ImagesSession
import qualified Slack
import qualified Urls
import qualified Views.Common
import qualified Views.Images

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

handleImageSearch :: Text -> Text -> Text -> Handler App App ()
handleImageSearch q channel user = do
  results <- liftIO $ ImageSearch.search q
  handleImageSearchResults q channel user results

handleImageSearchResults :: Text -> Text -> Text -> (Maybe (Vector.Vector ImageSearch.Result)) -> Handler App App ()
handleImageSearchResults _ _ _ Nothing = writeBS "oh no, no image response"
handleImageSearchResults query channel user (Just imageResults) = do
  imageSessionId <- ImagesSession.remember query channel user imageResults
  writeBS . encodeUtf8 $ "<" `T.append` (Urls.absolute $ Urls.images imageSessionId) `T.append` ">"

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

handleImagesGet :: Handler App App ()
handleImagesGet = method GET $ do
  imageSessionId <- getParam "imageSessionId" >>= return . fmap decodeUtf8
  imageSession <- maybe (return Nothing)
                        ImagesSession.fetch
                        imageSessionId
  writeBuilder $ renderHtmlBuilder $ Views.Images.list imageSessionId imageSession

imageFromSession :: ImagesSession.ImageSearchSession -> Int -> Maybe ImageSearch.Result
imageFromSession imageSession imageId = (Vector.!?) (imageSession ^. ImagesSession.images) imageId

handleImageGet :: Handler App App ()
handleImageGet = method GET $ do
  imageSessionId <- getParam "imageSessionId" >>= return . fmap decodeUtf8
  imageId <- getParam "imageId" >>= return . (\s -> rightToMaybe . parseOnly (decimal <* endOfInput) . decodeUtf8 =<< s)
  imageSession <- maybe (return Nothing)
                        ImagesSession.fetch
                        imageSessionId
  let image = join $ liftM2 imageFromSession imageSession imageId
  case liftM2 Slack.postMessage ((^. ImagesSession.channel) <$> imageSession) (ImageSearch.link <$> image) of
    Just doPost -> void $ liftIO doPost
    Nothing -> writeBS "Nothing to post"
  writeBuilder $ renderHtmlBuilder $ Views.Common.jsClose
  

  

routes :: [(ByteString, Handler App App ())]
routes = [ 
  ("/incoming_command", handleIncomingCommand),
  (encodeUtf8 $ Urls.images ":imageSessionId", handleImagesGet),
  (encodeUtf8 $ Urls.image ":imageSessionId" ":imageId", handleImageGet)
 ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    return $ App

