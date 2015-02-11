module ImageSearch where

import           Control.Lens
import           Control.Monad (join)
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.ByteString (ByteString)
import           Data.Vector (Vector, forM, fromList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Connection               (TLSSettings (..))
import           Network.HTTP.Client.TLS          (mkManagerSettings, tlsManagerSettings)
import           Network.Wreq

import qualified Config

data Result = Result { thumbnail :: Text, link :: Text }
  deriving (Show)

search :: Text -> IO (Maybe (Vector Result))
search q = do 
  let opts = defaults & param "key" .~ [Config.google_app_key]
                      & param "cx" .~ [Config.search_engine_id]
                      & param "q" .~ [q]
                      & param "searchType" .~ ["image"]
                      & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  response <- getWith opts "https://www.googleapis.com/customsearch/v1"
  let maybeItems = response ^? responseBody . key "items" . _Array
  return $ do 
    items <- maybeItems
    forM items $ \item -> do
       thumbnail <- item ^? key "image" . key "thumbnailLink" . _String
       link <- item ^? key "link" . _String
       return $ Result thumbnail link
