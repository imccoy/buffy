module Views.Images where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (toList)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import ImageSearch
import ImagesSession
import qualified Urls

list :: Maybe Text -> Maybe ImageSearchSession -> Html
list _ Nothing = docTypeHtml $ toHtml $ text "Oops, something went wrong and we can't find your session"
list Nothing _ = docTypeHtml $ toHtml $ text "Oops, something went wrong and we can't find your session"
list (Just sessionId) (Just session) = docTypeHtml $ do
  h1 (toHtml $ _query session)
  forM_ (zip [0..] (toList $ _images session)) $ \(n, result) ->
    a ! href (toValue $ Urls.absolute $ Urls.image sessionId (T.pack $ show n)) $ do
      img ! src (toValue $ ImageSearch.thumbnail result)
