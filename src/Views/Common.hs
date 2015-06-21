module Views.Common where


import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (toList)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

jsClose :: Html
jsClose = docTypeHtml $ do
  script "window.close();"
