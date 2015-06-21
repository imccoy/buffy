module Urls where

import qualified Data.Text as T

import qualified Config as Config

absolute u = Config.root_url `T.append` u

images imageSessionId = "/images/" `T.append` imageSessionId
image imageSessionId imageId = images imageSessionId `T.append` "/" `T.append` imageId
