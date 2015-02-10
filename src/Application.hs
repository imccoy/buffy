{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

import Control.Lens
import Snap.Snaplet

import qualified ImagesSnaplet

data App = App { _images :: Snaplet ImagesSnaplet.Images }

makeLenses ''App

type AppHandler = Handler App App
