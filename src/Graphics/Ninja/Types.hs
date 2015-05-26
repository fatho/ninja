{-# LANGUAGE ConstraintKinds #-}
module Graphics.Ninja.Types where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control

type ControlIO m = (MonadBaseControl IO m, MonadIO m)
