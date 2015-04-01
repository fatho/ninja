module Ninja.Util where

import           Control.Exception
import           Data.StateVar

withVar :: StateVar a -> a -> IO b -> IO b
withVar var val act = do
  oldVal <- get var
  finally
    (var $= val >> act)
    (var $= oldVal)
