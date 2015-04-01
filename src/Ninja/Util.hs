module Ninja.Util where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.StateVar
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

withVar :: StateVar a -> a -> IO b -> IO b
withVar var val act = do
  oldVal <- get var
  finally
    (var $= val >> act)
    (var $= oldVal)

withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr f = liftIO $ alloca $ liftM2 (>>) f peek
