module Ninja.Util where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.StateVar
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

-- | Modifies a StateVar locally.
withVar :: StateVar a -> a -> IO b -> IO b
withVar var val act = do
  oldVal <- get var
  finally
    (var $= val >> act)
    (var $= oldVal)

-- | Allocates memory and passes the pointer to the function, returning the contents afterwards.
withPtrOut :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtrOut f = liftIO $ alloca $ liftM2 (>>) f peek

-- | Allocates memory, copies the supplied value to that location and passes the pointer to the given function.
withPtrIn :: Storable a => a -> (Ptr a -> IO b) -> IO b
withPtrIn v f = alloca $ \p -> poke p v >> f p