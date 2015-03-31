module Ninja.GL.Object where

import Control.Monad
import Control.Monad.IO.Class
import Graphics.GL.Types

-- | Generic interface to an OpenGL object.
-- <https://www.opengl.org/wiki/OpenGL_Object>
class Object a where
  {-# MINIMAL objid, (delete | delete1) #-}

  -- | glDelete* call
  delete :: MonadIO m => [a] -> m ()
  delete = mapM_ delete1

  -- | Variant of delete for deleting just one object.
  delete1 :: MonadIO m => a -> m ()
  delete1 = delete . (:[])

  -- | The underlying OpenGL object ID
  objid   :: a -> GLuint

-- | Generic interface to an object that also can be user generated.
class Object a => GenObject a where
  {-# MINIMAL (gen | gen1) #-}

  -- | glGen* call, creating multiple objects at once
  gen :: MonadIO m => Int -> m [a]
  gen n  = replicateM n gen1

  -- | Variant of 'gen' to generate just one object.
  gen1 :: MonadIO m => m a
  gen1 = head `liftM` gen 1