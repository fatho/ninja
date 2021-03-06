{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
module Graphics.Ninja.GL.Framebuffer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import qualified Data.Vector.Storable        as VS
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Graphics.Ninja.GL.Object
import           Graphics.Ninja.Util

newtype Framebuffer = Framebuffer GLuint deriving (Eq, Ord, Show)

data FramebufferTarget = FramebufferTarget { fbTarget :: GLenum, fbTargetBinding :: GLenum }
    deriving (Eq, Ord, Show)

-- | Framebuffer target for rendering operations.
pattern DrawFramebuffer = FramebufferTarget GL_DRAW_FRAMEBUFFER GL_DRAW_FRAMEBUFFER_BINDING
-- | Framebuffer target for readback operations.
pattern ReadFramebuffer = FramebufferTarget GL_READ_FRAMEBUFFER GL_READ_FRAMEBUFFER_BINDING
-- | Framebuffer target binding draw and read targets simultaneously.
pattern AllFramebuffer  = FramebufferTarget GL_FRAMEBUFFER GL_FRAMEBUFFER_BINDING

type FramebufferAttachement = GLenum

instance Object Framebuffer where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteFramebuffers (fromIntegral n) ps
  isA = liftM (/= GL_FALSE) . glIsFramebuffer. objectId

instance GenObject Framebuffer where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenFramebuffers (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default Framebuffer where
  def = Framebuffer 0

-- | Wrapper for 'glBindFramebuffer' and 'glGetIntegerv' with GL_*FRAMEBUFFER_BINDING
boundFrameBuffer :: FramebufferTarget -> StateVar Framebuffer
boundFrameBuffer FramebufferTarget{..} = makeStateVar g s where
  g = Framebuffer . fromIntegral <$> withPtrOut (glGetIntegerv fbTargetBinding)
  s = glBindFramebuffer fbTarget . objectId

-- | Bind an FBO during the execution of the supplied action, revert to default afterwards
withFBO :: (MonadBaseControl IO m, MonadIO m) => FramebufferTarget -> Framebuffer -> m a -> m a
withFBO = withVar . boundFrameBuffer

-- TODO: some nice interface for attaching textures to FBO
