{-# LANGUAGE FlexibleContexts #-}
module Graphics.Ninja.GL.VertexArray where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import           Foreign.Marshal.Array
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Graphics.Ninja.GL.Object
import           Graphics.Ninja.Util

-- | Vertex Array Handle.
newtype VertexArray = VertexArray GLuint deriving (Eq, Ord, Show)

instance Object VertexArray where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteVertexArrays (fromIntegral n) ps
  isA = liftM (/= GL_FALSE) . glIsVertexArray . objectId

instance GenObject VertexArray where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenVertexArrays (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default VertexArray where
  def = VertexArray 0

-- | The currently bound vertex array. It is set using 'glBindVertexArray' and read using 'glGetIntegerv' with
-- 'GL_VERTEX_ARRAY_BINDING'.
boundVertexArray :: StateVar VertexArray
boundVertexArray = makeStateVar getva setva where
  getva = VertexArray . fromIntegral <$> withPtrOut (glGetIntegerv GL_VERTEX_ARRAY_BINDING)
  setva = glBindVertexArray . coerce

-- | Changes the VAO for the duration of the supplied action.
withVertexArray :: (MonadBaseControl IO m, MonadIO m) => VertexArray -> m a -> m a
withVertexArray = withVar boundVertexArray
