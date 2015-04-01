module Ninja.GL.VertexArray where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Ninja.GL.Object
import           Ninja.Util

-- | Vertex Array Handle.
newtype VertexArray = VertexArray GLuint deriving (Eq, Ord, Show)

instance Object VertexArray where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteVertexArrays (fromIntegral n) ps

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
  getva = VertexArray . fromIntegral <$> alloca (\p -> glGetIntegerv GL_VERTEX_ARRAY_BINDING p >> peek p)
  setva = glBindVertexArray . coerce

-- | Changes the VAO for the duration of the supplied action.
withVertexArray :: VertexArray -> IO a -> IO a
withVertexArray = withVar boundVertexArray
