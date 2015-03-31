module Ninja.GL.VAO where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Graphics.GL.Types
import Graphics.GL.Core33
import Data.Default.Class
import Data.StateVar

import Ninja.GL.Object

-- | Vertex Array Handle.
newtype VAO = VAO GLuint deriving (Eq, Ord, Show)

instance Object VAO where
  objid = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteVertexArrays (fromIntegral n) ps

instance GenObject VAO where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenVertexArrays (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default VAO where
  def = VAO 0

-- | The currently bound vertex array. It is set using 'glBindVertexArray' and read using 'glGetIntegerv' with
-- 'GL_VERTEX_ARRAY_BINDING'.
boundVertexArray :: StateVar VAO
boundVertexArray = makeStateVar getva setva where
  getva = VAO . fromIntegral <$> alloca (\p -> glGetIntegerv GL_VERTEX_ARRAY_BINDING p >> peek p)
  setva = glBindVertexArray . coerce