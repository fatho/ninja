module Ninja.GL.VertexAttrib where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
import           Data.Monoid
import           Data.StateVar
import qualified Data.Vector.Storable   as VS
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Ninja.GL.Object
import           Ninja.GL.Program
import           Ninja.GL.Types
import           Ninja.GL.VertexArray
import           Ninja.Util

-- * Vertex Attributes

-- | Handle to a vertex attribute.
newtype VertexAttrib = VertexAttrib GLuint deriving (Eq, Ord, Show)

-- | Returns the location of a vertex attribute.
attributeOf :: Program -> String -> IO VertexAttrib
attributeOf prog name = do
  loc <- withCString name (glGetAttribLocation (coerce prog))
  when (loc < 0) $ ioError $ userError $ "invalid attrib location of '" ++ name ++ "': " ++ show loc
  return $ VertexAttrib $ fromIntegral loc

-- | Layout of a vertex attribute.
data VertexAttribLayout = VertexAttribLayout
  { attribSize      :: Int
  , attribType      :: GLenum
  , attribNormalize :: Bool
  , attribStride    :: Int
  , attribPointer   :: IntPtr
  }
  deriving (Eq, Ord, Show, Read)

-- | Controls if a vertex attribute is enabled.
attribEnabled :: VertexAttrib -> StateVar Bool
attribEnabled va = makeStateVar g s where
  g = (GL_FALSE /=) <$> withPtrOut (glGetVertexAttribiv (coerce va) GL_VERTEX_ATTRIB_ARRAY_ENABLED)
  s True  = glEnableVertexAttribArray (coerce va)
  s False = glDisableVertexAttribArray (coerce va)

attribLayout :: VertexAttrib -> StateVar VertexAttribLayout
attribLayout va = makeStateVar g s where
  g = undefined
  s layout = glVertexAttribPointer (coerce va)
                (fromIntegral $ attribSize layout)
                (fromIntegral $ attribType layout)
                (toGLBool $ attribNormalize layout)
                (fromIntegral $ attribStride layout)
                (intPtrToPtr  $ attribPointer layout)

data VertexLayout = VertexLayout
  { vertexSize :: Int
  , vertexAttribs :: [VertexAttribInfo]
  }
  deriving (Eq, Ord, Show, Read)

data AttribLocation = AttribByName String | AttribByOrd Int
  deriving (Eq, Ord, Show, Read)

data VertexAttribInfo = VertexAttribInfo
  { vertexAttribLocation :: AttribLocation
  , vertexAttribLayout :: VertexAttribLayout
  }
  deriving (Eq, Ord, Show, Read)

instance Monoid VertexLayout where

-- | Values that can be used as vertex data to be streamed to the graphics card.
class Storable a => VertexData a where
  -- | Returns the vertex layout of the given data. The first argument is not used.
  vertexLayout :: a -> [VertexLayout]


class GVertexData a where
  gvertexLayout :: a -> [VertexLayout]