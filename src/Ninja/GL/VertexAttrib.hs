{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Ninja.GL.VertexAttrib where

import           Control.Applicative
import           Control.Exception
import           Control.Lens           hiding (coerce, from)
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
import           GHC.Generics           (Generic, Rep)
import qualified GHC.Generics           as Generics
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear

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
  { _attribSize      :: Int
  , _attribType      :: GLenum
  , _attribNormalize :: Bool
  , _attribStride    :: Int
  , _attribPointer   :: IntPtr
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''VertexAttribLayout

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
                (fromIntegral $ view attribSize layout)
                (fromIntegral $ view attribType layout)
                (toGLBool $ view attribNormalize layout)
                (fromIntegral $ view attribStride layout)
                (intPtrToPtr  $ view attribPointer layout)

data VertexLayout = VertexLayout
  { _vertexSize    :: Int
  , _vertexAttribs :: [VertexAttribLayout]
  }
  deriving (Eq, Ord, Show, Read)
{-
data AttribLocation = AttribByName String | AttribByOrd Int
  deriving (Eq, Ord, Show, Read)

data VertexAttribInfo = VertexAttribInfo
  { _vertexAttribLocation :: AttribLocation
  , _vertexAttribLayout   :: VertexAttribLayout
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''VertexAttribInfo
-}

makeLenses ''VertexLayout

applyLayout :: VertexLayout -> IO ()
applyLayout = mapM_ go . zip [0..] . view vertexAttribs where
  go (idx, layout) = do
    let loc = VertexAttrib idx
    attribEnabled loc $= True
    attribLayout loc $= layout

instance Monoid VertexLayout where
  mempty = VertexLayout 0 []
  mappend vl1 vl2 =
    let size1 = view vertexSize vl1
        size2 = view vertexSize vl2
    in VertexLayout
        { _vertexSize = size1 + size2
        , _vertexAttribs =
               map (attribStride +~ fromIntegral size2) (view vertexAttribs vl1)
            ++ map ( (attribPointer +~ fromIntegral (view vertexSize vl1))
                   . (attribStride +~ fromIntegral size1)
                   ) (view vertexAttribs vl2)
        }

-- | Values that can be used as vertex data to be streamed to the graphics card.
class Storable a => VertexData a where
  -- | Returns the vertex layout of the given data. The first argument is not used.
  vertexLayout :: a -> VertexLayout
  default vertexLayout :: (Generic a, GVertexData (Rep a)) => a -> VertexLayout
  vertexLayout = gvertexLayout . Generics.from

class GVertexData f where
  gvertexLayout :: f p -> VertexLayout

instance GVertexData Generics.U1 where
  gvertexLayout _ = mempty

instance (GVertexData f, GVertexData g) => GVertexData (f Generics.:*: g) where
  gvertexLayout _ = gvertexLayout x <> gvertexLayout y where
    x = undefined :: f p
    y = undefined :: g p

instance VertexData c => GVertexData (Generics.K1 i c) where
  gvertexLayout _ = vertexLayout (undefined :: c)

instance GVertexData f => GVertexData (Generics.M1 i t f) where
  gvertexLayout _ = gvertexLayout (undefined :: f p)

storableVertexLayout :: Storable a => GLenum -> Int -> a -> VertexLayout
storableVertexLayout dataType num x = VertexLayout (sizeOf x) [VertexAttribLayout num dataType False (sizeOf x) 0]

instance VertexData Float where
  vertexLayout = storableVertexLayout GL_FLOAT 1

instance VertexData (V1 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 1

instance VertexData (V2 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 2

instance VertexData (V3 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 3

instance VertexData (V4 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 4
