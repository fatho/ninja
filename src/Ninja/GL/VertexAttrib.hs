{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Ninja.GL.VertexAttrib where

import           Control.Applicative
import           Control.Lens           hiding (coerce, from)
import           Control.Monad
import           Data.Coerce
import           Data.Monoid
import           Data.StateVar
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics           (Generic, Rep)
import qualified GHC.Generics           as Generics
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear

import           Ninja.GL.Program
import           Ninja.GL.Types
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

attribLayout :: VertexAttrib -> SettableStateVar VertexAttribLayout
attribLayout va = makeSettableStateVar s where
  s layout = glVertexAttribPointer (coerce va)
                (fromIntegral $ view attribSize layout)
                (fromIntegral $ view attribType layout)
                (toGLBool $ view attribNormalize layout)
                (fromIntegral $ view attribStride layout)
                (intPtrToPtr  $ view attribPointer layout)

-- | Describes the layout of one vertex.
data VertexLayout = VertexLayout
  { _vertexSize    :: Int
  -- ^ total size of one vertex
  , _vertexAttribs :: [VertexAttribInfo]
  -- ^ vertex attributes
  }
  deriving (Eq, Ord, Show, Read)

-- | Description of one vertex attribute.
data VertexAttribInfo = VertexAttribInfo
  { _vertexAttribName   :: String
  -- ^ Name of the attribute which is used for looking up the location.
  , _vertexAttribWidth     :: Int
  -- ^ the number of attribute locations this attribute takes up.
  , _vertexAttribLayout :: VertexAttribLayout
  -- ^ The actual layout of the attribute.
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''VertexAttribInfo

makeLenses ''VertexLayout

-- | Applies a vertex layout, directly indexing the locations.
-- WARNING: Some data types may take more than one attribute location. This was not tested.
applyLayoutByIndex :: Int -> VertexLayout -> IO ()
applyLayoutByIndex baseLoc = go baseLoc . view vertexAttribs where
  go _ [] = return ()
  go idx (l:ls) = do
    let loc = VertexAttrib $ fromIntegral idx
    attribEnabled loc $= True
    attribLayout loc $= view vertexAttribLayout l
    go (idx + view vertexAttribWidth l) ls

-- | Applies a vertex layout using the names of the attributes to determine the locations.
applyLayoutByName :: Program -> VertexLayout -> IO ()
applyLayoutByName prog = mapM_ go . view vertexAttribs where
  go layout = do
    loc <- attributeOf prog (layout ^. vertexAttribName)
    attribEnabled loc $= True
    attribLayout loc $= view vertexAttribLayout layout

instance Monoid VertexLayout where
  mempty = VertexLayout 0 []
  mappend vl1 vl2 =
    let size1 = view vertexSize vl1
        size2 = view vertexSize vl2
    in VertexLayout
        { _vertexSize = size1 + size2
        , _vertexAttribs =
               map (vertexAttribLayout . attribStride +~ fromIntegral size2) (view vertexAttribs vl1)
            ++ map ( (vertexAttribLayout . attribPointer +~ fromIntegral (view vertexSize vl1))
                   . (vertexAttribLayout . attribStride +~ fromIntegral size1)
                   ) (view vertexAttribs vl2)
        }

-- | Values that can be used as vertex data to be streamed to the graphics card.
class Storable a => VertexData a where
  -- | Returns the vertex layout of the given data. The first argument is not used.
  vertexLayout :: a -> VertexLayout
  default vertexLayout :: (Generic a, GVertexData (Rep a)) => a -> VertexLayout
  vertexLayout = gvertexLayout . Generics.from

storableVertexLayout :: Storable a => GLenum -> Int -> Int -> a -> VertexLayout
storableVertexLayout dataType width num x = VertexLayout (sizeOf x) 
  [ VertexAttribInfo "" width $ VertexAttribLayout num dataType False (sizeOf x) 0 ]

instance VertexData Float where
  vertexLayout = storableVertexLayout GL_FLOAT 1 1

instance VertexData (V1 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 1 1

instance VertexData (V2 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 1 2

instance VertexData (V3 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 1 3

instance VertexData (V4 Float) where
  vertexLayout = storableVertexLayout GL_FLOAT 1 4


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

instance (Generics.Selector t, GVertexData f) => GVertexData (Generics.M1 Generics.S t f) where
  gvertexLayout _ = gvertexLayout (undefined :: f p) & vertexAttribs.traverse.vertexAttribName %~ (prefix++) where
    prefix = Generics.selName (undefined :: Generics.S1 t f a)

instance GVertexData f => GVertexData (Generics.M1 Generics.C t f) where
  gvertexLayout _ = gvertexLayout (undefined :: f p)

instance GVertexData f => GVertexData (Generics.M1 Generics.D t f) where
  gvertexLayout _ = gvertexLayout (undefined :: f p)
