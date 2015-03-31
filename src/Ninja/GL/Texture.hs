{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
module Ninja.GL.Texture where

import qualified Codec.Picture          as JP
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
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
import           Linear

import           Ninja.GL.Object
import           Ninja.GL.Types

-- | Encapsulates an OpenGL texture target.
data TextureTarget = TextureTarget GLenum GLenum deriving (Eq, Ord, Show)

-- | Texture handle
newtype Texture = Texture GLuint deriving (Eq, Ord, Show)

instance Object Texture where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteTextures (fromIntegral n) ps

instance GenObject Texture where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenTextures (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default Texture where
  def = Texture 0

-- | Controls the currently active texture unit.
activeTexture :: StateVar GLenum
activeTexture = makeStateVar g s where
  g = fromIntegral <$> alloca (\p -> glGetIntegerv GL_ACTIVE_TEXTURE p >> peek p)
  s = glActiveTexture

-- | Controls the currently bound texture for a texture target.
boundTexture :: TextureTarget -> StateVar Texture
boundTexture (TextureTarget binding target) = makeStateVar g s where
  g = Texture . fromIntegral <$> alloca (\p -> glGetIntegerv binding p >> peek p)
  s tex = glBindTexture target (objectId tex)

-- * Texture Image

type TextureLevel = GLint
type TextureInternalFormat = GLint
type TextureFormat = GLenum
type TextureType = GLenum

-- | Object convertible to texture data. d is the dimension and should be either 'V1', 'V2' or 'V3'.
class TextureData a d | a -> d where
  withRawTexture :: a -> (TextureFormat -> TextureType -> d GLsizei -> Ptr () -> IO ()) -> IO ()

instance TextureData JP.DynamicImage V2 where
  withRawTexture tex f = case tex of
        JP.ImageY8 _ -> unsupported
        JP.ImageY16 _ -> unsupported
        JP.ImageYF _ -> unsupported
        JP.ImageYA8 _ -> unsupported
        JP.ImageYA16 _ -> unsupported
        JP.ImageRGB8 img -> go img GL_RGB GL_UNSIGNED_BYTE
        JP.ImageRGB16 img -> go img GL_RGB GL_UNSIGNED_SHORT
        JP.ImageRGBF img -> go img GL_RGB GL_FLOAT
        JP.ImageRGBA8 img -> go img GL_RGBA GL_UNSIGNED_BYTE
        JP.ImageRGBA16 img -> go img GL_RGBA GL_UNSIGNED_SHORT
        JP.ImageYCbCr8 _ -> unsupported
        JP.ImageCMYK8 _ -> unsupported
        JP.ImageCMYK16 _ -> unsupported
    where
        unsupported = error "unsupported image format"
        go :: Storable (JP.PixelBaseComponent a)
            => JP.Image a -> GLenum -> GLenum -> IO ()
        go img fmt dataType = VS.unsafeWith (JP.imageData img) $ \dataArr ->
            f fmt dataType
                (fromIntegral <$> V2 (JP.imageWidth img) (JP.imageHeight img))
                (castPtr dataArr)

-- | Uploads a 1D texture to the driver.
textureImage1D :: (TextureData a V1) =>  TextureTarget -> Int -> TextureInternalFormat -> a -> IO ()
textureImage1D (TextureTarget _ target) lvl innerFmt tex = withRawTexture tex
  $ \fmt dataTy (V1 w) dat ->
      glTexImage1D target (fromIntegral lvl) innerFmt w 0 fmt dataTy dat

-- | Uploads a 2D texture to the driver.
textureImage2D :: (TextureData a V2) =>  TextureTarget -> Int -> TextureInternalFormat -> a -> IO ()
textureImage2D (TextureTarget _ target) lvl innerFmt tex = withRawTexture tex
   $ \fmt dataTy (V2 w h) dat ->
       glTexImage2D target (fromIntegral lvl) innerFmt w h 0 fmt dataTy dat

-- | Uploads a 3D texture to the driver.
textureImage3D :: (TextureData a V3) =>  TextureTarget -> Int -> TextureInternalFormat -> a -> IO ()
textureImage3D (TextureTarget _ target) lvl innerFmt tex = withRawTexture tex
   $ \fmt dataTy (V3 w h d) dat ->
       glTexImage3D target (fromIntegral lvl) innerFmt w h d 0 fmt dataTy dat

-- TODO: glGetTexImage, glTexSubImage

-- * Texture Parameters

newtype TextureWrap = TextureWrap GLenum deriving (Eq, Ord, Show)

-- | Controls texture wrapping for 2D textures.
textureWrap2D :: TextureTarget -> StateVar (TextureWrap, TextureWrap)
textureWrap2D (TextureTarget binding target) = makeStateVar g s where
  g = (,) <$> liftM (TextureWrap . fromIntegral) (alloca $ \p -> glGetTexParameteriv target GL_TEXTURE_WRAP_S p >> peek p)
          <*> liftM (TextureWrap . fromIntegral) (alloca $ \p -> glGetTexParameteriv target GL_TEXTURE_WRAP_T p >> peek p)
  s (TextureWrap s,TextureWrap t) = do
    glTexParameteri target GL_TEXTURE_WRAP_S (fromIntegral s)
    glTexParameteri target GL_TEXTURE_WRAP_T (fromIntegral t)

-- | Controls texture wrapping for 3D textures.
textureWrap3D :: TextureTarget -> StateVar (TextureWrap, TextureWrap, TextureWrap)
textureWrap3D (TextureTarget binding target) = makeStateVar g s where
  g = (,,) <$> liftM (TextureWrap . fromIntegral) (alloca $ \p -> glGetTexParameteriv target GL_TEXTURE_WRAP_S p >> peek p)
          <*> liftM (TextureWrap . fromIntegral) (alloca $ \p -> glGetTexParameteriv target GL_TEXTURE_WRAP_T p >> peek p)
          <*> liftM (TextureWrap . fromIntegral) (alloca $ \p -> glGetTexParameteriv target GL_TEXTURE_WRAP_R p >> peek p)
  s (TextureWrap s,TextureWrap t,TextureWrap r) = do
    glTexParameteri target GL_TEXTURE_WRAP_S (fromIntegral s)
    glTexParameteri target GL_TEXTURE_WRAP_T (fromIntegral t)
    glTexParameteri target GL_TEXTURE_WRAP_R (fromIntegral r)

-- | Controls the border color used for 'GL_CLAMP_TO_BORDER'.
textureBorderColor :: TextureTarget -> StateVar Color
textureBorderColor (TextureTarget _ target) = makeStateVar g s where
  g = alloca $ \p -> glGetTexParameterfv target GL_TEXTURE_BORDER_COLOR (castPtr p) >> peek p
  s col = alloca $ \p -> poke p col >> glTexParameterfv target GL_TEXTURE_BORDER_COLOR (castPtr p)

pattern WrapRepeat = TextureWrap GL_REPEAT
pattern WrapMirroredRepeat = TextureWrap GL_MIRRORED_REPEAT
pattern WrapClampToEdge = TextureWrap GL_CLAMP_TO_EDGE
pattern WrapClampToBorder = TextureWrap GL_CLAMP_TO_BORDER

-- * Texture Filtering

newtype TextureFilter = TextureFilter GLenum deriving (Eq, Ord, Show)

-- | Controls the 'GL_TEXTURE_MIN_FILTER'.
textureMinFilter :: TextureTarget -> StateVar TextureFilter
textureMinFilter (TextureTarget _ target) = makeStateVar g s where
  g = TextureFilter . fromIntegral <$> alloca (\p -> glGetTexParameteriv target GL_TEXTURE_MIN_FILTER p >> peek p)
  s (TextureFilter f) = glTexParameteri target GL_TEXTURE_MIN_FILTER (fromIntegral f)

-- | Controls the 'GL_TEXTURE_MAG_FILTER'.
textureMagFilter :: TextureTarget -> StateVar TextureFilter
textureMagFilter (TextureTarget _ target) = makeStateVar g s where
  g = TextureFilter . fromIntegral <$> alloca (\p -> glGetTexParameteriv target GL_TEXTURE_MAG_FILTER p >> peek p)
  s (TextureFilter f) = glTexParameteri target GL_TEXTURE_MAG_FILTER (fromIntegral f)

-- | Generates the mip maps.
generateMipMap :: TextureTarget -> IO ()
generateMipMap (TextureTarget _ target) = glGenerateMipmap target

pattern FilterNearest = TextureFilter GL_NEAREST
pattern FilterLinear = TextureFilter GL_LINEAR
pattern FilterNearestMipMapNearest = TextureFilter GL_NEAREST_MIPMAP_NEAREST
pattern FilterLinearMipMapNearest = TextureFilter GL_LINEAR_MIPMAP_NEAREST
pattern FilterNearestMipMapLinear = TextureFilter GL_NEAREST_MIPMAP_LINEAR
pattern FilterLinearMipMapLinear = TextureFilter GL_LINEAR_MIPMAP_LINEAR

-- * Texture Targets

pattern Texture1D = TextureTarget GL_TEXTURE_BINDING_1D GL_TEXTURE_1D
pattern Texture2D = TextureTarget GL_TEXTURE_BINDING_2D GL_TEXTURE_2D
pattern Texture3D = TextureTarget GL_TEXTURE_BINDING_3D GL_TEXTURE_3D

pattern Texture1DArray = TextureTarget GL_TEXTURE_BINDING_1D_ARRAY GL_TEXTURE_1D_ARRAY
pattern Texture2DArray = TextureTarget GL_TEXTURE_BINDING_2D_ARRAY GL_TEXTURE_2D_ARRAY

pattern TextureRectangle = TextureTarget GL_TEXTURE_BINDING_RECTANGLE GL_TEXTURE_RECTANGLE
pattern TextureCubeMap = TextureTarget GL_TEXTURE_BINDING_CUBE_MAP GL_TEXTURE_CUBE_MAP

pattern Texture2DMultisample = TextureTarget GL_TEXTURE_BINDING_2D_MULTISAMPLE GL_TEXTURE_BINDING_2D_MULTISAMPLE
pattern Texture2DMultisampleArray = TextureTarget GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
