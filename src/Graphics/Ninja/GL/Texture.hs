{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
module Graphics.Ninja.GL.Texture where

import qualified Codec.Picture                                 as JP
import qualified Codec.Picture.Types                           as JP
import           Control.Applicative
import           Control.Lens                                  hiding (coerce)
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import qualified Data.Vector.Storable                          as VS
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import qualified Graphics.GL.Ext.ARB.ClearTexture              as TexClear
import qualified Graphics.GL.Ext.ARB.TextureStorage            as TexStore
import qualified Graphics.GL.Ext.ARB.TextureStorageMultisample as TexStore
import           Graphics.GL.Types
import           Linear

import           Graphics.Ninja.GL.Object
import           Graphics.Ninja.GL.Types
import           Graphics.Ninja.Util

-- | Encapsulates an OpenGL texture target.
data TextureTarget = TextureTarget { texTargetBinding :: GLenum, texTarget :: GLenum } deriving (Eq, Ord, Show)

-- | Type of a texture unit 'GL_TEXTURE0', 'GL_TEXTURE1', ...
type TextureUnit = GLenum

-- | Type of mipmap level indices.
type MipmapLevel = Int

-- | Texture handle
newtype Texture = Texture GLuint deriving (Eq, Ord, Show)

instance Object Texture where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteTextures (fromIntegral n) ps
  isA = liftM (/= GL_FALSE) . glIsTexture . objectId

instance GenObject Texture where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenTextures (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default Texture where
  def = Texture 0

-- | Controls the currently active texture unit.
activeTexture :: StateVar TextureUnit
activeTexture = makeStateVar g s where
  g = fromIntegral <$> withPtrOut (glGetIntegerv GL_ACTIVE_TEXTURE)
  s = glActiveTexture

-- | Changes the texture unit for the duration of the supplied action.
withActiveTexture :: (MonadBaseControl IO m, MonadIO m) => TextureUnit -> m a -> m a
withActiveTexture = withVar activeTexture

-- | Controls the currently bound texture for a texture target.
boundTexture :: TextureTarget -> StateVar Texture
boundTexture (TextureTarget binding target) = makeStateVar g s where
  g = Texture . fromIntegral <$> withPtrOut (glGetIntegerv binding)
  s tex = glBindTexture target (objectId tex)

-- | Changes the texture for the duration of the supplied action.
withTexture :: (MonadBaseControl IO m, MonadIO m) => TextureTarget -> Texture -> m a -> m a
withTexture target = withVar (boundTexture target)

-- * Textures from file

-- | Loads a texture from a file using 'JP.readImage'.
textureFromFile :: MonadBase IO m => FilePath -> Bool -> m Texture
textureFromFile path generateMipmaps = liftBase $ do
  img <- JP.readImage path >>= \case
    Left err -> ioError $ userError $ "failed to load image: " ++ err
    Right x -> return x
  tex <- gen1
  withTexture Texture2D tex $ do
    textureImage Texture2D 0 GL_RGBA8 $= img
    textureWrap Texture2D $= V2 GL_CLAMP_TO_EDGE GL_CLAMP_TO_EDGE
    if generateMipmaps
        then do
            generateMipMap Texture2D
            textureMinFilter Texture2D $= GL_LINEAR_MIPMAP_LINEAR
        else textureMinFilter Texture2D $= GL_LINEAR
    textureMagFilter Texture2D $= GL_LINEAR
  return tex

-- * Texture Image

type TextureLevel = GLint
type TextureInternalFormat = GLint
type TextureFormat = GLenum
type TextureType = GLenum

-- | Object convertible to texture data. d is the dimension and should be either 'V1', 'V2' or 'V3'.
class TextureData a d | a -> d where
  withRawTexture :: (MonadBaseControl IO m, MonadIO m) => a -> (TextureFormat -> TextureType -> d GLsizei -> Ptr () -> m ()) -> m ()

-- | Use a JuicyPixels image as texture source.
-- Since JuicyPixels stores images top to bottom whereas OpenGL stores them bottom to top,
-- a temporary flipped copy is created before invoking the continuation.
withImage :: (Storable (JP.PixelBaseComponent a), MonadBaseControl IO m, JP.Pixel a)
             => GLenum -> GLenum -> JP.Image a
             -> (TextureFormat -> TextureType -> V2 GLsizei -> Ptr () -> m c)
             -> m c
withImage fmt dataType img f = liftBaseOp (VS.unsafeWith (JP.imageData $ flipImage img)) action where
  action dataArr =
    f fmt
      dataType
      (fromIntegral <$> V2 (JP.imageWidth img) (JP.imageHeight img))
      (castPtr dataArr)

instance TextureData (JP.Image JP.PixelRGBA8) V2 where
  withRawTexture = withImage GL_RGBA GL_UNSIGNED_BYTE

instance TextureData (JP.Image JP.PixelRGBA16) V2 where
  withRawTexture = withImage GL_RGBA GL_UNSIGNED_SHORT

instance TextureData (JP.Image JP.PixelRGB8) V2 where
  withRawTexture = withImage GL_RGB GL_UNSIGNED_BYTE

instance TextureData (JP.Image JP.PixelRGB16) V2 where
  withRawTexture = withImage GL_RGB GL_UNSIGNED_SHORT

instance TextureData (JP.Image JP.PixelRGBF) V2 where
  withRawTexture = withImage GL_RGB GL_FLOAT

instance TextureData (JP.Image JP.Pixel8) V2 where
  withRawTexture = withImage GL_RED GL_UNSIGNED_BYTE

instance TextureData (JP.Image JP.Pixel16) V2 where
  withRawTexture = withImage GL_RED GL_UNSIGNED_SHORT

instance TextureData (JP.Image JP.PixelF) V2 where
  withRawTexture = withImage GL_RED GL_FLOAT

instance TextureData (JP.Image JP.PixelYA8) V2 where
  withRawTexture = withImage GL_RG GL_UNSIGNED_BYTE

instance TextureData (JP.Image JP.PixelYA16) V2 where
  withRawTexture = withImage GL_RG GL_UNSIGNED_SHORT

-- | Note that OpenGL requires the texture origin in the bottom left-hand corner, whereas JuicyPixels uses the top left-hand corner.
-- Therefore, a copy of the image is flipped on the fly before being loaded.
instance TextureData JP.DynamicImage V2 where
  withRawTexture tex =
      case tex of
        JP.ImageY8 img -> withRawTexture img
        JP.ImageY16 img -> withRawTexture img
        JP.ImageYF img -> withRawTexture img
        JP.ImageYA8 img -> withRawTexture img
        JP.ImageYA16 img -> withRawTexture img
        JP.ImageRGB8 img -> withRawTexture img
        JP.ImageRGB16 img -> withRawTexture img
        JP.ImageRGBF img -> withRawTexture img
        JP.ImageRGBA8 img -> withRawTexture img
        JP.ImageRGBA16 img -> withRawTexture img
        JP.ImageYCbCr8 img -> withRawTexture (toRGBA8 img)
        JP.ImageCMYK8 img -> withRawTexture (toRGBA8 img)
        JP.ImageCMYK16 img -> withRawTexture (toRGBA16 img)
    where
        toRGBA8 :: JP.ColorSpaceConvertible a JP.PixelRGB8 => JP.Image a -> JP.Image JP.PixelRGBA8
        toRGBA8 x = JP.promoteImage (JP.convertImage x :: JP.Image JP.PixelRGB8)
        toRGBA16 :: JP.ColorSpaceConvertible a JP.PixelRGB16 => JP.Image a -> JP.Image JP.PixelRGBA16
        toRGBA16 x = JP.promoteImage (JP.convertImage x :: JP.Image JP.PixelRGB16)

-- | Provides generic access to textures of varying dimensions.
class TextureAccess d where
  -- | Uploads image data and allocates storage in the process.
  textureImage :: (TextureData a d) => TextureTarget -> MipmapLevel -> TextureInternalFormat -> SettableStateVar a
  -- | Initializes a multisample texture.
  textureImageMultisample :: (MonadBase IO m) => TextureTarget -> MipmapLevel -> TextureInternalFormat -> d GLsizei -> Bool -> m ()
  -- | Configures texture wrapping.
  textureWrap :: TextureTarget -> StateVar (d TextureWrap)
  -- | Only allocates storage for the texture.
  textureStorage :: (MonadIO m) => TextureTarget -> Int -> TextureInternalFormat -> d GLsizei -> m ()
  -- | Only allocates storage for a multisample texture.
  textureStorageMultisample :: (MonadBase IO m) => TextureTarget -> Int -> TextureInternalFormat -> d GLsizei -> Bool -> m ()
  -- | Accesses a subimage of a texture beginning at the given offset.
  textureSubImage :: (TextureData a d) => TextureTarget -> MipmapLevel -> d GLint -> SettableStateVar a
  -- | Clears a part of the texture
  textureClearSubImage :: (MonadBase IO m) => Texture -> MipmapLevel -> d GLint -> d GLsizei -> Color -> m ()

-- | Clears the texture using the given color.
textureClearImage :: Texture -> Int -> Color -> IO ()
textureClearImage tex level col = withPtrIn col $ \ptr ->
  TexClear.glClearTexImage (objectId tex) (fromIntegral level) GL_RGBA GL_FLOAT (castPtr ptr)

textureWrap' :: GLenum -> TextureTarget -> StateVar TextureWrap
textureWrap' idx (TextureTarget _ target) = makeStateVar g s where
    g = fromIntegral <$> withPtrOut (glGetTexParameteriv target idx)
    s = glTexParameteri target idx . fromIntegral

instance TextureAccess V1 where
  textureImage (TextureTarget _ target) lvl innerFmt = makeSettableStateVar $ flip withRawTexture
    $ \fmt dataTy (V1 w) dat ->
        glTexImage1D target (fromIntegral lvl) innerFmt w 0 fmt dataTy dat

  textureImageMultisample = error "multisampling not supported for 1D textures"

  textureWrap = mapStateVar (view _x) V1 . textureWrap' GL_TEXTURE_WRAP_S

  textureStorage (TextureTarget _ target) numLevels innerFmt (V1 w) =
    TexStore.glTexStorage1D target (fromIntegral numLevels) (fromIntegral innerFmt) w

  textureStorageMultisample = error "multisampling not supported for 1D textures"

  textureSubImage (TextureTarget _ target) lvl (V1 x) = makeSettableStateVar $ flip withRawTexture
    $ \fmt dataTy (V1 w) dat ->
        glTexSubImage1D target (fromIntegral lvl) x w fmt dataTy dat

  textureClearSubImage tex lvl (V1 x) (V1 w) col = liftBase $ withPtrIn col $ \ptr ->
    TexClear.glClearTexSubImage (objectId tex) (fromIntegral lvl) x 0 0 w 1 1 GL_RGBA GL_FLOAT (castPtr ptr)

instance TextureAccess V2 where
  textureImage (TextureTarget _ target) lvl innerFmt = makeSettableStateVar $ flip withRawTexture
   $ \fmt dataTy (V2 w h) dat ->
       glTexImage2D target (fromIntegral lvl) innerFmt w h 0 fmt dataTy dat

  textureImageMultisample (TextureTarget _ target) samples innerFmt (V2 w h) fixed = liftBase $
    glTexImage2DMultisample target (fromIntegral samples) (fromIntegral innerFmt) w h (toGLBool fixed)

  textureWrap target = combineStateVars (uncurry V2) (\(V2 x y) -> (x,y))
                                        (textureWrap' GL_TEXTURE_WRAP_S target)
                                        (textureWrap' GL_TEXTURE_WRAP_T target)

  textureStorage (TextureTarget _ target) numLevels innerFmt (V2 w h) =
    TexStore.glTexStorage2D target (fromIntegral numLevels) (fromIntegral innerFmt) w h

  textureStorageMultisample (TextureTarget _ target) samples innerFmt (V2 w h) fixed = liftBase $
    TexStore.glTexStorage2DMultisample target (fromIntegral samples) (fromIntegral innerFmt) w h (toGLBool fixed)

  textureSubImage (TextureTarget _ target) lvl (V2 x y) = makeSettableStateVar $ flip withRawTexture
    $ \fmt dataTy (V2 w h) dat ->
        glTexSubImage2D target (fromIntegral lvl) x y w h fmt dataTy dat

  textureClearSubImage tex lvl (V2 x y) (V2 w h) col = liftBase $ withPtrIn col $ \ptr ->
    TexClear.glClearTexSubImage (objectId tex) (fromIntegral lvl) x y 0 w h 1 GL_RGBA GL_FLOAT (castPtr ptr)

instance TextureAccess V3 where
  textureImage (TextureTarget _ target) lvl innerFmt = makeSettableStateVar $ flip withRawTexture
   $ \fmt dataTy (V3 w h d) dat ->
       glTexImage3D target (fromIntegral lvl) innerFmt w h d 0 fmt dataTy dat

  textureImageMultisample (TextureTarget _ target) samples innerFmt (V3 w h d) fixed = liftBase $
    glTexImage3DMultisample target (fromIntegral samples) (fromIntegral innerFmt) w h d (toGLBool fixed)

  textureWrap target = combineStateVars (\(V2 s t, r) -> V3 s t r) (\(V3 s t r) -> (V2 s t, r))
                                        (textureWrap target)
                                        (textureWrap' GL_TEXTURE_WRAP_R target)

  textureStorage (TextureTarget _ target) numLevels innerFmt (V3 w h d) =
    TexStore.glTexStorage3D target (fromIntegral numLevels) (fromIntegral innerFmt) w h d

  textureStorageMultisample (TextureTarget _ target) samples innerFmt (V3 w h d) fixed = liftBase $
    TexStore.glTexStorage3DMultisample target (fromIntegral samples) (fromIntegral innerFmt) w h d (toGLBool fixed)

  textureSubImage (TextureTarget _ target) lvl (V3 x y z) = makeSettableStateVar $ flip withRawTexture
    $ \fmt dataTy (V3 w h d) dat ->
        glTexSubImage3D target (fromIntegral lvl) x y z w h d fmt dataTy dat

  textureClearSubImage tex lvl (V3 x y z) (V3 w h d) col = liftBase $ withPtrIn col $ \ptr ->
    TexClear.glClearTexSubImage (objectId tex) (fromIntegral lvl) x y z w h d GL_RGBA GL_FLOAT (castPtr ptr)

-- * Texture Parameters

type TextureWrap = GLenum

-- | Controls the border color used for 'GL_CLAMP_TO_BORDER'.
textureBorderColor :: TextureTarget -> StateVar Color
textureBorderColor (TextureTarget _ target) = makeStateVar g s where
  g = withPtrOut (glGetTexParameterfv target GL_TEXTURE_BORDER_COLOR . castPtr)
  s col = withPtrIn col $ glTexParameterfv target GL_TEXTURE_BORDER_COLOR . castPtr

-- * Texture Filtering

type TextureFilter = GLenum

-- | Controls the 'GL_TEXTURE_MIN_FILTER'.
textureMinFilter :: TextureTarget -> StateVar TextureFilter
textureMinFilter (TextureTarget _ target) = makeStateVar g s where
  g = fromIntegral <$> withPtrOut (glGetTexParameteriv target GL_TEXTURE_MIN_FILTER)
  s f = glTexParameteri target GL_TEXTURE_MIN_FILTER (fromIntegral f)

-- | Controls the 'GL_TEXTURE_MAG_FILTER'.
textureMagFilter :: TextureTarget -> StateVar TextureFilter
textureMagFilter (TextureTarget _ target) = makeStateVar g s where
  g = fromIntegral <$> withPtrOut (glGetTexParameteriv target GL_TEXTURE_MAG_FILTER)
  s f = glTexParameteri target GL_TEXTURE_MAG_FILTER (fromIntegral f)

-- | Generates the mip maps.
generateMipMap :: MonadIO m => TextureTarget -> m ()
generateMipMap (TextureTarget _ target) = glGenerateMipmap target

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
