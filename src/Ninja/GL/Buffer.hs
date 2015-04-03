{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ninja.GL.Buffer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import qualified Data.Vector.Storable   as VS
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Ninja.GL.Object
import           Ninja.Util

-- | Encapsulates an OpenGL buffer target.
data BufferTarget = BufferTarget GLenum GLenum deriving (Eq, Ord, Show)

-- | Buffer Handle.
newtype Buffer a = Buffer GLuint deriving (Eq, Ord, Show)

-- | Buffer usage.
newtype BufferUsage = BufferUsage GLenum deriving (Eq, Ord, Show)

instance Object (Buffer a) where
  objectId = coerce
  delete xs = liftIO $ withArrayLen (coerce xs) $ \n ps -> glDeleteBuffers (fromIntegral n) ps
  isA = liftM (/= GL_FALSE) . glIsBuffer . objectId

instance GenObject (Buffer a) where
  gen n = liftIO $ allocaArray n $ \ps -> do
    glGenBuffers (fromIntegral n) ps
    coerce <$> peekArray n ps

instance Default (Buffer a) where
  def = Buffer 0

-- | Interface to buffer data. Must be able to provide a size and a pointer to the data.
class BufferData a where
  -- | Call the argument function with size and pointer to raw data.
  withRawData :: MonadIO m => a -> (GLsizeiptr -> Ptr () -> IO ()) -> m ()
  -- | Provide a buffer of the given size for the data to be read into.
  fromRawData :: MonadIO m => GLsizeiptr -> (Ptr () -> IO ()) -> m a

-- | Buffer data with a size but without actual data.
data NullData = NullData GLsizeiptr

instance BufferData NullData where
  withRawData (NullData size) f = liftIO $ f size nullPtr
  fromRawData size f = liftIO (f nullPtr) >> return (NullData size)

instance Storable a => BufferData (VS.Vector a) where
  withRawData vs f = liftIO $ VS.unsafeWith vs $ \p -> f (fromIntegral $ VS.length vs * sizeOf (undefined :: a)) (castPtr p)
  fromRawData size f = liftIO $ do
    buf <- mallocForeignPtrBytes (fromIntegral size)
    withForeignPtr buf (f . castPtr)
    return $ VS.unsafeFromForeignPtr0 buf (fromIntegral size `div` sizeOf (undefined :: a))

instance Storable a => BufferData [a] where
  withRawData vs f = liftIO $ allocaArray l $ \p -> pokeArray p vs >> f (fromIntegral $ l * sizeOf (undefined :: a)) (castPtr p)
    where l = length vs
  fromRawData size f = liftIO $ allocaArray l $ \p -> f (castPtr p) >> peekArray l p
    where l = fromIntegral size `div` sizeOf (undefined :: a)

-- | Streams data from or to the buffer, see 'glBufferData'.
bufferData :: (BufferData a) => BufferTarget -> SettableStateVar (BufferUsage, a)
bufferData (BufferTarget _ t) = makeSettableStateVar $ \(BufferUsage usage, dat) ->
  withRawData dat $ \s p -> glBufferData t s p usage

-- | Modifies data in a buffer.
bufferSubData :: BufferData a => BufferTarget -> GLintptr -> GLsizeiptr -> StateVar a
bufferSubData (BufferTarget _ t) off size = makeStateVar g s where
  g = fromRawData size $ \p -> glGetBufferSubData t off size p
  s dat = withRawData dat $ \size' p -> glBufferSubData t off (min size size') p

-- | Returns the size of a buffer.
bufferSize :: BufferTarget -> GettableStateVar GLsizeiptr
bufferSize (BufferTarget _ t) = makeGettableStateVar $ 
  fromIntegral <$> withPtrOut (glGetBufferParameteriv t GL_BUFFER_SIZE)

-- | Returns the size of a buffer.
bufferUsage :: BufferTarget -> GettableStateVar BufferUsage
bufferUsage (BufferTarget _ t) = makeGettableStateVar $
  BufferUsage . fromIntegral <$> withPtrOut (glGetBufferParameteriv t GL_BUFFER_USAGE)

-- | The currently bound vertex array. It is set using 'glBindBuffer' and get using 'glGetIntegerv'.
boundBuffer :: BufferTarget -> StateVar (Buffer a)
boundBuffer (BufferTarget ivar buftype) = makeStateVar getva setva where
  getva = Buffer . fromIntegral <$> alloca (\p -> glGetIntegerv ivar p >> peek p)
  setva = glBindBuffer buftype . coerce


pattern ArrayBuffer = BufferTarget GL_ARRAY_BUFFER_BINDING GL_ARRAY_BUFFER
pattern ElementArrayBuffer = BufferTarget GL_ELEMENT_ARRAY_BUFFER_BINDING GL_ELEMENT_ARRAY_BUFFER
pattern TextureBuffer = BufferTarget GL_TEXTURE_BINDING_BUFFER GL_TEXTURE_BUFFER
pattern UniformBuffer = BufferTarget GL_UNIFORM_BUFFER_BINDING GL_UNIFORM_BUFFER
-- TODO: Other Buffer Targets

pattern StreamRead = BufferUsage GL_STREAM_READ
pattern StreamDraw = BufferUsage GL_STREAM_DRAW
pattern StreamCopy = BufferUsage GL_STREAM_COPY

pattern DynamicRead = BufferUsage GL_DYNAMIC_READ
pattern DynamicDraw = BufferUsage GL_DYNAMIC_DRAW
pattern DynamicCopy = BufferUsage GL_DYNAMIC_COPY

pattern StaticRead = BufferUsage GL_STATIC_READ
pattern StaticDraw = BufferUsage GL_STATIC_DRAW
pattern StaticCopy = BufferUsage GL_STATIC_COPY
