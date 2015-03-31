{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
module Ninja.GL.Shader where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.StateVar
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Ninja.GL.Object

-- | Shader stages available in OpenGL 3.x according to <https://www.opengl.org/wiki/Shader>
data ShaderType
  = FragmentShader
  | VertexShader
  | GeometryShader
  | Unknown
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Shader object
--newtype Shader t = Shader GLuint deriving (Eq, Ord, Show)

newtype Shader (t :: ShaderType) = Shader GLuint deriving (Eq, Ord, Show)

instance GenObject (Shader FragmentShader) where
  gen1 = Shader `liftM` glCreateShader GL_FRAGMENT_SHADER

instance GenObject (Shader VertexShader) where
  gen1 = Shader `liftM` glCreateShader GL_VERTEX_SHADER

instance GenObject (Shader GeometryShader) where
  gen1 = Shader `liftM` glCreateShader GL_GEOMETRY_SHADER

-- | Create a shader with a type not contained in the 'ShaderType' enum.
createShader :: GLenum -> IO (Shader Unknown)
createShader ty = liftM Shader $ glCreateShader ty

instance Object (Shader t) where
  objectId = coerce
  delete1  = glDeleteShader . objectId

-- | Tries to compile a shader using 'glCompileShader'. Returns a boolean indicating success or failure and the shader log.
compileShader :: Shader t -> IO (Bool, String)
compileShader shader = do
  glCompileShader (objectId shader)
  success <- (GL_FALSE /=) <$> alloca (\s -> glGetShaderiv (objectId shader) GL_COMPILE_STATUS s >> peek s)
  logsize <- alloca $ \p -> glGetShaderiv (objectId shader) GL_INFO_LOG_LENGTH p >> peek p
  logstr <- allocaBytes (fromIntegral logsize) $ \cstr -> do
              glGetShaderInfoLog (objectId shader) logsize nullPtr cstr
              peekCString cstr
  return (success, logstr)

-- | Returns the type of a shader.
shaderType :: Shader t -> GettableStateVar ShaderType
shaderType shader = makeGettableStateVar $
  alloca (\p -> glGetShaderiv (objectId shader) GL_SHADER_TYPE p >> peek p) >>= \case
    GL_VERTEX_SHADER -> return VertexShader
    GL_FRAGMENT_SHADER -> return FragmentShader
    GL_GEOMETRY_SHADER -> return GeometryShader
    o                  -> return Unknown

-- | Gets or sets the source code of a shader.
shaderSource :: Shader t -> StateVar String
shaderSource shader = makeStateVar g s where
  g = do
    len <- alloca $ \p -> do
        glGetShaderiv (objectId shader) GL_SHADER_SOURCE_LENGTH p
        peek p
    allocaBytes (fromIntegral len) $ \cstr -> do
      glGetShaderSource (objectId shader) len nullPtr cstr
      peekCString cstr
  s str = withCString str $ \cstr ->
    alloca $ \p -> do
        poke p cstr
        glShaderSource (objectId shader) 1 p nullPtr

-- | Gets or sets the source code of a shader as a 'ByteString'.
shaderSourceBytes :: Shader t -> StateVar BS.ByteString
shaderSourceBytes shader = makeStateVar g s where
  g = do
    len <- alloca $ \p -> do
        glGetShaderiv (objectId shader) GL_SHADER_SOURCE_LENGTH p
        peek p
    allocaBytes (fromIntegral len) $ \cstr -> do
      glGetShaderSource (objectId shader) len nullPtr cstr
      BS.packCString cstr
  s str = BSU.unsafeUseAsCString str $ \cstr ->
    alloca $ \p -> do
        poke p cstr
        glShaderSource (objectId shader) 1 p nullPtr

-- | Casts the shader to a fragment shader, if it is one.
asFragmentShader :: Shader t -> IO (Maybe (Shader FragmentShader))
asFragmentShader = castShader FragmentShader

-- | Casts the shader to a vertex shader, if it is one.
asVertexShader :: Shader t -> IO (Maybe (Shader VertexShader))
asVertexShader = castShader VertexShader

-- | Casts the shader to a geometry shader, if it is one.
asGeometryShader :: Shader t -> IO (Maybe (Shader GeometryShader))
asGeometryShader = castShader GeometryShader

-- | Forgets the type information about the shader.
asUnknownShader :: Shader t -> Shader Unknown
asUnknownShader = coerce

-- | Tries to cast a shader to the desired type.
castShader :: ShaderType -> Shader t -> IO (Maybe (Shader t'))
castShader targetType shader = do
  sourceType <- get $ shaderType shader
  if targetType == Unknown || sourceType == targetType
    then return $ Just $ coerce shader
    else return Nothing