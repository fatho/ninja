{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
module Ninja.GL.Shader where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.StateVar
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Graphics.GL.Core33
import           Graphics.GL.Types

import           Ninja.GL.Exception
import           Ninja.GL.Object
import           Ninja.Util

-- | Shader object
newtype Shader = Shader GLuint deriving (Eq, Ord, Show)

type ShaderType = GLenum

-- | Create a shader of the given type.
createShader :: ShaderType -> IO Shader
createShader ty = do
  shId <- glCreateShader ty
  when (shId == 0) $ throw $ ObjectCreationFailed "Shader"
  return $ Shader shId

instance Object Shader where
  objectId = coerce
  delete1  = glDeleteShader . objectId
  isA = liftM (/= GL_FALSE) . glIsShader . objectId

-- | Tries to compile a shader using 'glCompileShader'.
compileShader :: Shader -> IO ()
compileShader shader = do
  glCompileShader (objectId shader)
  success <- (GL_FALSE /=) <$> withPtrOut (glGetShaderiv (objectId shader) GL_COMPILE_STATUS)
  logsize <- withPtrOut $ glGetShaderiv (objectId shader) GL_INFO_LOG_LENGTH
  logstr <- allocaBytes (fromIntegral logsize) $ \cstr -> do
              glGetShaderInfoLog (objectId shader) logsize nullPtr cstr
              peekCString cstr
  unless success $ throw $ ShaderCompileError logstr

-- | Returns the type of a shader.
shaderType :: Shader -> GettableStateVar ShaderType
shaderType shader = makeGettableStateVar $ 
  fromIntegral <$> withPtrOut (glGetShaderiv (objectId shader) GL_SHADER_TYPE)

class ShaderSource a where
  withSourcePtr :: a -> (CStringLen -> IO b) -> IO b
  fromSourcePtr :: CStringLen -> IO a

instance ShaderSource String where
  withSourcePtr = withCStringLen
  fromSourcePtr = peekCStringLen

instance ShaderSource BS.ByteString where
  withSourcePtr = BSU.unsafeUseAsCStringLen
  fromSourcePtr = BS.packCStringLen

-- | Gets or sets the source code of a shader.
shaderSource :: ShaderSource a => Shader -> StateVar a
shaderSource shader = makeStateVar g s where
  g = do
    len <- withPtrOut $ glGetShaderiv (objectId shader) GL_SHADER_SOURCE_LENGTH
    allocaBytes (fromIntegral len) $ \cstr -> do
      actualLen <- withPtrOut $ \plenOut -> glGetShaderSource (objectId shader) len plenOut cstr
      fromSourcePtr (cstr, fromIntegral actualLen)
  s src = withSourcePtr src $ \(cstr,len) ->
    withPtrIn cstr $ \psrc -> 
    withPtrIn (fromIntegral len) $ \plen -> glShaderSource (objectId shader) 1 psrc plen

-- | Creates a new shader object, loads the source code and compiles.
-- Throws an exception on failure.
createShaderFromSource :: (MonadResource m, ShaderSource a) => ShaderType -> a -> m (ReleaseKey,Shader)
createShaderFromSource shtype src = do
  (rkey, shd) <- allocate (createShader shtype) delete1
  liftIO $ do
    shaderSource shd $= src
    compileShader shd
  return (rkey,shd)
