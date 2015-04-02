{-# LANGUAGE DataKinds #-}
module Ninja.GL.Program where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
import           Data.Maybe
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

import           Ninja.GL.Exception
import           Ninja.GL.Object
import           Ninja.GL.Shader
import           Ninja.Util

-- | Shader Program object handle
newtype Program = Program GLuint deriving (Eq, Ord, Show)

instance Object Program where
  objectId = coerce
  delete1 = glDeleteProgram . objectId
  isA = liftM (/= GL_FALSE) . glIsProgram . objectId

instance GenObject Program where
  gen1 = liftM Program glCreateProgram

instance Default Program where
  def = Program 0

-- | Attaches a shader to a program.
attachShader :: Program -> Shader -> IO ()
attachShader prog shader = glAttachShader (objectId prog) (objectId shader)

-- | Detaches a shader from a program.
detachShader :: Program -> Shader -> IO ()
detachShader prog shader = glDetachShader (objectId prog) (objectId shader)

-- | Gets or sets the shaders attached to the program.
attachedShaders :: Program -> StateVar [Shader]
attachedShaders prog = makeStateVar g s where
  g = do
    num <- alloca $ \p -> glGetProgramiv (objectId prog) GL_ATTACHED_SHADERS p >> peek p
    allocaArray (fromIntegral num) $ \arr -> do
      glGetAttachedShaders (objectId prog) num nullPtr arr
      coerce <$> peekArray (fromIntegral num) arr
  s xs = do
    g >>= mapM_ (detachShader prog)
    mapM_ (attachShader prog) xs

-- | Links a shader program.
linkProgram :: Program -> IO ()
linkProgram prog = do
  glLinkProgram (objectId prog)
  success <- (GL_FALSE /=) <$> withPtrOut (glGetProgramiv (objectId prog) GL_LINK_STATUS)
  logsize <- withPtrOut $ glGetProgramiv (objectId prog) GL_INFO_LOG_LENGTH
  logstr <- allocaBytes (fromIntegral logsize) $ \cstr -> do
              glGetProgramInfoLog (objectId prog) logsize nullPtr cstr
              peekCString cstr
  unless success $ throw $ ShaderCompileError logstr

-- | The currently used shader program (see 'glUseProgram').
usedProgram :: StateVar Program
usedProgram = makeStateVar g s where
  g = Program . fromIntegral <$> withPtrOut (glGetIntegerv GL_CURRENT_PROGRAM)
  s = glUseProgram . objectId

-- | Sets the shader program for the duration of the supplied action and restores it afterwards.
withProgram :: Program -> IO a -> IO a
withProgram = withVar usedProgram

-- | Creates a program from a list of already compiled shaders.
createProgramFromShaders :: [Shader] -> IO Program
createProgramFromShaders shaders = do
  prog <- gen1
  withVar (attachedShaders prog) shaders (linkProgram prog)
    `onException` delete1 prog
  return prog

-- | Creates a shader program from source code.
createProgramFromSource :: (ShaderSource a)
  => [a] -- ^ vertex shaders
  -> [a] -- ^ geometry shaders
  -> [a] -- ^ fragment shaders
  -> IO Program
createProgramFromSource vertSource geomSource fragSource = runResourceT $ do
  vs <- mapM (liftM snd . createShaderFromSource GL_VERTEX_SHADER) vertSource
  gs <- mapM (liftM snd . createShaderFromSource GL_GEOMETRY_SHADER) geomSource
  fs <- mapM (liftM snd . createShaderFromSource GL_FRAGMENT_SHADER) fragSource
  liftIO $ createProgramFromShaders (vs ++ gs ++ fs)
