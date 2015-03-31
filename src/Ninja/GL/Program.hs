{-# LANGUAGE DataKinds #-}
module Ninja.GL.Program where

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

import           Ninja.GL.Object
import           Ninja.GL.Shader

-- | Shader Program object handle
newtype Program = Program GLuint deriving (Eq, Ord, Show)

instance Object Program where
  objectId = coerce
  delete1 = glDeleteProgram . objectId

instance GenObject Program where
  gen1 = liftM Program glCreateProgram

instance Default Program where
  def = Program 0

-- | Attaches a shader to a program.
attachShader :: Program -> Shader t -> IO ()
attachShader prog shader = glAttachShader (objectId prog) (objectId shader)

-- | Detaches a shader from a program.
detachShader :: Program -> Shader t -> IO ()
detachShader prog shader = glDetachShader (objectId prog) (objectId shader)

-- | Gets or sets the shaders attached to the program.
attachedShaders :: Program -> StateVar [Shader Unknown]
attachedShaders prog = makeStateVar g s where
  g = do
    num <- alloca $ \p -> glGetProgramiv (objectId prog) GL_ATTACHED_SHADERS p >> peek p
    allocaArray (fromIntegral num) $ \arr -> do
      glGetAttachedShaders (objectId prog) num nullPtr arr
      coerce <$> peekArray (fromIntegral num) arr
  s xs = do
    g >>= mapM_ (detachShader prog)
    mapM_ (attachShader prog) xs

-- | Links a shader program, returning a boolean indicating success or failure and the program log.
linkProgram :: Program -> IO (Bool, String)
linkProgram prog = do
  glLinkProgram (objectId prog)
  success <- (GL_FALSE /=) <$> alloca (\s -> glGetProgramiv (objectId prog) GL_LINK_STATUS s >> peek s)
  logsize <- alloca $ \p -> glGetProgramiv (objectId prog) GL_INFO_LOG_LENGTH p >> peek p
  logstr <- allocaBytes (fromIntegral logsize) $ \cstr -> do
              glGetProgramInfoLog (objectId prog) logsize nullPtr cstr
              peekCString cstr
  return (success, logstr)

-- | The currently used shader program (see 'glUseProgram').
usedProgram :: StateVar Program
usedProgram = makeStateVar g s where
  g = Program . fromIntegral <$> alloca (\p -> glGetIntegerv GL_CURRENT_PROGRAM p >> peek p)
  s = glUseProgram . objectId

-- | Sets the shader program for the duration of the supplied action and restores it afterwards.
withProgram :: Program -> IO a -> IO a
withProgram prog action = do
  oldProg <- get usedProgram
  usedProgram $= prog
  finally action
    $ usedProgram $= oldProg
