{-# LANGUAGE FlexibleInstances #-}
module Ninja.GL.Uniform where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import Control.Lens hiding (coerce)
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
import           Ninja.GL.Program

-- | A uniform is just something that can be set.
type Uniform a = SettableStateVar a

-- | Typed handle to a uniform variable.
newtype UniformLoc a = UniformLoc GLint deriving (Eq, Ord, Show)

-- | Returns the location of a uniform value of a shader program (see 'glGetUniformLocation').
uniformLocation :: Program -> String -> IO (UniformLoc a)
uniformLocation prog name = do
  locId <- withCString name (glGetUniformLocation (objectId prog))
  when (locId < 0) $ ioError $ userError $ "uniform not found: " ++ name
  return $ UniformLoc locId

-- | Returns a uniform of the given program.
uniformOf :: Uniformable a => Program -> String -> IO (Uniform a)
uniformOf prog name = uniform <$> uniformLocation prog name

-- | Class of uniform values.
class Uniformable a where
  -- | Uploads a uniform value to the driver.
  uniform :: UniformLoc a -> SettableStateVar a

-- | Generic function to upload a uniform vector.
uniformVector :: Storable a => (GLint -> GLsizei -> Ptr b -> IO ()) -> UniformLoc (VS.Vector a) -> SettableStateVar (VS.Vector a)
uniformVector loadVec loc = makeSettableStateVar $ \vs ->
  VS.unsafeWith vs $ \arr -> loadVec (coerce loc) (fromIntegral $ VS.length vs) (castPtr arr)
{-# INLINE uniformVector #-}

-- | Generic function to upload a uniform vector given by a list.
uniformList :: Storable a => (GLint -> GLsizei -> Ptr b -> IO ()) -> UniformLoc [a] -> SettableStateVar [a]
uniformList loadVec loc = makeSettableStateVar $ \vs ->
  withArrayLen vs $ \len arr -> loadVec (coerce loc) (fromIntegral len) (castPtr arr)
{-# INLINE uniformList #-}

-- * Float based uniform vectors

instance Uniformable Float where
  uniform loc = makeSettableStateVar $ glUniform1f (coerce loc)

instance Uniformable [Float] where
  uniform = uniformList glUniform1fv

instance Uniformable (VS.Vector Float) where
  uniform = uniformVector glUniform1fv


instance Uniformable (V1 Float) where
  uniform loc = makeSettableStateVar $ \(V1 x) -> glUniform1f (coerce loc) x

instance Uniformable [V1 Float] where
  uniform = uniformList glUniform1fv

instance Uniformable (VS.Vector (V1 Float)) where
  uniform = uniformVector glUniform1fv


instance Uniformable (V2 Float) where
  uniform loc = makeSettableStateVar $ \(V2 x y) -> glUniform2f (coerce loc) x y

instance Uniformable [V2 Float] where
  uniform = uniformList glUniform2fv

instance Uniformable (VS.Vector (V2 Float)) where
  uniform = uniformVector glUniform2fv


instance Uniformable (V3 Float) where
  uniform loc = makeSettableStateVar $ \(V3 x y z) -> glUniform3f (coerce loc) x y z

instance Uniformable [V3 Float] where
  uniform = uniformList glUniform3fv

instance Uniformable (VS.Vector (V3 Float)) where
  uniform = uniformVector glUniform3fv


instance Uniformable (V4 Float) where
  uniform loc = makeSettableStateVar $ \(V4 x y z w) -> glUniform4f (coerce loc) x y z w

instance Uniformable [V4 Float] where
  uniform = uniformList glUniform4fv

instance Uniformable (VS.Vector (V4 Float)) where
  uniform = uniformVector glUniform4fv


-- * GLint based uniform vectors

instance Uniformable GLint where
  uniform loc = makeSettableStateVar $ glUniform1i (coerce loc)

instance Uniformable [GLint] where
  uniform = uniformList glUniform1iv

instance Uniformable (VS.Vector GLint) where
  uniform = uniformVector glUniform1iv


instance Uniformable (V1 GLint) where
  uniform loc  = makeSettableStateVar $ \(V1 x) -> glUniform1i (coerce loc) x

instance Uniformable [V1 GLint] where
  uniform = uniformList glUniform1iv

instance Uniformable (VS.Vector (V1 GLint)) where
  uniform = uniformVector glUniform1iv


instance Uniformable (V2 GLint) where
  uniform loc = makeSettableStateVar $ \(V2 x y) -> glUniform2i (coerce loc) x y

instance Uniformable [V2 GLint] where
  uniform = uniformList glUniform2iv

instance Uniformable (VS.Vector (V2 GLint)) where
  uniform = uniformVector glUniform2iv


instance Uniformable (V3 GLint) where
  uniform loc = makeSettableStateVar $ \(V3 x y z) -> glUniform3i (coerce loc) x y z

instance Uniformable [V3 GLint] where
  uniform = uniformList glUniform3iv

instance Uniformable (VS.Vector (V3 GLint)) where
  uniform = uniformVector glUniform3iv


instance Uniformable (V4 GLint) where
  uniform loc = makeSettableStateVar $ \(V4 x y z w) -> glUniform4i (coerce loc) x y z w

instance Uniformable [V4 GLint] where
  uniform = uniformList glUniform4iv

instance Uniformable (VS.Vector (V4 GLint)) where
  uniform = uniformVector glUniform4iv


-- * GLuint based uniform vectors

instance Uniformable GLuint where
  uniform loc = makeSettableStateVar $ glUniform1ui (coerce loc)

instance Uniformable [GLuint] where
  uniform = uniformList glUniform1uiv

instance Uniformable (VS.Vector GLuint) where
  uniform = uniformVector glUniform1uiv


instance Uniformable (V1 GLuint) where
  uniform loc = makeSettableStateVar $ \(V1 x) -> glUniform1ui (coerce loc) x

instance Uniformable [V1 GLuint] where
  uniform = uniformList glUniform1uiv

instance Uniformable (VS.Vector (V1 GLuint)) where
  uniform = uniformVector glUniform1uiv


instance Uniformable (V2 GLuint) where
  uniform loc = makeSettableStateVar $ \(V2 x y) -> glUniform2ui (coerce loc) x y

instance Uniformable [V2 GLuint] where
  uniform = uniformList glUniform2uiv

instance Uniformable (VS.Vector (V2 GLuint)) where
  uniform = uniformVector glUniform2uiv


instance Uniformable (V3 GLuint) where
  uniform loc = makeSettableStateVar $ \(V3 x y z) -> glUniform3ui (coerce loc) x y z

instance Uniformable [V3 GLuint] where
  uniform = uniformList glUniform3uiv

instance Uniformable (VS.Vector (V3 GLuint)) where
  uniform = uniformVector glUniform3uiv


instance Uniformable (V4 GLuint) where
  uniform loc = makeSettableStateVar $ \(V4 x y z w) -> glUniform4ui (coerce loc) x y z w

instance Uniformable [V4 GLuint] where
  uniform = uniformList glUniform4uiv

instance Uniformable (VS.Vector (V4 GLuint)) where
  uniform = uniformVector glUniform4uiv


-- * Matrix based uniforms

-- | Generic function to upload uniform matrix.
uniformMatrix :: Storable a => (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> UniformLoc a -> SettableStateVar a
uniformMatrix loadMat loc = makeSettableStateVar $ \mat -> alloca $ \p -> poke p mat
    >> loadMat (coerce loc) 1 GL_FALSE (castPtr p)
{-# INLINE uniformMatrix #-}

-- | Generic function to upload uniform matrix.
uniformMatrixVector :: Storable a => (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
  -> UniformLoc (VS.Vector a) -> SettableStateVar (VS.Vector a)
uniformMatrixVector loadMat loc = makeSettableStateVar $ \mats -> VS.unsafeWith mats $ \arr ->
    loadMat (coerce loc) (fromIntegral $ VS.length mats) GL_FALSE (castPtr arr)

instance Uniformable (M22 Float) where
  uniform = uniformMatrix glUniformMatrix2fv

instance Uniformable (M33 Float) where
  uniform = uniformMatrix glUniformMatrix3fv

instance Uniformable (M44 Float) where
  uniform = uniformMatrix glUniformMatrix4fv

instance Uniformable (M23 Float) where
  uniform = uniformMatrix glUniformMatrix2x3fv

instance Uniformable (M32 Float) where
  uniform = uniformMatrix glUniformMatrix3x2fv

instance Uniformable (M24 Float) where
  uniform = uniformMatrix glUniformMatrix2x4fv

instance Uniformable (M42 Float) where
  uniform = uniformMatrix glUniformMatrix4x2fv

instance Uniformable (M34 Float) where
  uniform = uniformMatrix glUniformMatrix3x4fv

instance Uniformable (M43 Float) where
  uniform = uniformMatrix glUniformMatrix4x3fv