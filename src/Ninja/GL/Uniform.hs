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

-- | Typed handle to a uniform variable.
newtype UniformLoc a = UniformLoc GLint deriving (Eq, Ord, Show)

-- | Returns the location of a uniform value of a shader program (see 'glGetUniformLocation').
uniformLocation :: Program -> String -> IO (UniformLoc a)
uniformLocation prog name = UniformLoc <$> withCString name (glGetUniformLocation (objectId prog))

-- | Class of uniform values.
class Uniform a where
  -- | Uploads a uniform value to the driver.
  uniform :: UniformLoc a -> a -> IO ()

-- | Generic function to upload a uniform vector.
uniformVector :: Storable a => (GLint -> GLsizei -> Ptr b -> IO ()) -> UniformLoc (VS.Vector a) -> VS.Vector a -> IO ()
uniformVector loadVec loc vs = VS.unsafeWith vs $ \arr -> loadVec (coerce loc) (fromIntegral $ VS.length vs) (castPtr arr)
{-# INLINE uniformVector #-}

-- | Generic function to upload a uniform vector given by a list.
uniformList :: Storable a => (GLint -> GLsizei -> Ptr b -> IO ()) -> UniformLoc [a] -> [a] -> IO ()
uniformList loadVec loc vs = withArrayLen vs $ \len arr -> loadVec (coerce loc) (fromIntegral len) (castPtr arr)
{-# INLINE uniformList #-}

-- * Float based uniform vectors

instance Uniform Float where
  uniform loc = glUniform1f (coerce loc)

instance Uniform [Float] where
  uniform = uniformList glUniform1fv

instance Uniform (VS.Vector Float) where
  uniform = uniformVector glUniform1fv


instance Uniform (V1 Float) where
  uniform loc (V1 x) = glUniform1f (coerce loc) x

instance Uniform [V1 Float] where
  uniform = uniformList glUniform1fv

instance Uniform (VS.Vector (V1 Float)) where
  uniform = uniformVector glUniform1fv


instance Uniform (V2 Float) where
  uniform loc (V2 x y) = glUniform2f (coerce loc) x y

instance Uniform [V2 Float] where
  uniform = uniformList glUniform2fv

instance Uniform (VS.Vector (V2 Float)) where
  uniform = uniformVector glUniform2fv


instance Uniform (V3 Float) where
  uniform loc (V3 x y z) = glUniform3f (coerce loc) x y z

instance Uniform [V3 Float] where
  uniform = uniformList glUniform3fv

instance Uniform (VS.Vector (V3 Float)) where
  uniform = uniformVector glUniform3fv


instance Uniform (V4 Float) where
  uniform loc (V4 x y z w) = glUniform4f (coerce loc) x y z w

instance Uniform [V4 Float] where
  uniform = uniformList glUniform4fv

instance Uniform (VS.Vector (V4 Float)) where
  uniform = uniformVector glUniform4fv


-- * GLint based uniform vectors

instance Uniform GLint where
  uniform loc = glUniform1i (coerce loc)

instance Uniform [GLint] where
  uniform = uniformList glUniform1iv

instance Uniform (VS.Vector GLint) where
  uniform = uniformVector glUniform1iv


instance Uniform (V1 GLint) where
  uniform loc (V1 x) = glUniform1i (coerce loc) x

instance Uniform [V1 GLint] where
  uniform = uniformList glUniform1iv

instance Uniform (VS.Vector (V1 GLint)) where
  uniform = uniformVector glUniform1iv


instance Uniform (V2 GLint) where
  uniform loc (V2 x y) = glUniform2i (coerce loc) x y

instance Uniform [V2 GLint] where
  uniform = uniformList glUniform2iv

instance Uniform (VS.Vector (V2 GLint)) where
  uniform = uniformVector glUniform2iv


instance Uniform (V3 GLint) where
  uniform loc (V3 x y z) = glUniform3i (coerce loc) x y z

instance Uniform [V3 GLint] where
  uniform = uniformList glUniform3iv

instance Uniform (VS.Vector (V3 GLint)) where
  uniform = uniformVector glUniform3iv


instance Uniform (V4 GLint) where
  uniform loc (V4 x y z w) = glUniform4i (coerce loc) x y z w

instance Uniform [V4 GLint] where
  uniform = uniformList glUniform4iv

instance Uniform (VS.Vector (V4 GLint)) where
  uniform = uniformVector glUniform4iv


-- * GLuint based uniform vectors

instance Uniform GLuint where
  uniform loc = glUniform1ui (coerce loc)

instance Uniform [GLuint] where
  uniform = uniformList glUniform1uiv

instance Uniform (VS.Vector GLuint) where
  uniform = uniformVector glUniform1uiv


instance Uniform (V1 GLuint) where
  uniform loc (V1 x) = glUniform1ui (coerce loc) x

instance Uniform [V1 GLuint] where
  uniform = uniformList glUniform1uiv

instance Uniform (VS.Vector (V1 GLuint)) where
  uniform = uniformVector glUniform1uiv


instance Uniform (V2 GLuint) where
  uniform loc (V2 x y) = glUniform2ui (coerce loc) x y

instance Uniform [V2 GLuint] where
  uniform = uniformList glUniform2uiv

instance Uniform (VS.Vector (V2 GLuint)) where
  uniform = uniformVector glUniform2uiv


instance Uniform (V3 GLuint) where
  uniform loc (V3 x y z) = glUniform3ui (coerce loc) x y z

instance Uniform [V3 GLuint] where
  uniform = uniformList glUniform3uiv

instance Uniform (VS.Vector (V3 GLuint)) where
  uniform = uniformVector glUniform3uiv


instance Uniform (V4 GLuint) where
  uniform loc (V4 x y z w) = glUniform4ui (coerce loc) x y z w

instance Uniform [V4 GLuint] where
  uniform = uniformList glUniform4uiv

instance Uniform (VS.Vector (V4 GLuint)) where
  uniform = uniformVector glUniform4uiv


-- * Matrix based uniforms

-- | Generic function to upload uniform matrix.
uniformMatrix :: Storable a => (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> UniformLoc a -> a -> IO ()
uniformMatrix loadMat loc mat = alloca $ \p -> poke p mat
    >> loadMat (coerce loc) (fromIntegral $ sizeOf mat) GL_FALSE (castPtr p)
{-# INLINE uniformMatrix #-}

instance Uniform (M22 Float) where
  uniform = uniformMatrix glUniformMatrix2fv

instance Uniform (M33 Float) where
  uniform = uniformMatrix glUniformMatrix3fv

instance Uniform (M44 Float) where
  uniform = uniformMatrix glUniformMatrix4fv

instance Uniform (M23 Float) where
  uniform = uniformMatrix glUniformMatrix2x3fv

instance Uniform (M32 Float) where
  uniform = uniformMatrix glUniformMatrix3x2fv

instance Uniform (M24 Float) where
  uniform = uniformMatrix glUniformMatrix2x4fv

instance Uniform (M42 Float) where
  uniform = uniformMatrix glUniformMatrix4x2fv

instance Uniform (M34 Float) where
  uniform = uniformMatrix glUniformMatrix3x4fv

instance Uniform (M43 Float) where
  uniform = uniformMatrix glUniformMatrix4x3fv