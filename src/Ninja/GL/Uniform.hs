{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ninja.GL.Uniform where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.StateVar
import qualified Data.Vector.Storable   as VS
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear

import           Ninja.GL.Object
import           Ninja.GL.Program
import           Ninja.Util

-- | A uniform is just an index.
newtype Uniform a = Uniform { uniformIndex :: GLint } deriving (Eq, Ord, Show)

-- | Class of values that can be uploaded to uniform.
type Uniformable a = HasSetter (Uniform a) a

-- | Returns a uniform of the given program.
uniformOf :: Program -> String -> IO (Uniform a)
uniformOf prog name = Uniform <$> withCString name (glGetUniformLocation (objectId prog))

-- | Checks if the uniform refers to a valid index.
uniformValid :: Uniform a -> Bool
uniformValid = (>=0) . uniformIndex

-- | Generic function to upload a uniform vector.
uniformVector :: (MonadIO m, Storable a) => (GLint -> GLsizei -> Ptr b -> IO ()) -> Uniform (VS.Vector a) -> VS.Vector a -> m ()
uniformVector loadVec loc vs = liftIO $ VS.unsafeWith vs $ \arr -> loadVec (coerce loc) (fromIntegral $ VS.length vs) (castPtr arr)
{-# INLINE uniformVector #-}

-- | Generic function to upload a uniform vector given by a list.
uniformList :: (MonadIO m, Storable a) => (GLint -> GLsizei -> Ptr b -> IO ()) -> Uniform [a] -> [a] -> m ()
uniformList loadVec loc vs = liftIO $ withArrayLen vs $ \len arr -> loadVec (coerce loc) (fromIntegral len) (castPtr arr)
{-# INLINE uniformList #-}

-- * Float based uniform vectors

instance HasSetter (Uniform Float) Float where
  ($=) = glUniform1f . coerce

instance HasSetter (Uniform [Float]) [Float] where
  ($=) = uniformList glUniform1fv

instance HasSetter (Uniform (VS.Vector Float)) (VS.Vector Float) where
  ($=) = uniformVector glUniform1fv


instance HasSetter (Uniform (V1 Float)) (V1 Float) where
  ($=) loc (V1 x) = glUniform1f (coerce loc) x

instance HasSetter (Uniform [V1 Float]) [V1 Float] where
  ($=) = uniformList glUniform1fv

instance HasSetter (Uniform (VS.Vector (V1 Float))) (VS.Vector (V1 Float)) where
  ($=) = uniformVector glUniform1fv


instance HasSetter (Uniform (V2 Float)) (V2 Float) where
  ($=) loc (V2 x y) = glUniform2f (coerce loc) x y

instance HasSetter (Uniform [V2 Float]) [V2 Float] where
  ($=) = uniformList glUniform2fv

instance HasSetter (Uniform (VS.Vector (V2 Float))) (VS.Vector (V2 Float)) where
  ($=) = uniformVector glUniform2fv


instance HasSetter (Uniform (V3 Float)) (V3 Float) where
  ($=) loc (V3 x y z) = glUniform3f (coerce loc) x y z

instance HasSetter (Uniform [V3 Float]) [V3 Float] where
  ($=) = uniformList glUniform3fv

instance HasSetter (Uniform (VS.Vector (V3 Float))) (VS.Vector (V3 Float)) where
  ($=) = uniformVector glUniform3fv


instance HasSetter (Uniform (V4 Float)) (V4 Float) where
  ($=) loc (V4 x y z w) = glUniform4f (coerce loc) x y z w

instance HasSetter (Uniform [V4 Float]) [V4 Float] where
  ($=) = uniformList glUniform4fv

instance HasSetter (Uniform (VS.Vector (V4 Float))) (VS.Vector (V4 Float)) where
  ($=) = uniformVector glUniform4fv


-- * GLint based uniform vectors

instance HasSetter (Uniform GLint) GLint where
  ($=) = glUniform1i . coerce

instance HasSetter (Uniform [GLint]) [GLint] where
  ($=) = uniformList glUniform1iv

instance HasSetter (Uniform (VS.Vector GLint)) (VS.Vector GLint) where
  ($=) = uniformVector glUniform1iv


instance HasSetter (Uniform (V1 GLint)) (V1 GLint) where
  ($=) loc (V1 x) = glUniform1i (coerce loc) x

instance HasSetter (Uniform [V1 GLint]) [V1 GLint] where
  ($=) = uniformList glUniform1iv

instance HasSetter (Uniform (VS.Vector (V1 GLint))) (VS.Vector (V1 GLint)) where
  ($=) = uniformVector glUniform1iv


instance HasSetter (Uniform (V2 GLint)) (V2 GLint) where
  ($=) loc (V2 x y) = glUniform2i (coerce loc) x y

instance HasSetter (Uniform [V2 GLint]) [V2 GLint] where
  ($=) = uniformList glUniform2iv

instance HasSetter (Uniform (VS.Vector (V2 GLint))) (VS.Vector (V2 GLint)) where
  ($=) = uniformVector glUniform2iv


instance HasSetter (Uniform (V3 GLint)) (V3 GLint) where
  ($=) loc (V3 x y z) = glUniform3i (coerce loc) x y z

instance HasSetter (Uniform [V3 GLint]) [V3 GLint] where
  ($=) = uniformList glUniform3iv

instance HasSetter (Uniform (VS.Vector (V3 GLint))) (VS.Vector (V3 GLint)) where
  ($=) = uniformVector glUniform3iv


instance HasSetter (Uniform (V4 GLint)) (V4 GLint) where
  ($=) loc (V4 x y z w) = glUniform4i (coerce loc) x y z w

instance HasSetter (Uniform [V4 GLint]) [V4 GLint] where
  ($=) = uniformList glUniform4iv

instance HasSetter (Uniform (VS.Vector (V4 GLint))) (VS.Vector (V4 GLint)) where
  ($=) = uniformVector glUniform4iv


-- * GLuint based uniform vectors

instance HasSetter (Uniform GLuint) GLuint where
  ($=) = glUniform1ui . coerce

instance HasSetter (Uniform [GLuint]) [GLuint] where
  ($=) = uniformList glUniform1uiv

instance HasSetter (Uniform (VS.Vector GLuint)) (VS.Vector GLuint) where
  ($=) = uniformVector glUniform1uiv


instance HasSetter (Uniform (V1 GLuint)) (V1 GLuint) where
  ($=) loc (V1 x) = glUniform1ui (coerce loc) x

instance HasSetter (Uniform [V1 GLuint]) [V1 GLuint] where
  ($=) = uniformList glUniform1uiv

instance HasSetter (Uniform (VS.Vector (V1 GLuint))) (VS.Vector (V1 GLuint)) where
  ($=) = uniformVector glUniform1uiv


instance HasSetter (Uniform (V2 GLuint)) (V2 GLuint) where
  ($=) loc (V2 x y) = glUniform2ui (coerce loc) x y

instance HasSetter (Uniform [V2 GLuint]) [V2 GLuint] where
  ($=) = uniformList glUniform2uiv

instance HasSetter (Uniform (VS.Vector (V2 GLuint))) (VS.Vector (V2 GLuint)) where
  ($=) = uniformVector glUniform2uiv


instance HasSetter (Uniform (V3 GLuint)) (V3 GLuint) where
  ($=) loc (V3 x y z) = glUniform3ui (coerce loc) x y z

instance HasSetter (Uniform [V3 GLuint]) [V3 GLuint] where
  ($=) = uniformList glUniform3uiv

instance HasSetter (Uniform (VS.Vector (V3 GLuint))) (VS.Vector (V3 GLuint)) where
  ($=) = uniformVector glUniform3uiv


instance HasSetter (Uniform (V4 GLuint)) (V4 GLuint) where
  ($=) loc (V4 x y z w) = glUniform4ui (coerce loc) x y z w

instance HasSetter (Uniform [V4 GLuint]) [V4 GLuint] where
  ($=) = uniformList glUniform4uiv

instance HasSetter (Uniform (VS.Vector (V4 GLuint))) (VS.Vector (V4 GLuint)) where
  ($=) = uniformVector glUniform4uiv


-- * Matrix based uniforms

-- | Generic function to upload uniform matrix.
uniformMatrix :: (MonadIO m, Storable a) => (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()) -> Uniform a -> a -> m ()
uniformMatrix loadMat loc mat = liftIO $ withPtrIn mat $ loadMat (coerce loc) 1 GL_FALSE . castPtr
{-# INLINE uniformMatrix #-}

-- | Generic function to upload uniform matrix.
uniformMatrixVector :: (MonadIO m, Storable a) => (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
  -> Uniform (VS.Vector a) -> VS.Vector a -> m ()
uniformMatrixVector loadMat loc mats = liftIO $ VS.unsafeWith mats $ loadMat (coerce loc) (fromIntegral $ VS.length mats) GL_FALSE . castPtr

instance HasSetter (Uniform (M22 Float)) (M22 Float) where
  ($=) = uniformMatrix glUniformMatrix2fv

instance HasSetter (Uniform (M33 Float)) (M33 Float) where
  ($=) = uniformMatrix glUniformMatrix3fv

instance HasSetter (Uniform (M44 Float)) (M44 Float) where
  ($=) = uniformMatrix glUniformMatrix4fv

instance HasSetter (Uniform (M23 Float)) (M23 Float) where
  ($=) = uniformMatrix glUniformMatrix2x3fv

instance HasSetter (Uniform (M32 Float)) (M32 Float) where
  ($=) = uniformMatrix glUniformMatrix3x2fv

instance HasSetter (Uniform (M24 Float)) (M24 Float) where
  ($=) = uniformMatrix glUniformMatrix2x4fv

instance HasSetter (Uniform (M42 Float)) (M42 Float) where
  ($=) = uniformMatrix glUniformMatrix4x2fv

instance HasSetter (Uniform (M34 Float)) (M34 Float) where
  ($=) = uniformMatrix glUniformMatrix3x4fv

instance HasSetter (Uniform (M43 Float)) (M43 Float) where
  ($=) = uniformMatrix glUniformMatrix4x3fv
