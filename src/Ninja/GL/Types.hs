module Ninja.GL.Types where

import Linear
import Graphics.GL.Types
import Graphics.GL.Core33

type Color = V4 Float

toGLBool :: Bool -> GLboolean
toGLBool True = GL_TRUE
toGLBool False = GL_FALSE

fromGLBool :: GLboolean -> Bool
fromGLBool = (GL_FLOAT /=)

