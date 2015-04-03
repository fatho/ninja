{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Ninja.GL.Blending where

import           Control.Applicative
import           Data.StateVar
import           Foreign.Ptr
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear

import           Ninja.GL.Types
import           Ninja.Util

-- | Wraps the blending parameters for 'glBlendFuncSeparate'.
data BlendEquation = BlendEquation
  { blendEquationRgb   :: GLenum
  , blendEquationAlpha :: GLenum
  } deriving (Eq, Ord, Show, Read)

-- | Wraps the blending parameters for 'glBlendFuncSeparate'.
data BlendFunc = BlendFunc
  { blendFuncSrcRgb   :: GLenum
  , blendFuncDstRgb   :: GLenum
  , blendFuncSrcAlpha :: GLenum
  , blendFuncDstAlpha :: GLenum
  } deriving (Eq, Ord, Show, Read)

-- | Globally controls blending.
blendingEnabled :: StateVar Bool
blendingEnabled = makeStateVar g s where
  g = fromGLBool <$> glIsEnabled GL_BLEND
  s True = glEnable GL_BLEND
  s False = glDisable GL_BLEND

  -- | Controls blending for a single drawbuffer.
blendingEnabledFor :: GLuint -> StateVar Bool
blendingEnabledFor idx = makeStateVar g s where
  g = fromGLBool <$> glIsEnabledi GL_BLEND idx
  s True = glEnablei GL_BLEND idx
  s False = glDisablei GL_BLEND idx

-- | Globalls controls the blend equation.
blendEquation :: StateVar BlendEquation
blendEquation = makeStateVar g s where
  g = BlendEquation <$> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_EQUATION_RGB))
                    <*> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_EQUATION_ALPHA))
  s BlendEquation{..} = glBlendEquationSeparate blendEquationRgb blendEquationAlpha

-- | Globalls controls the blend function.
blendFunc :: StateVar BlendFunc
blendFunc = makeStateVar g s where
  g = BlendFunc <$> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_SRC_RGB))
                <*> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_SRC_ALPHA))
                <*> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_DST_RGB))
                <*> (fromIntegral <$> withPtrOut (glGetIntegerv GL_BLEND_DST_ALPHA))
  s BlendFunc{..} = glBlendFuncSeparate blendFuncSrcRgb blendFuncSrcAlpha blendFuncDstRgb blendFuncDstAlpha

-- | Globalls controls the blend color.
blendColor :: StateVar Color
blendColor = makeStateVar g s where
  g = withPtrOut (glGetFloatv GL_BLEND_COLOR . castPtr)
  s (V4 r g b a) = glBlendColor r g b a

-- | Temporarily enables the specified blending functions.
withBlending :: BlendEquation -> BlendFunc -> Maybe Color -> IO a -> IO a
withBlending equ func col = withVar blendingEnabled True
  . withVar blendEquation equ
  . withVar blendFunc func
  . maybe id (withVar blendColor) col

-- | Overwrites destination with source.
pattern BlendSourceFunc = BlendFunc GL_ONE GL_ZERO GL_ONE GL_ZERO
-- | Adds source and destination.
pattern BlendAdditiveFunc = BlendFunc GL_ONE GL_ONE GL_ONE GL_ONE
-- | Adds premultiplied source and to destination multiplied with inverse alpha.
pattern BlendAlphaPremultFunc = BlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO
-- | Alpha blending.
pattern BlendAlphaFunc = BlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO

-- | Additive blend equation (default).
pattern BlendSrcPlusDst = BlendEquation GL_FUNC_ADD GL_FUNC_ADD
-- | Subtract destination product from source product.
pattern BlendSrcMinusDst = BlendEquation GL_FUNC_SUBTRACT GL_FUNC_SUBTRACT
-- | Subtract source product from destination product.
pattern BlendDstMinusSrc = BlendEquation GL_FUNC_REVERSE_SUBTRACT GL_FUNC_REVERSE_SUBTRACT
-- | Uses component-wise minimum
pattern BlendMin = BlendEquation GL_MIN GL_MIN
-- | Uses component-wise maximum
pattern BlendMax = BlendEquation GL_MAX GL_MAX
