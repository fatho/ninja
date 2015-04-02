{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Ninja.GL2D.Sprite where

import qualified Codec.Picture          as JP
import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
import           Data.IORef
import qualified Data.List              as List
import           Data.Ord
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
import           System.IO.Unsafe       (unsafePerformIO)
import           Text.RawString.QQ

import           Ninja.GL


data SpriteRenderer = SpriteRenderer
  { spriteRendererProgram :: Program
  }


newSpriteRenderer :: IO SpriteRenderer
newSpriteRenderer = undefined

-- = Source code of the sprite rendering shaders.

vertexShaderSource :: String
vertexShaderSource = [r|#version 330 core

in vec4 spritePos;
in vec2 spriteExtend;
in vec2 spriteUV
in vec2 spriteUVExtend;

out vec4 spritePos;
out vec2 spriteExtend;
out vec2 spriteUV
out vec2 spriteUVExtend;

void main() {
}
|]

geometryShaderSource :: String
geometryShaderSource = [r|#version 330 core

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in VertexData {
  vec2 spriteExtend;
  vec2 spriteUV
  vec2 spriteUVExtend;
} VertexIn[3];

out VertexData {
    vec2 texCoord;
} VertexOut;

uniform mat4 mvp;

void main() {
}
|]

fragmentShaderSource :: String
fragmentShaderSource = [r|#version 330 core

in vec2 texCoord;
out vec4 color;

uniform vec4 tint_color;
uniform sampler2D tex;

void main() {
    color = texture(tex, texCoord) * tint_color;
}
|]