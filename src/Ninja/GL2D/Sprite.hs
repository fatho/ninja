{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Data
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
import           GHC.Generics
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear
import           System.IO.Unsafe       (unsafePerformIO)
import           Text.RawString.QQ

import           Ninja.GL

data SpriteVertex = SpriteVertex
  { spriteVertexPos      :: V3 Float
  , spriteVertexExtend   :: V2 Float
  , spriteVertexUV       :: V2 Float
  , spriteVertexUVExtend :: V2 Float
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data SpriteRenderer = SpriteRenderer
  { spriteRendererProgram :: Program
  }

newSpriteRenderer :: IO SpriteRenderer
newSpriteRenderer = do
  prog <- createProgramFromSource [vertexShaderSource] [geometryShaderSource] [fragmentShaderSource]
  return $ SpriteRenderer prog

deleteSpriteRenderer :: SpriteRenderer -> IO ()
deleteSpriteRenderer SpriteRenderer{..} = delete1 spriteRendererProgram

-- = Source code of the sprite rendering shaders.

vertexShaderSource :: String
vertexShaderSource = [r|#version 330 core

layout(location=0) in vec4 spritePos;
layout(location=1) in vec2 spriteExtend;
layout(location=2) in vec2 spriteUV;
layout(location=3) in vec2 spriteUVExtend;

out vec2 spriteExtend_g;
out vec2 spriteUV_g;
out vec2 spriteUVExtend_g;

void main() {
}
|]

geometryShaderSource :: String
geometryShaderSource = [r|#version 330 core

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in VertexData {
  vec2 spriteExtend_g;
  vec2 spriteUV_g;
  vec2 spriteUVExtend_g;
} VertexIn[1];

out FragmentData {
    vec2 texCoord;
} VertexOut;

uniform mat4 mvp;

void main() {
  gl_Position = gl_in[0].gl_Position;
  gl_Position.xy -= VertexIn[0].spriteExtend_g;
  VertexOut.texCoord = VertexIn[0].spriteUV_g;
  VertexOut.texCoord -= VertexIn[0].spriteUVExtend_g;
  EmitVertex();

  gl_Position.x += VertexIn[0].spriteExtend_g.x;
  VertexOut.texCoord.x += VertexIn[0].spriteUVExtend_g.x;
  EmitVertex();

  gl_Position.x -= VertexIn[0].spriteExtend_g.x;
  VertexOut.texCoord.x -= VertexIn[0].spriteUVExtend_g.x;
  gl_Position.y += VertexIn[0].spriteExtend_g.y;
  VertexOut.texCoord.y += VertexIn[0].spriteUVExtend_g.y;
  EmitVertex();

  gl_Position.x += VertexIn[0].spriteExtend_g.x;
  VertexOut.texCoord.x += VertexIn[0].spriteUVExtend_g.x;
  EmitVertex();
  EndPrimitive();
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