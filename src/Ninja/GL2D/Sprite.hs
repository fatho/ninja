{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
module Ninja.GL2D.Sprite where

import qualified Codec.Picture           as JP
import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Unsafe  as BSU
import           Data.Coerce
import           Data.Data
import           Data.Default.Class
import           Data.IORef
import qualified Data.List               as List
import           Data.Ord
import           Data.StateVar
import qualified Data.Vector.Storable    as VS
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
import           System.IO.Unsafe        (unsafePerformIO)
import           Text.RawString.QQ

import           Ninja.GL
import           Ninja.Storable.Generics

data SpriteVertex = SpriteVertex
  { spriteVertexPos      :: V3 Float
  , spriteVertexExtend   :: V2 Float
  , spriteVertexUV       :: V2 Float
  , spriteVertexUVExtend :: V2 Float
  , spriteVertexRotation :: Float
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Size of the SpriteVertex data.
spriteVertexSize :: Int
spriteVertexSize = sizeOf (undefined :: SpriteVertex)

instance Storable SpriteVertex where
  sizeOf = genericSizeOf
  alignment = genericAlignment
  peek = genericPeek
  poke = genericPoke
instance VertexData SpriteVertex

data SpriteRenderer = SpriteRenderer
  { spriteRendererProgram :: Program
  , spriteRendererVAO     :: VertexArray
  , spriteRendererBuffer  :: Buffer SpriteVertex
  , spriteRendererMVP     :: Uniform (M44 Float)
  , spriteRendererSampler :: Uniform GLint
  , spriteRendererSize    :: Int
  }

newSpriteRenderer :: Int -> IO SpriteRenderer
newSpriteRenderer nmax = do
  prog <- createProgramFromSource [vertexShaderSource] [geometryShaderSource] [fragmentShaderSource]
  putStr "is a prog: " >> isA prog >>= print
  mvp <- uniformOf prog "mvp"
  tex <- uniformOf prog "tex"
  vao <- gen1
  buf <- gen1
  withVertexArray vao $ do
    boundBuffer ArrayBuffer $= buf
    -- preallocate a buffer on the GPU
    putStrLn $ "allocating buffer of size " ++ show (nmax * spriteVertexSize) ++ " B"
    bufferData ArrayBuffer $= (StreamDraw, NullData (fromIntegral $ nmax * spriteVertexSize))
    putStrLn "applying layout"
    let layout = vertexLayout (undefined :: SpriteVertex)
    mapM_ print (view vertexAttribs layout)
    applyLayoutByName prog layout
  return $ SpriteRenderer prog vao buf mvp tex nmax

deleteSpriteRenderer :: SpriteRenderer -> IO ()
deleteSpriteRenderer SpriteRenderer{..} = delete1 spriteRendererProgram

drawWithTexture :: SpriteRenderer -> Texture -> [SpriteVertex] -> IO ()
drawWithTexture SpriteRenderer{..} tex sprites =
    withProgram spriteRendererProgram $
    withVertexArray spriteRendererVAO $ do
       boundBuffer ArrayBuffer $= spriteRendererBuffer
       spriteRendererMVP $= eye4
       activeTexture $= GL_TEXTURE0
       boundTexture Texture2D $= tex
       go sprites
  where
    go [] = return ()
    go sps = do
        let (now, later) = List.splitAt spriteRendererSize sps
            numElems     = length now
        bufferSubData ArrayBuffer 0 (fromIntegral $ numElems * spriteVertexSize) $= now
        glDrawArrays GL_POINTS 0 (fromIntegral numElems)
        go later

-- = Source code of the sprite rendering shaders.

vertexShaderSource :: String
vertexShaderSource = [r|#version 330 core

in vec3 spriteVertexPos;
in vec2 spriteVertexExtend;
in vec2 spriteVertexUV;
in vec2 spriteVertexUVExtend;
in float spriteVertexRotation;

out vec2 spriteExtend_g;
out vec2 spriteUV_g;
out vec2 spriteUVExtend_g;
out float spriteRotation_g;

void main() {
  gl_Position = vec4(spriteVertexPos, 1);
  spriteExtend_g = spriteVertexExtend;
  spriteUV_g = spriteVertexUV;
  spriteUVExtend_g = spriteVertexUVExtend;
  spriteRotation_g = spriteVertexRotation;
}
|]

geometryShaderSource :: String
geometryShaderSource = [r|#version 330 core

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec2 spriteExtend_g[];
in vec2 spriteUV_g[];
in vec2 spriteUVExtend_g[];
in float spriteRotation_g[];

out vec2 texCoord;

uniform mat4 mvp;

void main() {
  mat4 RotationMatrix = mat4( cos(spriteRotation_g[0]), -sin(spriteRotation_g[0]), 0.0, 0.0,
                               sin(spriteRotation_g[0]),  cos(spriteRotation_g[0]), 0.0, 0.0,
                               0.0,           0.0, 1.0, 0.0,
                               0.0,           0.0, 0.0, 1.0 );

  vec4 cornerPos = gl_in[0].gl_Position;
  texCoord = spriteUV_g[0];

  cornerPos.xy -= spriteExtend_g[0];
  texCoord.x -= spriteUVExtend_g[0].x;
  texCoord.y += spriteUVExtend_g[0].y;
  gl_Position = mvp * RotationMatrix * cornerPos;
  EmitVertex();

  cornerPos.x += 2 * spriteExtend_g[0].x;
  texCoord.x += 2 * spriteUVExtend_g[0].x;
  gl_Position = mvp * RotationMatrix * cornerPos;
  EmitVertex();

  cornerPos.x -= 2 * spriteExtend_g[0].x;
  texCoord.x -= 2 * spriteUVExtend_g[0].x;
  cornerPos.y += 2 * spriteExtend_g[0].y;
  texCoord.y -= 2 * spriteUVExtend_g[0].y;
  gl_Position = mvp * RotationMatrix * cornerPos;
  EmitVertex();

  cornerPos.x += 2 * spriteExtend_g[0].x;
  texCoord.x += 2 * spriteUVExtend_g[0].x;
  gl_Position = mvp * RotationMatrix * cornerPos;
  EmitVertex();
  EndPrimitive();
}
|]

fragmentShaderSource :: String
fragmentShaderSource = [r|#version 330 core

in vec2 texCoord;
out vec4 color;

uniform sampler2D tex;

void main() {
    color = texture(tex, texCoord);
}
|]
