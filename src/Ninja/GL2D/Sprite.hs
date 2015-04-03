{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
module Ninja.GL2D.Sprite where

import           Control.Lens
import           Data.Data
import qualified Data.List               as List
import           Data.StateVar
import           Foreign.Storable
import           GHC.Generics
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear
import           Text.RawString.QQ

import           Ninja.GL
import           Ninja.Storable.Generics

data SpriteVertex = SpriteVertex
  { spriteVertexPos      :: V3 Float
  , spriteVertexExtend   :: V2 Float
  , spriteVertexUV       :: V2 Float
  , spriteVertexUVExtend :: V2 Float
  , spriteVertexRotation :: Float
  , spriteVertexTint     :: V4 Float
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

in vec3  spriteVertexPos;
in vec2  spriteVertexExtend;
in vec2  spriteVertexUV;
in vec2  spriteVertexUVExtend;
in float spriteVertexRotation;
in vec4  spriteVertexTint;

out vec2 spriteExtend_g;
out vec2 spriteUV_g;
out vec2 spriteUVExtend_g;
out float spriteRotation_g;
out vec4  spriteTint_g;

void main() {
  gl_Position = vec4(spriteVertexPos, 1);
  spriteExtend_g = spriteVertexExtend;
  spriteUV_g = spriteVertexUV;
  spriteUVExtend_g = spriteVertexUVExtend;
  spriteRotation_g = spriteVertexRotation;
  spriteTint_g = spriteVertexTint;
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
in vec4 spriteTint_g[];

out vec2 texCoord;
out vec4 tintColor;

uniform mat4 mvp;

void main() {
  vec2 extX = vec2( cos(spriteRotation_g[0]) * spriteExtend_g[0].x
                  , -sin(spriteRotation_g[0]) * spriteExtend_g[0].x
                  );
  vec2 extY = vec2( sin(spriteRotation_g[0]) * spriteExtend_g[0].y
                  , cos(spriteRotation_g[0]) * spriteExtend_g[0].y
                  );

  tintColor = spriteTint_g[0];

  vec4 cornerPos = gl_in[0].gl_Position;
  texCoord = spriteUV_g[0];

  cornerPos.xy -= extX;
  cornerPos.xy -= extY;

  texCoord.x -= spriteUVExtend_g[0].x;
  texCoord.y -= spriteUVExtend_g[0].y;
  gl_Position = mvp * cornerPos;
  EmitVertex();

  cornerPos.xy += 2 * extX;
  gl_Position = mvp * cornerPos;

  texCoord.x += 2 * spriteUVExtend_g[0].x;
  EmitVertex();

  cornerPos.xy -= 2 * extX;
  cornerPos.xy += 2 * extY;
  gl_Position = mvp * cornerPos;

  texCoord.x -= 2 * spriteUVExtend_g[0].x;
  texCoord.y += 2 * spriteUVExtend_g[0].y;
  EmitVertex();

  cornerPos.xy += 2 * extX;
  gl_Position = mvp * cornerPos;

  texCoord.x += 2 * spriteUVExtend_g[0].x;
  EmitVertex();
  EndPrimitive();
}
|]

fragmentShaderSource :: String
fragmentShaderSource = [r|#version 330 core

in vec2 texCoord;
in vec4 tintColor;
out vec4 color;

uniform sampler2D tex;

void main() {
    color = texture(tex, texCoord) * tintColor;
}
|]
