{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
module Graphics.Ninja.GL2D.Sprite where

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

import           Graphics.Ninja.GL
import           Graphics.Ninja.Storable.Generics

-- | Data associated with a sprite, ready to be sent to the shader.
data SpriteVertex = SpriteVertex
  { spriteVertexPos      :: V3 Float
  -- ^ center position in 3D space
  , spriteVertexExtent   :: V2 Float
  -- ^ extent in both X and both Y directions (i.e. half of the total size).
  , spriteVertexUV       :: V2 Float
  -- ^ center UV coordinate
  , spriteVertexUVExtent :: V2 Float
  -- ^ UV coordinate extent (similar to real size)
  , spriteVertexRotation :: Float
  -- ^ counter-clockwise rotation in radians
  , spriteVertexTint     :: V4 Float
  -- ^ tint color applied during the fragment shader phase.
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

-- | Encapsulates the global state needed for sprite rendering.
data SpriteRenderer = SpriteRenderer
  { spriteRendererProgram :: Program
  -- ^ shader program processing the vertices
  , spriteRendererVAO     :: VertexArray
  -- ^ vertex array object needed for drawing
  , spriteRendererBuffer  :: Buffer SpriteVertex
  -- ^ array buffer taking the sprites
  , spriteRendererSize    :: Int
  -- ^ maximum number of sprite vertices fitting in the buffer
  , spriteRendererMVP     :: Uniform (M44 Float)
  -- ^ uniform for transferring the model-view-projection matrix
  , spriteRendererSampler :: Uniform GLint
  -- ^ sampler uniform
  }

newSpriteRenderer :: Int -> IO SpriteRenderer
newSpriteRenderer nmax = do
  prog <- createProgramFromSource [vertexShaderSource] [geometryShaderSource] [fragmentShaderSource]
  mvp <- uniformOf prog "mvp"
  tex <- uniformOf prog "tex"
  vao <- gen1
  buf <- gen1
  withVertexArray vao $ do
    boundBuffer ArrayBuffer $= buf
    -- preallocate a buffer on the GPU
    bufferData ArrayBuffer $= (StreamDraw, NullData (fromIntegral $ nmax * spriteVertexSize))

    let layout = vertexLayout (undefined :: SpriteVertex)
    applyLayoutByName prog layout

  return $ SpriteRenderer prog vao buf nmax mvp tex

deleteSpriteRenderer :: SpriteRenderer -> IO ()
deleteSpriteRenderer SpriteRenderer{..} = delete1 spriteRendererProgram

drawWithTexture :: SpriteRenderer -> Texture -> [SpriteVertex] -> IO ()
drawWithTexture SpriteRenderer{..} tex sprites =
    withProgram spriteRendererProgram $
    withVertexArray spriteRendererVAO $ do
       boundBuffer ArrayBuffer $= spriteRendererBuffer
       spriteRendererMVP $= identity
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
in vec2  spriteVertexExtent;
in vec2  spriteVertexUV;
in vec2  spriteVertexUVExtent;
in float spriteVertexRotation;
in vec4  spriteVertexTint;

out vec2 spriteExtent_g;
out vec2 spriteUV_g;
out vec2 spriteUVExtent_g;
out float spriteRotation_g;
out vec4  spriteTint_g;

void main() {
  gl_Position = vec4(spriteVertexPos, 1);
  spriteExtent_g = spriteVertexExtent;
  spriteUV_g = spriteVertexUV;
  spriteUVExtent_g = spriteVertexUVExtent;
  spriteRotation_g = spriteVertexRotation;
  spriteTint_g = spriteVertexTint;
}
|]

geometryShaderSource :: String
geometryShaderSource = [r|#version 330 core

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in vec2 spriteExtent_g[];
in vec2 spriteUV_g[];
in vec2 spriteUVExtent_g[];
in float spriteRotation_g[];
in vec4 spriteTint_g[];

out vec2 texCoord;
out vec4 tintColor;

uniform mat4 mvp;

void main() {
  vec2 extX = vec2( cos(spriteRotation_g[0]) * spriteExtent_g[0].x
                  , -sin(spriteRotation_g[0]) * spriteExtent_g[0].x
                  );
  vec2 extY = vec2( sin(spriteRotation_g[0]) * spriteExtent_g[0].y
                  , cos(spriteRotation_g[0]) * spriteExtent_g[0].y
                  );

  tintColor = spriteTint_g[0];

  vec4 cornerPos = gl_in[0].gl_Position;
  texCoord = spriteUV_g[0];

  cornerPos.xy -= extX;
  cornerPos.xy -= extY;

  texCoord.x -= spriteUVExtent_g[0].x;
  texCoord.y -= spriteUVExtent_g[0].y;
  gl_Position = mvp * cornerPos;
  EmitVertex();

  cornerPos.xy += 2 * extX;
  gl_Position = mvp * cornerPos;

  texCoord.x += 2 * spriteUVExtent_g[0].x;
  EmitVertex();

  cornerPos.xy -= 2 * extX;
  cornerPos.xy += 2 * extY;
  gl_Position = mvp * cornerPos;

  texCoord.x -= 2 * spriteUVExtent_g[0].x;
  texCoord.y += 2 * spriteUVExtent_g[0].y;
  EmitVertex();

  cornerPos.xy += 2 * extX;
  gl_Position = mvp * cornerPos;

  texCoord.x += 2 * spriteUVExtent_g[0].x;
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
