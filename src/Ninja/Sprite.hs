{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Ninja.Sprite where

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

data SpriteBatch = SpriteBatch
  { spriteBatchProgram    :: Program
  -- ^ the shader program responsible for drawing
  , spriteBatchTintColorU :: Uniform Color
  -- ^ tint color for textures, it is multiplied with the actual texture color
  , spriteBatchMatU       :: Uniform (M44 Float)
  -- ^ transformation matrix uniform
  , spriteBatchVAO        :: VertexArray
  -- ^ vertex array object for sprite vertices
  , spriteBatchBuffer     :: Buffer (VS.Vector SpriteVertex)
  -- ^ buffer containing sprite vertices
  , spriteBatchMaxBatch   :: Int
  }

-- | Vertex data for sprites.
data SpriteVertex = SpriteVertex
  { spriteVertexPos :: V3 Float
  -- ^ position of the vertex in 3D space
  , vertexTex       :: V2 Float
  -- ^ texture coordinate
  }
  deriving (Eq, Show)

instance Storable SpriteVertex where
  sizeOf _ = sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
  alignment = sizeOf
  peek ptr = SpriteVertex <$> peek (castPtr ptr) <*> peekByteOff ptr (sizeOf (undefined :: V3 Float))
  poke ptr (SpriteVertex p t) = poke (castPtr ptr) p >> pokeByteOff ptr (sizeOf (undefined :: V3 Float)) t

instance VertexData SpriteVertex where
  vertexLayout _ =
    [ VertexLayout 3 GL_FLOAT False stride 0
    , VertexLayout 2 GL_FLOAT False stride 12
    ] where stride = sizeOf (undefined :: SpriteVertex)

-------------------------------------------------------------------------------
-- * Shader code

-- | Fragment shader code
spriteBatchFrag :: String
spriteBatchFrag = [r|#version 330 core

in vec2 texCoord_frag;
out vec4 color;

uniform vec4 tint_color;
uniform sampler2D tex;

void main() {
    color = texture(tex, texCoord_frag) * tint_color;
}
|]

-- | Vertex shader code
spriteBatchVert :: String
spriteBatchVert = [r|#version 330 core

in vec3 vertexPos;
in vec2 texCoord_in;
out vec2 texCoord_frag;

uniform mat4 mvp;

void main() {
  vec4 pos = vec4(vertexPos, 1.0);
  gl_Position = mvp * pos;
  texCoord_frag = texCoord_in;
}
|]

-------------------------------------------------------------------------------
-- * Compilation

-- | Runs a compiler pipeline.
compilerPipeLine :: [(String, IO (Bool, String))] -> IO ()
compilerPipeLine = mapM_ runStage where
  runStage (name, act) = do
    (success, msg) <- act
    unless success $ ioError $ userError $ "stage '" ++ name ++ "': " ++ msg

-- | A compiler stage that is always succeeding
succeedingStage :: IO () -> IO (Bool, String)
succeedingStage act = act >> return (True, "")

-------------------------------------------------------------------------------
-- * Initialization

_spriteBatchProgramRef :: IORef (Maybe Program)
_spriteBatchProgramRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE _spriteBatchProgramRef #-}

getSpriteBatchProgram :: IO Program
getSpriteBatchProgram =
  readIORef _spriteBatchProgramRef >>= \case
    Just p -> return p
    Nothing -> do
     fragShader <- gen1 :: IO (Shader FragmentShader)
     vertShader <- gen1 :: IO (Shader VertexShader)
     prog <- gen1
     shaderSource fragShader $= spriteBatchFrag
     shaderSource vertShader $= spriteBatchVert
     compilerPipeLine
       [ ("vertex shader", compileShader vertShader)
       , ("fragment shader", compileShader fragShader)
       , ("attaching", succeedingStage $ attachShader prog vertShader >> attachShader prog fragShader )
       , ("fragment shader", compileShader fragShader)
       , ("linking", linkProgram prog)
       , ("detach and delete", succeedingStage $ do
             attachedShaders prog $= []
             delete1 vertShader
             delete1 fragShader
         )
       ]
     writeIORef _spriteBatchProgramRef (Just prog)
     return prog

-- | Initializes the sprite batch.
initSpriteBatch :: Int -> IO SpriteBatch
initSpriteBatch maxBatch = do
  prog <- getSpriteBatchProgram
  vao <- gen1
  buf <- gen1
  withVertexArray vao $ do
    boundBuffer ArrayBuffer $= buf
    bufferData ArrayBuffer $= (DynamicDraw, NullData $ fromIntegral $ sizeOf (undefined::SpriteVertex) * 4 * maxBatch)
  SpriteBatch prog
    <$> uniformOf prog "tint_color"
    <*> uniformOf prog "mvp"
    <*> pure vao
    <*> pure buf
    <*> pure maxBatch

-------------------------------------------------------------------------------
-- * Drawing
{-
draw :: SpriteBatch -> [Sprite] -> IO ()
draw SpriteBatch{..} sprites = withProgram spriteBatchProgram $ do
    activeTexture $= GL_TEXTURE20
    boundVertexArray $= spriteBatchVAO
    spriteBatchTintColorU $= 1
    map (id &&& mkQuad) sprites
  where
    mkQuad Sprite{..} = uncurry (spriteQuad spritePos spriteSize) spriteUV

-- | Creates a quad using a triangle fan from a sprite coordinate.
spriteQuad :: V3 Float -> V2 Float -> V2 Float -> V2 Float -> VS.Vector SpriteVertex
spriteQuad (V3 x y z) (V2 w h) (V2 u v) (V2 du dv) = VS.fromListN 4
  [ SpriteVertex (V3 x y z) (V2 u v)
  , SpriteVertex (V3 (x+w) y z) (V2 (u+du) v)
  , SpriteVertex (V3 (x+w) (y+h) z) (V2 (u+du) (v+dv))
  , SpriteVertex (V3 x (y+h) z) (V2 u (v+dv))
  ]
-}

draw :: SpriteBatch -> Sprite -> IO ()
draw SpriteBatch{..} Sprite{..} = withProgram spriteBatchProgram $
  withVertexArray spriteBatchVAO $ do
    activeTexture $= GL_TEXTURE0
    boundTexture Texture2D $= spriteTexture

    boundBuffer ArrayBuffer $= spriteBatchBuffer
    let bd = uncurry (spriteQuad spritePos spriteSize) spriteUV
    bufferSubData ArrayBuffer 0 (fromIntegral $ 4 * sizeOf (undefined::SpriteVertex))
      $= bd

    let [a0, a1] = vertexLayout (undefined::SpriteVertex)

    spriteBatchMatU       $= eye4
    spriteBatchTintColorU $= 1

    l0 <- attributeOf spriteBatchProgram "vertexPos"
    l1 <- attributeOf spriteBatchProgram "texCoord_in"

    attribEnabled l0 $= True
    attribLayout  l0 $= a0
    attribEnabled l1 $= True
    attribLayout  l1 $= a1

    glDrawArrays GL_TRIANGLE_FAN 0 4

    attribEnabled l0 $= False
    attribEnabled l1 $= False

-- | Creates a quad using a triangle fan from a sprite coordinate.
spriteQuad :: V3 Float -> V2 Float -> V2 Float -> V2 Float -> VS.Vector SpriteVertex
spriteQuad (V3 x y z) (V2 w h) (V2 u v) (V2 du dv) = VS.fromListN 4
  [ SpriteVertex (V3 x y z) (V2 u v)
  , SpriteVertex (V3 (x+w) y z) (V2 (u+du) v)
  , SpriteVertex (V3 (x+w) (y+h) z) (V2 (u+du) (v+dv))
  , SpriteVertex (V3 x (y+h) z) (V2 u (v+dv))
  ]

-- | Sprite with size and position.
data Sprite = Sprite
  { spriteTexture :: Texture
  -- ^ sprite texture object. must have been allocated previously.
  , spriteUV      :: (V2 Float, V2 Float)
  -- ^ UV range of this sprite. First component is bottom left, second component ist size.
  , spriteSize    :: V2 Float
  -- ^ size of the sprite in world coordinates.
  , spritePos     :: V3 Float
  -- ^ bottom left corner of the sprite in world coordinates. the Z coordinate is used for all four corners.
  }
  deriving (Show)

-- | Loads a sprite from a file with an initial texture, position and size.
loadSprite :: FilePath -> V2 Float -> V3 Float -> IO Sprite
loadSprite path size pos = do
  tex <- textureFromFile path
  return Sprite
    { spriteTexture = tex
    , spriteUV      = (0, 1)
    , spriteSize    = size
    , spritePos     = pos
    }

