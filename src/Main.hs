{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Codec.Picture        as JP
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IfElse
import           Control.Monad.Trans.Resource
import           Data.Bits
import qualified Data.ByteString      as BS
import           Data.IORef
import           Data.Maybe
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types
import qualified Graphics.UI.GLFW     as GLFW
import           Linear
import           System.Directory     as Dir

import           Ninja.GL
import           Ninja.Particles
import           Ninja.GL2D.Sprite

ngon :: Int -> GLfloat -> [V3 GLfloat]
ngon n radius = map mkV [0..n-1] where
  mkV i = V3 (cos (toAngle i) * radius) (sin (toAngle i) * radius) 0
  toAngle i = fromIntegral i / fromIntegral n * 2 * pi

testBuffer :: [V3 GLfloat]
testBuffer =
  [ V3 (-1.0) (-1.0) 0.0
  , V3   1.0  (-1.0) 0.0
  , V3   0.0    1.0  0.0
  ]

data PosTex = PosTex
  { vertexPos :: V3 Float
  , vertexTex :: V2 Float
  }

instance Storable PosTex where
  sizeOf _ = sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
  alignment = sizeOf
  peek ptr = PosTex <$> peek (castPtr ptr) <*> peekByteOff ptr (sizeOf (undefined :: V3 Float))
  poke ptr (PosTex p t) = poke (castPtr ptr) p >> pokeByteOff ptr (sizeOf (undefined :: V3 Float)) t

testSquare :: [PosTex]
testSquare =
  [ PosTex (V3 (-1.0) (-1.0) 0.0) (V2 0 0)
  , PosTex (V3   1.0  (-1.0) 0.0) (V2 1 0)
  , PosTex (V3   1.0    1.0  0.0) (V2 1 1)
  , PosTex (V3 (-1.0)   1.0  0.0) (V2 0 1)
  ]

loadShaders :: FilePath -> FilePath -> IO Program
loadShaders vertPath fragPath = do
  putStrLn "Loading source from files"
  vertSource <- BS.readFile vertPath
  fragSource <- BS.readFile fragPath
  createProgramFromSource [vertSource] [] [fragSource]

main :: IO ()
main = withGLFW BorderlessFullscreen "Yolo Ninja" hints $ \win -> do
    glEnable GL_TEXTURE_2D

    putStrLn "initialization"
    renderer <- newSpriteRenderer

    {-batch <- initSpriteBatch 10
    sprite <- loadSprite "data/tex/explosion.png" (V2 1 1) (V3 (-0.5) (-0.5) 0)
    print sprite
    -}
    putStrLn "Begin Loop"
    untilM (GLFW.windowShouldClose win) $ do
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
      {-
      draw batch sprite
      -}

      GLFW.swapBuffers win
      GLFW.pollEvents
    putStrLn "Exit"
    deleteSpriteRenderer renderer
  where
    hints = GLFW.WindowHint'Samples 4 : openGL33Core

-- * GLFW Wrapper

-- | Window hints stating the requirement of OpenGL 3.3 Core
openGL33Core :: [GLFW.WindowHint]
openGL33Core =
  [ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  , GLFW.WindowHint'ContextVersionMajor 3
  , GLFW.WindowHint'ContextVersionMinor 3
  ]

-- | Window mode for high-level GLFW wrapper
data WindowMode
  = Windowed Int Int
  | BorderlessFullscreen

-- | High-level GLFW initialization function.
withGLFW :: WindowMode -> String -> [GLFW.WindowHint] -> (GLFW.Window -> IO ()) -> IO ()
withGLFW mode title hints runApp = do
    liftIO $ GLFW.setErrorCallback (Just onError)
    bracket GLFW.init (`when` GLFW.terminate) $ const $ do
        (w,h,mon) <- case mode of
            Windowed w h -> return (w,h,Nothing)
            BorderlessFullscreen -> do
              mon <- GLFW.getPrimaryMonitor <?> "no primary monitor"
              vm <- GLFW.getVideoMode mon   <?> "error getting video mode"
              GLFW.windowHint (GLFW.WindowHint'RedBits $ GLFW.videoModeRedBits vm)
              GLFW.windowHint (GLFW.WindowHint'GreenBits $ GLFW.videoModeGreenBits vm)
              GLFW.windowHint (GLFW.WindowHint'BlueBits $ GLFW.videoModeBlueBits vm)
              GLFW.windowHint (GLFW.WindowHint'RefreshRate $ GLFW.videoModeRefreshRate vm)
              return (GLFW.videoModeWidth vm, GLFW.videoModeHeight vm, Just mon)
        mapM_ GLFW.windowHint hints
        win <- GLFW.createWindow w h title mon Nothing <?> "window creation failed"
        GLFW.makeContextCurrent (Just win)
        runApp win `finally` GLFW.destroyWindow win
  where
    onError err msg = do
      putStrLn "********** INIT ERROR **********"
      print err
      putStrLn msg
      putStrLn "********************************"

infixr 0 <?>
(<?>) :: IO (Maybe a) -> String -> IO a
act <?> err = act >>= maybe (ioError $ userError err) return
