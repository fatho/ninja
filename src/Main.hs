{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase, ForeignFunctionInterface #-}
module Main where

import qualified Codec.Picture                as JP
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IfElse
import           Data.Bits
import           Data.Fixed (mod')
import           Data.IORef
import qualified Data.Vector.Storable         as VS
import           Foreign.C.String
import           Foreign.Ptr
import           Graphics.GL.Core33
import qualified Graphics.GL.Ext.ARB.DebugOutput as Dbg
import           Graphics.GL.Types
import qualified Graphics.UI.GLFW             as GLFW
import           Linear
import           System.Directory             as Dir
import           System.IO

import           Ninja.GL
import           Ninja.GL2D.Sprite
import           Ninja.Util

onDebugMessage :: GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
onDebugMessage _ _ _ _ len msg _ = do
  putStr "[OPENGL] "
  peekCStringLen (msg,fromIntegral len) >>= putStrLn

onFramebufferResized :: (Double -> IO ()) -> GLFW.Window -> Int -> Int -> IO ()
onFramebufferResized setRatio _ w h = glViewport 0 0 (fromIntegral w) (fromIntegral h)
  >> setRatio (fromIntegral w / fromIntegral h)

main :: IO ()
main = withGLFW (Windowed 800 800) "Yolo Ninja" hints $ \win -> do
    callback <- mkGLDEBUGPROCARB onDebugMessage
    Dbg.glDebugMessageCallbackARB callback nullPtr

    ratioRef <- newIORef undefined
    GLFW.setFramebufferSizeCallback win (Just $ onFramebufferResized $ writeIORef ratioRef)

    printGLStats

    -- extensions that should be used if available according to <https://www.opengl.org/wiki/Common_Mistakes>
    exts <- openGLExtensions
    requireExtension exts "GL_ARB_texture_storage"
    ----------------------------------------------------------

    putStrLn "initialization"

    renderer <- newSpriteRenderer 360
    Dir.getCurrentDirectory >>= hPutStrLn stderr

    --sampleTex <- textureFromFile "data/tex/alpha.png" True
    redTex <- gen1
    boundTexture Texture2D $= redTex
    textureMinFilter Texture2D $= FilterNearest
    textureMagFilter Texture2D $= FilterNearest
    textureStorage Texture2D 1 GL_RGBA8 (V2 2 2)
    textureSubImage Texture2D 0 0 $= JP.ImageRGBA8 
      (JP.Image 2 2 (VS.fromList [ 255, 0, 0, 255,  0, 255, 0, 255
                                 , 0, 0, 255, 255,  255, 255, 0, 255 ]))
    cloudTex <- textureFromFile "data/tex/cloud.png" True
    explosionTex <- textureFromFile "data/tex/explosion.png" True
    wallTex <- textureFromFile "data/tex/wall.jpg" True

    glPointSize 10
    glClearColor 0 0 0 1

    putStrLn "Begin Loop"
    untilM (GLFW.windowShouldClose win) $ do
      ratio <- readIORef ratioRef
      -- TODO: use ratio to create projection matrix
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT

      Just time <- GLFW.getTime

      --let t = 0.5
      let rnd co = snd (properFraction (sin (co `dot` V2 12.9898 78.233) * 43758.5453) :: (Int, Float))

      let f t a = let 
                      t' = t `mod'` 4
                      tmp = rnd (V2 (1+a) a)
                      speed = max 0.3 $ if tmp < 0.5 then 0.5 * (sqrt $ 2 * tmp) else 0.5 + 0.5 * (sqrt $ 2 * tmp)
                      size = min 0.2 t'
                      d = max 0 ((t' - 0.15) / 2) --(t' / 2 * 0.9 + 0.1)**1.2
                      fadeout = max 0 $ min 1 $ (1 - t' / 4)
                  in SpriteVertex (speed * d *^ V3 (cos a) (sin a) 0 ^ 5) (realToFrac $ size) 0.5 0.5 ((rnd (V2 a a) * 2 - 1) * t' * speed)
                          (realToFrac $ fadeout) -- (fadeout *^ V4 (0.27 + 0.73 * fadeout) (0.25 - fadeout * 0.25) 0 1)
      let sprites = map (f (realToFrac time * 2) . (/180) . (*pi)) [0,2..359]

      drawWithTexture renderer wallTex 
        [ SpriteVertex (V3 (-0.6) (-0.6) 0) 0.4 0.5 0.5 0 1
        , SpriteVertex (V3 (-0.6)   0.6  0) 0.4 0.5 0.5 0 1
        , SpriteVertex (V3   0.6  (-0.6) 0) 0.4 0.5 0.5 0 1
        , SpriteVertex (V3   0.6    0.6  0) 0.4 0.5 0.5 0 1
        ]

      withBlending BlendSrcPlusDst BlendAdditiveFunc Nothing $
        drawWithTexture renderer explosionTex sprites

      GLFW.swapBuffers win
      GLFW.pollEvents

    putStrLn "Exit"
    deleteSpriteRenderer renderer
  where
    hints = GLFW.WindowHint'OpenGLDebugContext True 
          : GLFW.WindowHint'Resizable False
          : GLFW.WindowHint'Samples 4 
          : openGL33Core


printGLStats :: IO ()
printGLStats = do
  putStr "Version:  "
  glGetString GL_VERSION >>= peekCString . castPtr >>= putStrLn
  putStr "Vendor:   "
  glGetString GL_VENDOR >>= peekCString . castPtr >>= putStrLn
  putStr "Renderer: "
  glGetString GL_RENDERER >>= peekCString . castPtr >>= putStrLn
  putStr "GLSL:     "
  glGetString GL_SHADING_LANGUAGE_VERSION >>= peekCString . castPtr >>= putStrLn
  putStr "#TIU:     "
  withPtrOut (glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS) >>= print
  --putStrLn "Extensions:"
  --openGLExtensions >>= mapM_ (\str -> putStrLn $ "  " ++ str)

-- * OpenGL Extensions

requireExtension :: [String] -> String -> IO ()
requireExtension exts ext = unless (ext `elem` exts) $ ioError $ userError $ ext ++ " not available"

openGLExtensions :: IO [String]
openGLExtensions = do
  num <- withPtrOut $ glGetIntegerv GL_NUM_EXTENSIONS
  mapM (glGetStringi GL_EXTENSIONS . fromIntegral >=> peekCString . castPtr) [0..num-1]

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
