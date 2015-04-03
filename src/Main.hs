{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase, ForeignFunctionInterface #-}
module Main where

import qualified Codec.Picture                as JP
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IfElse
import           Control.Monad.Trans.Resource
import           Data.Bits
import qualified Data.ByteString              as BS
import           Data.IORef
import           Data.Maybe
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
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
onDebugMessage source typ msgId severity len msg usr = do
  putStr "[OPENGL] "
  peekCStringLen (msg,fromIntegral len) >>= putStrLn

onFramebufferResized :: (Double -> IO ()) -> GLFW.Window -> Int -> Int -> IO ()
onFramebufferResized setRatio win w h = glViewport 0 0 (fromIntegral w) (fromIntegral h)
  >> setRatio (fromIntegral w / fromIntegral h)

main :: IO ()
main = withGLFW BorderlessFullscreen "Yolo Ninja" hints $ \win -> do
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

    -- enable alpha blending
    glEnable GL_BLEND
    glBlendEquationSeparate GL_FUNC_ADD GL_FUNC_ADD
    glBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO
    {-
    -- Other blend modes are:
    -- SOURCE BLENDING
    glBlendFuncSeparate GL_ONE GL_ZERO GL_ONE GL_ZERO
    -- ADDITIVE BLENDING:
    glBlendFuncSeparate GL_ONE GL_ONE GL_ONE GL_ONE
    -- PREMULTIPLIED ALPHA
    glBlendFuncSeparate GL_ONE GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO
    -- ALPHA
    glBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO
    -}

    renderer <- newSpriteRenderer 100
    Dir.getCurrentDirectory >>= hPutStrLn stderr

    sampleTex <- textureFromFile "data/tex/alpha.png" True
    print sampleTex

    glPointSize 10
    glClearColor 0 0 0.5 1

    putStrLn "Begin Loop"
    untilM (GLFW.windowShouldClose win) $ do
      ratio <- readIORef ratioRef
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT

      Just t <- GLFW.getTime
      drawWithTexture renderer sampleTex
        [ SpriteVertex (V3 0 0 0) 0.5 0.5 0.5 (realToFrac t)]

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
  putStrLn "Extensions:"
  openGLExtensions >>= mapM_ (\str -> putStrLn $ "  " ++ str)

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
