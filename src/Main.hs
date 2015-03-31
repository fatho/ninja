{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IfElse
import           Data.Bits
import qualified Data.ByteString      as BS
import           Foreign.Ptr
import           Graphics.GL.Core33
import           Graphics.GL.Types
import qualified Graphics.UI.GLFW     as GLFW
import           Linear
import           System.Directory     as Dir

import           Ninja.GL

testBuffer :: [V3 GLfloat]
testBuffer =
  [ V3 (-1.0) (-1.0) 0.0
  , V3   1.0  (-1.0) 0.0
  , V3   0.0    1.0  0.0
  ]

loadShaders :: FilePath -> FilePath -> IO Program
loadShaders vertPath fragPath = do
  putStrLn "Loading source from files"
  vertSource <- BS.readFile vertPath
  fragSource <- BS.readFile fragPath
  putStrLn "Creating shader objects"
  vertShader <- gen1 :: IO (Shader VertexShader)
  fragShader <- gen1 :: IO (Shader FragmentShader)
  putStrLn "Feeding source to shaders"
  shaderSourceBytes vertShader $= vertSource
  shaderSourceBytes fragShader $= fragSource
  putStrLn "Compiling vertex shader"
  (success, msg) <- compileShader vertShader
  putStrLn msg
  unless success $ ioError $ userError "failed to compile vertex shader"
  putStrLn "Compiling fragment shader"
  (success, msg) <- compileShader fragShader
  putStrLn msg
  unless success $ ioError $ userError "failed to compile fragment shader"
  putStrLn "Creating program"
  prog <- gen1
  attachShader prog vertShader
  attachShader prog fragShader
  putStrLn "Linking program"
  (success, msg) <- linkProgram prog
  putStrLn msg
  unless success $ ioError $ userError "failed to link program"
  attachedShaders prog $= []
  delete1 vertShader
  delete1 fragShader
  return prog

main :: IO ()
main = withGLFW BorderlessFullscreen "Yolo Ninja" hints $ \win -> do
    vao  <- gen1 :: IO VAO
    vbuf <- gen1 :: IO (Buffer GLfloat)
    boundVertexArray $= vao
    boundBuffer ArrayBuffer $= vbuf
    bufferData ArrayBuffer StaticDraw testBuffer

    prog <- loadShaders "shaders/vert.glsl" "shaders/frag.glsl"
    fillColorUniform <- uniformLocation prog "fill_color"

    untilM (GLFW.windowShouldClose win) $ do
      (fw, fh) <- GLFW.getFramebufferSize win
      let ratio = fromIntegral fw / fromIntegral fh
      glViewport 0 0 (fromIntegral fw) (fromIntegral fh)
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

      Just time <- GLFW.getTime

      withProgram prog $ do
          uniform fillColorUniform (V3 (realToFrac $ (sin (2 * time) + 1) / 2) (realToFrac $ (sin time + 1) / 2) (realToFrac $ (sin (3 * time) + 1) / 2) :: V3 Float)
          glEnableVertexAttribArray 0
          boundBuffer ArrayBuffer $= vbuf
          glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
          glDrawArrays GL_TRIANGLES 0 3
          glDisableVertexAttribArray 0

      GLFW.swapBuffers win
      GLFW.pollEvents
    delete1 vbuf
    delete1 vao
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
