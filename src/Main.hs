{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IfElse
import qualified Graphics.UI.GLFW       as GLFW
import           Graphics.GL.Core33
import Foreign.Ptr
import Graphics.GL.Types

import Ninja.GL

testBuffer :: [GLfloat]
testBuffer =
  [ -1.0, -1.0, 0.0
  ,  1.0, -1.0, 0.0
  ,  0.0,  1.0, 0.0
  ]

main :: IO ()
main = withGLFW BorderlessFullscreen "Yolo Ninja" hints $ \win -> do
    vao  <- gen1 :: IO VAO
    vbuf <- gen1 :: IO (Buffer GLfloat)
    boundVertexArray $= vao
    boundBuffer ArrayBuffer $= vbuf
    bufferData ArrayBuffer StaticDraw testBuffer
    untilM (GLFW.windowShouldClose win) $ do
      (fh, fw) <- GLFW.getFramebufferSize win
      let ratio = fromIntegral fw / fromIntegral fh
      -- glViewport 0 0 (fromIntegral fw) (fromIntegral fh)
      -- glClear GL_COLOR_BUFFER_BIT

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
