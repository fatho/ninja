{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Main where

import qualified Codec.Picture                   as JP
import           Control.Exception
import           Control.Monad
import           Control.Monad.IfElse
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Fixed                      (mod')
import           Data.IORef
import qualified Data.Vector.Storable            as VS
import           Foreign.C.String
import           Foreign.Ptr
import           Graphics.GL.Core33
import qualified Graphics.GL.Ext.ARB.DebugOutput as Dbg
import           Graphics.GL.Types
import qualified Graphics.UI.GLFW                as GLFW
import           Linear
import           System.Directory                as Dir
import           System.IO

import           Graphics.Ninja.GL
import           Graphics.Ninja.UI
import           Graphics.Ninja.GL2D.Sprite
import           Graphics.Ninja.Util

onDebugMessage :: GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
onDebugMessage _ _ _ _ len msg _ = do
  putStr "[OPENGL] "
  peekCStringLen (msg,fromIntegral len) >>= putStrLn

data GlfwEvent
  = GlfwFramebufferResized GLFW.Window Int Int
  deriving (Eq, Ord, Show)

class EventToCallback e r where
  asCallback :: e -> r

instance (b ~ ()) => EventToCallback GlfwEvent (IO b) where
  asCallback evt = do
    putStrLn "TODO: push event to queue or something like that"
    print evt

instance EventToCallback b r => EventToCallback (a -> b) (a -> r) where
  asCallback e x = asCallback (e x)

onFramebufferResized :: GLFW.Window -> Int -> Int -> IO ()
onFramebufferResized _ w h =
  glViewport 0 0 (fromIntegral w) (fromIntegral h)

main :: IO ()
main = withGLFW BorderlessFullscreen "Space" hints $ \win -> do
    -- setup debug callback
    -- when using a debug context, this yields some diagnostic messages useful for troubleshooting problems
    -- and for achieving standard compliance.
    callback <- mkGLDEBUGPROCARB onDebugMessage
    Dbg.glDebugMessageCallbackARB callback nullPtr

    --
    GLFW.setFramebufferSizeCallback win (Just onFramebufferResized)

    -- GLFW.setFramebufferSizeCallback win (Just $ asCallback $ GlfwFramebufferResized)

    printGLStats

    -- extensions that should be used if available according to <https://www.opengl.org/wiki/Common_Mistakes>
    requireExtensions ["GL_ARB_texture_storage"]
    ----------------------------------------------------------

    putStrLn "initialization"

    glClearColor 0 0 0.5 1

    putStrLn "Begin Loop"
    untilM (GLFW.windowShouldClose win) $ do
      GLFW.pollEvents

      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT

      GLFW.swapBuffers win

    putStrLn "Exit"
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
