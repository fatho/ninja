module Graphics.Ninja.GL.Extensions
  ( openGLExtensions
  , requireExtensions
  ) where

import           Control.Exception
import           Control.Monad
import Data.List ((\\))
import           Foreign.C.String
import           Foreign.Ptr
import           Graphics.GL.Core33

import           Graphics.Ninja.GL.Exception
import           Graphics.Ninja.Util

-- * OpenGL Extensions

-- | Checks whether the given extensions are available and fails with an exception otherwise.
requireExtensions :: [String] -> IO ()
requireExtensions required = do
  available <- openGLExtensions
  let missing = required \\ available
  when (not $ null missing) $ do
    throwIO (ExtensionsUnavailable missing)

-- | Returns a list of all available OpenGL extensions.
openGLExtensions :: IO [String]
openGLExtensions = do
  num <- withPtrOut $ glGetIntegerv GL_NUM_EXTENSIONS
  mapM (glGetStringi GL_EXTENSIONS . fromIntegral >=> peekCString . castPtr) [0..num-1]
