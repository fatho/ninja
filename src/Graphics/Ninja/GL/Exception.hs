{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Ninja.GL.Exception where

import Control.Exception
import Data.Typeable
import Data.Data

-- | The type of general OpenGL exceptions.
data GLException
  = ObjectCreationFailed String
  deriving (Eq, Ord, Show, Typeable, Data)

-- | Exception type thrown by the shader module.
data ShaderException
  = ShaderCompileError String
  -- ^ A shader failed to compile with some error message.
  | ProgramLinkError String
  -- ^ The program failed to link with some error message.
  deriving (Eq, Ord, Show, Typeable, Data)

-- | Exception thrown when required extensions are not available.
data ExtensionsUnavailable = ExtensionsUnavailable [String]
  deriving (Eq, Ord, Show, Typeable, Data)

instance Exception ShaderException
instance Exception GLException
instance Exception ExtensionsUnavailable
