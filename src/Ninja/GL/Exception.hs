{-# LANGUAGE DeriveDataTypeable #-}
module Ninja.GL.Exception where

import Control.Exception
import Data.Typeable
import Data.Data

data GLException
  = ObjectCreationFailed String
  deriving (Eq, Ord, Show, Typeable, Data)

-- | Exception type thrown by the shader module.
data ShaderException 
  = ShaderCompileError String 
  | ProgramLinkError String
  deriving (Eq, Ord, Show, Typeable, Data)

instance Exception GLException
instance Exception ShaderException