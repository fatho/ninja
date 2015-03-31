{-# LANGUAGE PatternSynonyms #-}
module Ninja.GL
  ( module NGL
  , module SV
  ) where

import           Ninja.GL.Buffer  as NGL
import           Ninja.GL.Object  as NGL
import           Ninja.GL.Program as NGL
import           Ninja.GL.Shader  as NGL
import           Ninja.GL.VAO     as NGL

import           Data.StateVar    as SV
