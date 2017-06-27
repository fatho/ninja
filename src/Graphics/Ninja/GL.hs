{-# LANGUAGE PatternSynonyms #-}
module Graphics.Ninja.GL
  ( module NGL
  , module SV
  ) where

import           Graphics.Ninja.GL.Blending     as NGL
import           Graphics.Ninja.GL.Buffer       as NGL
import           Graphics.Ninja.GL.Exception    as NGL
import           Graphics.Ninja.GL.Extensions   as NGL
import           Graphics.Ninja.GL.Object       as NGL
import           Graphics.Ninja.GL.Program      as NGL
import           Graphics.Ninja.GL.Shader       as NGL
import           Graphics.Ninja.GL.Texture      as NGL
import           Graphics.Ninja.GL.Types        as NGL
import           Graphics.Ninja.GL.Uniform      as NGL
import           Graphics.Ninja.GL.VertexArray  as NGL
import           Graphics.Ninja.GL.VertexAttrib as NGL

import           Data.StateVar         as SV
