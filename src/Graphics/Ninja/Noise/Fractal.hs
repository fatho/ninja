{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module implements generation of fractals based on other noise modules.
module Graphics.Ninja.Noise.Fractal
       ( fBm
       ) where

import           Graphics.Ninja.Noise.Types

-- | Fractal brownian motion
-- Input value range is assumed `[-1..1]`, in that case, the output value range also is `[-1..1]`
fBm :: (Num (d a), Fractional b) => Int -> d a -> d a -> b -> Noise (d a) b -> Noise (d a) b
fBm octaves frequency lacunarity persistence source = Noise go where
  go x = let values = map (\ (f,a) -> a * noiseAt source (x * f)) $ zip freqs ampls
         in sum values / expsum
  -- frequencies and amplitutes of the octaves
  freqs = take octaves $ iterate (* lacunarity) frequency
  ampls = take octaves $ iterate (* persistence) 1
  -- maximum value, needed for scaling
  expsum = (persistence^octaves-1)/(persistence-1)
