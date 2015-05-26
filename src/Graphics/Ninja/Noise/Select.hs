{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}

-- | Implements selection between different noise modules based on control values.
module Graphics.Ninja.Noise.Select (select, noiseSelect) where

import           Control.Monad
import           Graphics.Ninja.Math.Interpolation
import           Graphics.Ninja.Noise.Types

-- | Selects either `low` or `high`, based on whether `control` is below or above `threshold`
-- If control is in range of `threshold` +- `falloff`, quintic interpolation is performed between `low` and `high`.
noiseSelect :: (Ord b, Fractional b)
               => Noise (d a) b
               -> Noise (d a) b
               -> Noise (d a) b
               -> Noise (d a) b
               -> Noise (d a) b
               -> Noise (d a) b
noiseSelect = liftM5 select

-- | Selects either `low` or `high`, based on whether `control` is below or above `threshold`
-- If control is in range of `threshold` +- `falloff`, quintic interpolation is performed between `low` and `high`.
select :: (Fractional a, Ord a)
       => a -- ^ `low`
       -> a -- ^ `high`
       -> a -- ^ `threshold`
       -> a -- ^ `falloff`
       -> a -- ^ `control`
       -> a
select low high threshold falloff control
  | falloff > 0 =
    let lower = threshold - falloff
        upper = threshold + falloff
    in if | control < lower -> low
          | control > upper -> high
          | otherwise -> lerp (quinticBlend $ (control - lower) / (upper - lower)) low high
  | otherwise = if control <= threshold then low else high
