{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module contains functions to create gradients.
module Graphics.Ninja.Noise.Gradient
       ( linearGradient
       , sphericalGradient
       , projectSegment
       )
       where

import           Graphics.Ninja.Noise.Types

import           Linear

sphericalGradient :: (Metric f, Floating a) => f a -> (a -> a) -> Noise (f a) a
sphericalGradient center distVal = Noise $ distVal . distance center

linearGradient :: (Metric f, Fractional a) => f a -> f a -> a -> a -> Noise (f a) a
linearGradient from to low high = Noise go where
  go pt = let t = projectSegment from to pt
          in (t - 1) * low + t * high

-- | Implements orthogonal projection of a point on a line in n-dimensional space.
projectSegment :: (Metric f, Fractional a) => f a -> f a -> f a -> a
projectSegment xs ys vs = dot dd dv / quadrance dd where
  dd = ys ^-^ xs
  dv = vs ^-^ xs
