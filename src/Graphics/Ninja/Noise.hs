module Graphics.Ninja.Noise (module Noise, controlNoise) where

import           Graphics.Ninja.Noise.Fractal  as Noise
import           Graphics.Ninja.Noise.Gradient as Noise
import           Graphics.Ninja.Noise.Select   as Noise
import           Graphics.Ninja.Noise.Simplex  as Noise
import           Graphics.Ninja.Noise.Types    as Noise

controlNoise :: (a -> a -> a) -> Noise a a -> Noise a b -> Noise a b
controlNoise f na nb = Noise $ \x -> noiseAt nb (f x (noiseAt na x))
