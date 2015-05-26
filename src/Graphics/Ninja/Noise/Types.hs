{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Ninja.Noise.Types where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Prelude             hiding (id, (.))

-- | Noise is just a fancy name for function. Usually @a@ is a point in space and @b@ is the noise value at that point.
newtype Noise a b = Noise { noiseAt :: a -> b }
                    deriving (Functor, Applicative, Monad, Arrow, Category, ArrowChoice, ArrowApply, ArrowLoop)

instance Num b => Num (Noise a b) where
  f + g = liftA2 (+) f g
  f - g = liftA2 (-) f g
  f * g = liftA2 (*) f g
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Noise a b) where
  fromRational = pure . fromRational
  recip = fmap recip

instance Floating b => Floating (Noise a b) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 (**)
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
