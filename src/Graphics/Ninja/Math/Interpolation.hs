module Graphics.Ninja.Math.Interpolation where

-- | Quintic blend between 0 and 1 by the formula f(t) = 6*t^5-15*t^4+10*t^3 (f(0) = 0 and f(1) = 1)
-- This function has the special property, that f'(0) = 0 = f'(1) and f''(0) = 0 = f''(1)
-- f'(t) = 30*t^4-60*t^3+30*t^2
-- f''(t) = 120*t^3-180*t^2+60*t
quinticBlend :: Num a => a -> a
quinticBlend t = t*t*t*(t*(t*6-15)+10)

-- | The hermite blending function f(t) = 3*t^2-2*t^3 (f(0) = 0 and f(1) = 1)
-- The first derivative at 0 and 1 is zero, but the second derivative is not because of a constant term.
-- f'(t) = 6*t-6*t^2
-- f''(t) = 6 - 12 * t^2
hermiteBlend :: Num a => a -> a
hermiteBlend t = t*t*(3-2*t)

-- | Linear interpolation between `a` and `b` controlled by `t`
-- lerp(t,a,b) = (1-t)*a + t*b
lerp :: Num a => a -> a -> a -> a
lerp t a b = (1 - t) * a + t * b
