{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | This module contains an implementation of the simplex noise algorithm for noise generation in two, three and four dimensions.
module Graphics.Ninja.Noise.Simplex (
  -- * Noise interface
  simplex2D,
  simplex3D,
  simplex4D,
  simplexLoop1D,
  simplexLoop2D,
  -- ** Parameters
  Permutation,
  defaultPerm, randomPerm,
  ) where

import           Control.Arrow
import           Data.Bits
import qualified Data.Vector.Unboxed   as V
import           Linear
import           System.Random
import           System.Random.Shuffle

import           Graphics.Ninja.Noise.Types

simplex2D :: Permutation -> Noise (V2 Double) Double
simplex2D perm = Noise (\(V2 x y) -> simplex2 perm x y)

simplex3D :: Permutation -> Noise (V3 Double) Double
simplex3D perm = Noise (\(V3 x y z) -> simplex3 perm x y z)

simplex4D :: Permutation -> Noise (V4 Double) Double
simplex4D perm = Noise (\(V4 x y z w) -> simplex4 perm x y z w)

simplexLoop1D :: Permutation -> Double -> Noise (V1 Double) Double
simplexLoop1D perm r = Noise go where
  go (V1 u) = let t = u * 2 * pi
                  x = r * cos t
                  y = r * sin t
              in simplex2 perm x y

simplexLoop2D :: Permutation -> Double -> Noise (V2 Double) Double
simplexLoop2D perm r = Noise go where
  go (V2 u v) = let s = u * 2 * pi
                    t = v * 2 * pi
                    x = r * cos s
                    y = r * cos s
                    z = r * sin t
                    w = r * sin t
                in simplex4 perm x y z w

-- | A permutation is an int vector with 512 elements, whereupon the second 256 elements
-- are the same as the first 256 to faciliate indexing (no bound checks needed).
-- For speed reasons, indexing of permutations uses `Data.Vector.Unboxed.unsafeIndex`,
-- so care must be taken not to provide less than 512 elements.
type Permutation = V.Vector Int

grad3 :: V.Vector (Double,Double,Double)
grad3 = V.fromList
  [ (1,1,0),(-1,1,0),(1,-1,0),(-1,-1,0)
  , (1,0,1),(-1,0,1),(1,0,-1),(-1,0,-1)
  , (0,1,1),(0,-1,1),(0,1,-1),(0,-1,-1)]

grad2 :: V.Vector (Double,Double)
grad2 = V.map (\(x,y,z) -> (x,y)) grad3

grad4 :: V.Vector (Double,Double,Double,Double)
grad4 = V.fromList
  [ (0,1,1,1), (0,1,1,-1), (0,1,-1,1), (0,1,-1,-1)
  , (0,-1,1,1), (0,-1,1,-1), (0,-1,-1,1), (0,-1,-1,-1)
  , (1,0,1,1), (1,0,1,-1), (1,0,-1,1), (1,0,-1,-1)
  , (-1,0,1,1), (-1,0,1,-1), (-1,0,-1,1), (-1,0,-1,-1)
  , (1,1,0,1), (1,1,0,-1), (1,-1,0,1), (1,-1,0,-1)
  , (-1,1,0,1), (-1,1,0,-1), (-1,-1,0,1), (-1,-1,0,-1)
  , (1,1,1,0), (1,1,-1,0), (1,-1,1,0), (1,-1,-1,0)
  , (-1,1,1,0), (-1,1,-1,0), (-1,-1,1,0), (-1,-1,-1,0)]

-- | A default permutation. Useful for testing.
defaultPerm :: Permutation
defaultPerm = V.fromList $ concat $ replicate 2 [151,160,137,91,90,15,
  131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
  190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
  88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
  77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
  102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
  135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
  5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
  223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
  129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
  251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
  49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
  138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180]

-- | Generates a random permutation based on the supplied random generator.
randomPerm :: RandomGen g => g -> (Permutation, g)
randomPerm g = (V.fromList $ concat $ replicate 2 $ shuffle [0..255] samples, g') where
  (samples, g') = f 1 256 g
  f i n g
    | i == n    = ([], g)
    | otherwise = let (x,g') = randomR (0,n-i) g in first (x:) $ f (i+1) n g'


-- | A lookup table to traverse the simplex around a given point in 4D.
--   Details can be found where this table is used, in the 4D noise method.
simplexLookup :: V.Vector (Int,Int,Int,Int)
simplexLookup = V.fromList [
  (0,1,2,3),(0,1,3,2),(0,0,0,0),(0,2,3,1),(0,0,0,0),(0,0,0,0),(0,0,0,0),(1,2,3,0),
  (0,2,1,3),(0,0,0,0),(0,3,1,2),(0,3,2,1),(0,0,0,0),(0,0,0,0),(0,0,0,0),(1,3,2,0),
  (0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),
  (1,2,0,3),(0,0,0,0),(1,3,0,2),(0,0,0,0),(0,0,0,0),(0,0,0,0),(2,3,0,1),(2,3,1,0),
  (1,0,2,3),(1,0,3,2),(0,0,0,0),(0,0,0,0),(0,0,0,0),(2,0,3,1),(0,0,0,0),(2,1,3,0),
  (0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),
  (2,0,1,3),(0,0,0,0),(0,0,0,0),(0,0,0,0),(3,0,1,2),(3,0,2,1),(0,0,0,0),(3,1,2,0),
  (2,1,0,3),(0,0,0,0),(0,0,0,0),(0,0,0,0),(3,1,0,2),(0,0,0,0),(3,2,0,1),(3,2,1,0)];

{-# INLINE dot2 #-}
dot2 :: (Double, Double) -> (Double, Double) -> Double
dot2 (i,j) (x,y) = i * x + j * y
{-# INLINE dot3 #-}
dot3 :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot3 (i,j,k) (x,y,z) = i * x + j * y + k * z
{-# INLINE dot4 #-}
dot4 :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> Double
dot4 (i,j,k,l) (x,y,z,w) = i * x + j * y + k * z + l * w

infixl 9 !
{-# INLINE (!) #-}
(!) :: V.Unbox a => V.Vector a -> Int -> a
(!) = V.unsafeIndex

-- | 2D simplex noise
simplex2 :: Permutation -> Double -> Double -> Double
simplex2 perm xin yin = let
  -- Skew the input space to determine which simplex cell we're in
  f2 = 0.5 * (sqrt 3.0 - 1.0)
  s = (xin + yin) * f2 -- Hairy factor for 2D
  i = floor (xin + s)
  j = floor (yin + s)
  g2 = (3.0 - sqrt 3.0) / 6.0
  t = (fromIntegral (i+j)) * g2
  x0' = fromIntegral i - t -- Unskew the cell origin back to (x,y) space
  y0' = fromIntegral j - t
  x0 = xin - x0' -- The x,y distances from the cell origin
  y0 = yin - y0'
  -- For the 2D case, the simplex shape is an equilateral triangle.
  -- Determine which simplex we are in.
  -- i, j: Offsets for second (middle) corner of simplex in (i,j) coords
  (i1,j1) = if x0 > y0
              then (1,0) -- lower triangle, XY order: (0,0)->(1,0)->(1,1)
              else (0,1) -- upper triangle, YX order: (0,0)->(0,1)->(1,1)
  x1 = x0 - fromIntegral i1 + g2 -- Offsets for middle corner in (x,y) unskewed coords
  y1 = y0 - fromIntegral j1 + g2
  x2 = x0 - 1.0 + 2.0 * g2 -- Offsets for last corner in (x,y) unskewed coords
  y2 = y0 - 1.0 + 2.0 * g2
  -- Work out the hashed gradient indices of the three simplex corners
  ii = i .&. 255
  jj = j .&. 255
  gi0 = perm!(ii + perm!jj) `rem` 12
  gi1 = perm!(ii+i1+perm!(jj+j1)) `rem` 12
  gi2 = perm!(ii+1+perm!(jj+1)) `rem` 12
  -- Calculate the contribution from the three corners
  t0 = 0.5 - x0^2 - y0^2
  n0 = if t0 < 0 then 0 else t0^4 * dot2 (grad2!gi0) (x0,y0)
  t1 = 0.5 - x1^2 - y1^2
  n1 = if t1 < 0 then 0 else t1^4 * dot2 (grad2!gi1) (x1,y1)
  t2 = 0.5 - x2^2 - y2^2
  n2 = if t2 < 0 then 0 else t2^4 * dot2 (grad2!gi2) (x2,y2)
  in 70 * (n0 + n1 + n2)

-- | 3D simplex noise
simplex3 :: Permutation -> Double -> Double -> Double -> Double
simplex3 perm xin yin zin = let
  -- Skew the input space to determine which simplex cell we're in
  f3 = 1.0/3.0;
  s = (xin+yin+zin)*f3; -- Very nice and simple skew factor for 3D
  i = floor (xin+s);
  j = floor (yin+s);
  k = floor (zin+s);
  g3 = 1.0/6.0 -- Very nice and simple unskew factor, too
  t = fromIntegral (i+j+k) * g3;
  x0' = fromIntegral i - t; -- Unskew the cell origin back to (x,y,z) space
  y0' = fromIntegral j - t;
  z0' = fromIntegral k - t;
  x0 = xin-x0'; -- The x,y,z distances from the cell origin
  y0 = yin-y0';
  z0 = zin-z0';
  -- For the 3D case, the simplex shape is a slightly irregular tetrahedron.
  -- Determine which simplex we are in.
  -- int i1, j1, k1; // Offsets for second corner of simplex in (i,j,k) coords
  -- int i2, j2, k2; // Offsets for third corner of simplex in (i,j,k) coords
  (i1,j1,k1,i2,j2,k2) =
    if x0 >= y0
      then if y0 >= z0
        then (1,0,0,1,1,0)
        else if x0 >= z0
          then (1,0,0,1,0,1) -- X Z Y order
          else (0,0,1,1,0,1) -- Z X Y order
      else if y0 < z0
        then (0,0,1,0,1,1) -- Z Y X order
        else if x0 < z0
          then (0,1,0,0,1,1) -- Y Z X order
          else (0,1,0,1,1,0) -- Y X T order
  x1 = x0 - fromIntegral i1 + g3;
  y1 = y0 - fromIntegral j1 + g3;
  z1 = z0 - fromIntegral k1 + g3;
  x2 = x0 - fromIntegral i2 + 2.0*g3; -- Offsets for third corner in (x,y,z) coords
  y2 = y0 - fromIntegral j2 + 2.0*g3;
  z2 = z0 - fromIntegral k2 + 2.0*g3;
  x3 = x0 - 1.0 + 3.0*g3; -- Offsets for last corner in (x,y,z) coords
  y3 = y0 - 1.0 + 3.0*g3;
  z3 = z0 - 1.0 + 3.0*g3;
 -- Work out the hashed gradient indices of the four simplex corners
  ii = i .&. 255;
  jj = j .&. 255;
  kk = k .&. 255;
  gi0 = perm!(ii+perm!(jj+perm!kk)) `rem` 12;
  gi1 = perm!(ii+i1+perm!(jj+j1+perm!(kk+k1))) `rem` 12;
  gi2 = perm!(ii+i2+perm!(jj+j2+perm!(kk+k2))) `rem` 12;
  gi3 = perm!(ii+1+perm!(jj+1+perm!(kk+1))) `rem` 12;
  -- Calculate the contribution from the four corners
  t0 = 0.6 - x0*x0 - y0*y0 - z0*z0;
  n0 = if t0 < 0 then 0 else t0^4 * dot3 (grad3!gi0) (x0,y0,z0)
  t1 = 0.6 - x1*x1 - y1*y1 - z1*z1;
  n1 = if t1 < 0 then 0 else t1^4 * dot3 (grad3!gi1) (x1,y1,z1)
  t2 = 0.6 - x2*x2 - y2*y2 - z2*z2;
  n2 = if t2 < 0 then 0 else t2^4 * dot3 (grad3!gi2) (x2,y2,z2)
  t3 = 0.6 - x3*x3 - y3*y3 - z3*z3;
  n3 = if t3 < 0 then 0 else t3^4 * dot3 (grad3!gi3) (x3,y3,z3)
  -- Add contributions from each corner to get the final noise value.
  -- The result is scaled to stay just inside [-1,1]
  in 32.0*(n0 + n1 + n2 + n3);


-- | 4D simplex noise
simplex4 :: Permutation -> Double -> Double -> Double -> Double -> Double
simplex4 perm x y z w = let
  -- The skewing and unskewing factors are hairy again for the 4D case
  f4 = (sqrt(5.0)-1.0)/4.0;
  g4 = (5.0-sqrt(5.0))/20.0;
  --double n0, n1, n2, n3, n4; // Noise contributions from the five corners
  -- Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
  s = (x + y + z + w) * f4; -- Factor for 4D skewing
  i = floor (x + s);
  j = floor (y + s);
  k = floor (z + s);
  l = floor (w + s);
  t = fromIntegral (i + j + k + l) * g4; -- Factor for 4D unskewing
  x0' = fromIntegral i - t; -- Unskew the cell origin back to (x,y,z,w) space
  y0' = fromIntegral j - t;
  z0' = fromIntegral k - t;
  w0' = fromIntegral l - t;
  x0 = x - x0'; -- The x,y,z,w distances from the cell origin
  y0 = y - y0';
  z0 = z - z0';
  w0 = w - w0';
  -- For the 4D case, the simplex is a 4D shape I won't even try to describe.
  -- To find out which of the 24 possible simplices we're in, we need to
  -- determine the magnitude ordering of x0, y0, z0 and w0.
  -- The method below is a good way of finding the ordering of x,y,z,w and
  -- then find the correct traversal order for the simplex we’re in.
  -- First, six pair-wise comparisons are performed between each possible pair
  -- of the four coordinates, and the results are used to add up binary bits
  -- for an integer index.
  c1 = if (x0 > y0) then 32 else 0;
  c2 = if (x0 > z0) then 16 else 0;
  c3 = if (y0 > z0) then 8 else 0;
  c4 = if (x0 > w0) then 4 else 0;
  c5 = if (y0 > w0) then 2 else 0;
  c6 = if (z0 > w0) then 1 else 0;
  c = c1 + c2 + c3 + c4 + c5 + c6;
  -- i1, j1, k1, l1; -- The integer offsets for the second simplex corner
  -- i2, j2, k2, l2; -- The integer offsets for the third simplex corner
  -- i3, j3, k3, l3; -- The integer offsets for the fourth simplex corner
  -- simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
  -- Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
  -- impossible. Only the 24 indices which have non-zero entries make any sense.
  -- We use a thresholding to set the coordinates in turn from the largest magnitude.
  -- The number 3 in the "simplex" array is at the position of the largest coordinate.
  {-# INLINE threshold #-}
  threshold t x = if x >= t then 1 else 0
  {-# INLINE tupCmp #-}
  tupCmp (i,j,k,l) t = let th = threshold t in (th i, th j, th k, th l)
  (i1, j1, k1, l1) = tupCmp (simplexLookup!c) 3
  -- The number 2 in the "simplexLookup" array is at the second largest coordinate.
  (i2, j2, k2, l2) = tupCmp (simplexLookup!c) 2
  -- The number 1 in the "simplexLookup" array is at the second smallest coordinate.
  (i3, j3, k3, l3) = tupCmp (simplexLookup!c) 1
  -- The fifth corner has all coordinate offsets = 1, so no need to look that up.
  x1 = x0 - fromIntegral i1 + g4; -- Offsets for second corner in (x,y,z,w) coords
  y1 = y0 - fromIntegral j1 + g4;
  z1 = z0 - fromIntegral k1 + g4;
  w1 = w0 - fromIntegral l1 + g4;
  x2 = x0 - fromIntegral i2 + 2.0*g4; -- Offsets for third corner in (x,y,z,w) coords
  y2 = y0 - fromIntegral j2 + 2.0*g4;
  z2 = z0 - fromIntegral k2 + 2.0*g4;
  w2 = w0 - fromIntegral l2 + 2.0*g4;
  x3 = x0 - fromIntegral i3 + 3.0*g4; -- Offsets for fourth corner in (x,y,z,w) coords
  y3 = y0 - fromIntegral j3 + 3.0*g4;
  z3 = z0 - fromIntegral k3 + 3.0*g4;
  w3 = w0 - fromIntegral l3 + 3.0*g4;
  x4 = x0 - 1.0 + 4.0*g4; -- Offsets for last corner in (x,y,z,w) coords
  y4 = y0 - 1.0 + 4.0*g4;
  z4 = z0 - 1.0 + 4.0*g4;
  w4 = w0 - 1.0 + 4.0*g4;
  -- Work out the hashed gradient indices of the five simplex corners
  ii = i .&. 255;
  jj = j .&. 255;
  kk = k .&. 255;
  ll = l .&. 255;
  gi0 = perm!(ii+perm!(jj+perm!(kk+perm!(ll)))) `rem` 32;
  gi1 = perm!(ii+i1+perm!(jj+j1+perm!(kk+k1+perm!(ll+l1)))) `rem` 32;
  gi2 = perm!(ii+i2+perm!(jj+j2+perm!(kk+k2+perm!(ll+l2)))) `rem` 32;
  gi3 = perm!(ii+i3+perm!(jj+j3+perm!(kk+k3+perm!(ll+l3)))) `rem` 32;
  gi4 = perm!(ii+1+perm!(jj+1+perm!(kk+1+perm!(ll+1)))) `rem` 32;
  -- Calculate the contribution from the five corners
  t0 = 0.6 - x0*x0 - y0*y0 - z0*z0 - w0*w0;
  n0 = if t0 < 0 then 0 else t0^4 * dot4 (grad4!gi0) (x0,y0,z0,w0)
  t1 = 0.6 - x1*x1 - y1*y1 - z1*z1 - w1*w1;
  n1 = if t1 < 0 then 0 else t1^4 * dot4 (grad4!gi1) (x1,y1,z1,w1)
  t2 = 0.6 - x2*x2 - y2*y2 - z2*z2 - w2*w2;
  n2 = if t2 < 0 then 0 else t2^4 * dot4 (grad4!gi2) (x2,y2,z2,w2)
  t3 = 0.6 - x3*x3 - y3*y3 - z3*z3 - w3*w3;
  n3 = if t3 < 0 then 0 else t3^4 * dot4 (grad4!gi3) (x3,y3,z3,w3)
  t4 = 0.6 - x4*x4 - y4*y4 - z4*z4 - w4*w4;
  n4 = if t4 < 0 then 0 else t4^4 * dot4 (grad4!gi4) (x4,y4,z4,w4)
  -- Sum up and scale the result to cover the range [-1,1]
  in 27.0 * (n0 + n1 + n2 + n3 + n4);
