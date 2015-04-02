{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Ninja.Storable.Generics where


import           Control.Applicative
import           Control.Exception
import           Control.Lens           hiding (coerce, from, to)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
import           Data.Monoid
import           Data.StateVar
import qualified Data.Vector.Storable   as VS
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear

import           Ninja.Util

class GStorable f where
  gSizeOf :: f p -> Int
  gAlignment :: f p -> Int
  gPeek :: Ptr (f p) -> IO (f p)
  gPoke :: Ptr (f p) -> f p -> IO ()

instance GStorable U1 where
  gSizeOf _ = 0
  gAlignment _ = 1
  gPeek _ = return U1
  gPoke _ U1 = return ()

padding :: (GStorable f, GStorable g) => f p -> g p -> Int
padding x y = (gAlignment y - gSizeOf x) `mod` gAlignment y

offset :: (GStorable f, GStorable g) => f p -> g p -> Int
offset x y = gSizeOf x + padding x y

instance (GStorable f, GStorable g) => GStorable (f :*: g) where
  gSizeOf _ = gSizeOf x + gSizeOf y + padding x y where
    x = undefined :: f p
    y = undefined :: g p
  gAlignment _ = lcm (gAlignment x) (gAlignment y) where
    x = undefined :: f p
    y = undefined :: g p
  gPeek ptr = (:*:) <$> gPeek (castPtr ptr) <*> gPeek (ptr `plusPtr` offset x y) where
    x = undefined :: f p
    y = undefined :: g p
  gPoke ptr (x :*: y) = do
    gPoke (castPtr ptr) x
    gPoke (ptr `plusPtr` offset x y) y

instance Storable c => GStorable (K1 i c) where
  gSizeOf _ = sizeOf (undefined :: c)
  gAlignment _ = alignment (undefined :: c)
  gPeek ptr = K1 <$> peek (castPtr ptr)
  gPoke ptr (K1 x) = poke (castPtr ptr) x

instance GStorable f => GStorable (M1 i t f) where
  gSizeOf _ = gSizeOf (undefined :: f p)
  gAlignment _ = gAlignment (undefined :: f p)
  gPeek ptr = M1 <$> gPeek (castPtr ptr)
  gPoke ptr (M1 x) = gPoke (castPtr ptr) x

genericSizeOf :: (Generic a, GStorable (Rep a)) => a -> Int
genericSizeOf x = gSizeOf (from x)
genericAlignment :: (Generic a, GStorable (Rep a)) => a -> Int
genericAlignment x = gAlignment (from x)
genericPeek :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
genericPeek ptr = to <$> gPeek (castPtr ptr)
genericPoke :: (Generic a, GStorable (Rep a)) => Ptr a -> a -> IO ()
genericPoke ptr x = gPoke (castPtr ptr) (from x)