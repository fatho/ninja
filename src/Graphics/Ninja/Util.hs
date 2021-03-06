{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Graphics.Ninja.Util where

import qualified Codec.Picture                as JP
import qualified Codec.Picture.Types          as JP
import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.StateVar
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Ninja.Types

-- | Modifies a StateVar locally.
withVar :: (ControlIO m) => StateVar a -> a -> m b -> m b
withVar var val act = do
  oldVal <- get var
  finally
    (var $= val >> act)
    (var $= oldVal)

-- | Allocates memory and passes the pointer to the function, returning the contents afterwards.
withPtrOut :: (ControlIO m, Storable a) => (Ptr a -> m ()) -> m a
withPtrOut f = liftBaseOp alloca $ liftM2 (>>) f (liftBase . peek)

-- | Allocates memory, copies the supplied value to that location and passes the pointer to the given function.
withPtrIn :: (ControlIO m, Storable a) => a -> (Ptr a -> m b) -> m b
withPtrIn v f = liftBaseOp alloca $ liftM2 (>>) (liftBase . flip poke v) f

-- | Combines two StateVars into one.
combineStateVars :: ((a,b) -> c) -> (c -> (a,b)) -> StateVar a -> StateVar b -> StateVar c
combineStateVars from to sta stb = makeStateVar g s where
  g = curry from <$> get sta <*> get stb
  s c = let (a,b) = to c in (sta $= a) >> (stb $= b)

-- | Flips raw texture data.
flipRawTextureData :: Int -> Int -> Ptr a -> IO ()
flipRawTextureData stride height ptr =
    allocaBytes stride $ \tmpPtr ->
      forM_ [0..height `div` 2 - 1] (swap tmpPtr)
  where
    swap tmpPtr y = do
      let y' = height - 1 - y
      copyBytes tmpPtr (ptr `plusPtr` (stride * y)) stride
      copyBytes (ptr `plusPtr` (stride * y)) (ptr `plusPtr` (stride * y')) stride
      copyBytes (ptr `plusPtr` (stride * y')) tmpPtr stride

-- | Flips an image by creating a copy.
flipImage :: forall px. (Storable (JP.PixelBaseComponent px), JP.Pixel px) => JP.Image px -> JP.Image px
flipImage img = JP.pixelMapXY vswap img where
  height = JP.imageHeight img
  vswap x y _ = JP.pixelAt img x (height - 1 - y)
