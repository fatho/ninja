{-# LANGUAGE TemplateHaskell #-}
module Graphics.Ninja.Particles where

import qualified Codec.Picture          as JP
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.Coerce
import           Data.Default.Class
import           Data.StateVar
import qualified Data.Vector.Storable   as VS
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear
import qualified Data.Map as M

import           Graphics.Ninja.GL

type TileCoord = V2 (V2 Float)
type SpriteId  = Int

data ParticleCfg = ParticleCfg
  { _particleCfgTexture :: Texture
  }

data ParticleSystem t = ParticleSystem
  { _particleSystemConfig :: ParticleCfg
  , _particleSystemParts  :: [Particle t]
  }

data Particle t = Particle
  { _particleLifetime :: t
  , _particleSpeed    :: V2 Float
  , _particlePosition :: V2 Float
  , _particleDecay    :: Float
  }

makeLenses ''ParticleCfg
makeLenses ''ParticleSystem
makeLenses ''Particle

newParticleSystem :: Texture -> ParticleSystem t
newParticleSystem tex = ParticleSystem
  { _particleSystemConfig = ParticleCfg tex
  , _particleSystemParts  = []
  }

updateSystem :: (Num t, Ord t) => t -> [Particle t] -> ParticleSystem t -> ParticleSystem t
updateSystem dt new sys = sys & particleSystemParts %~ filter ((<= 0) . view particleLifetime) . (new++) . map upd where
  upd part = part & particleLifetime -~ dt
                  & particlePosition +~ view particleSpeed part
                  & particleSpeed    %~ (^* view particleDecay part)
