-- |
-- Module: Graphics.Lights.LightEmitter
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- LightEmitter is a general source of light. The module is exporting two
-- records, one is the light itself and the other is the record of locations
-- tied to a program.
module Graphics.Lights.LightEmitter
    ( LightEmitter (..)
    , LightEmitterLoc
    , getLightEmitterLoc
    , setLightEmitter
    ) where

import qualified BigE.Program           as Program
import           BigE.Types             (Location, Program, setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat)
import           Linear                 (V3)

-- | The components of a light emitter.
data LightEmitter = LightEmitter
    { position :: !(V3 GLfloat)
      -- ^ The position of the light.

    , color    :: !(V3 GLfloat)
      -- ^ The color of the light.
    } deriving Show

-- | The collection of locations for a light emitter.
data LightEmitterLoc = LightEmitterLoc
    { positionLoc :: !Location
    , colorLoc    :: !Location
    } deriving Show

-- | For the given program the function is expecting there is a a struct type
-- uniform with the members; position and color.
getLightEmitterLoc :: MonadIO m => Program -> String -> m LightEmitterLoc
getLightEmitterLoc program var =
    LightEmitterLoc <$> Program.getUniformLocation program (var ++ ".position")
                    <*> Program.getUniformLocation program (var ++ ".color")

-- | Set the light emitter.
setLightEmitter :: MonadIO m => LightEmitterLoc -> LightEmitter -> m ()
setLightEmitter lightEmitterLoc lightEmitter = do
    setUniform (positionLoc lightEmitterLoc) (position lightEmitter)
    setUniform (colorLoc lightEmitterLoc) (color lightEmitter)
