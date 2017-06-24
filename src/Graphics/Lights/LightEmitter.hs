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
module Graphics.Light.LightEmitter
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

    , strength :: !GLfloat
      -- ^ The strength of the light.
    } deriving Show

-- | The collection of locations for a light emitter.
data LightEmitterLoc = LightEmitterLoc
    { positionLoc :: !Location
    , colorLoc    :: !Location
    , strengthLoc :: !Location
    } deriving Show

-- | For the given program the function is expecting there is a a struct type
-- uniform with the members; position, color and strength.
getLightEmitterLoc :: MonadIO m => Program -> String -> m LightEmitterLoc
getLightEmitterLoc program var =
    LightEmitterLoc <$> Program.getUniformLocation program (var ++ ".position")
                    <*> Program.getUniformLocation program (var ++ ".color")
                    <*> Program.getUniformLocation program (var ++ ".strength")

-- | Set the light emitter.
setLightEmitter :: MonadIO m => LightEmitter -> LightEmitterLoc -> m ()
setLightEmitter lightEmitter lightEmitterLoc = do
    setUniform (positionLoc lightEmitterLoc) (position lightEmitter)
    setUniform (colorLoc lightEmitterLoc) (color lightEmitter)
    setUniform (strengthLoc lightEmitterLoc) (strength lightEmitter)
