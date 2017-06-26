-- |
-- Module: Graphics.Lights
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Lights
    ( sunAtDawn
    , sunAtNoon
    , ambientAtDawn
    , ambientAtNoon
    ) where

import           BigE.Math                    (mkRotate33, toRadians)
import           Graphics.GL                  (GLfloat)
import           Graphics.Lights.AmbientLight (AmbientLight (..))
import qualified Graphics.Lights.AmbientLight as AmbientLight
import           Graphics.Lights.LightEmitter (LightEmitter (..))
import qualified Graphics.Lights.LightEmitter as LightEmitter
import           Linear                       (V3 (..), normalize, (!*), (^*))

-- | A 'LightEmitter' value corresponding to the sun at dawn.
sunAtDawn :: LightEmitter
sunAtDawn =
    LightEmitter
        { LightEmitter.position = sunPositionAtDawn
        , LightEmitter.color = dawnSunLight
        }

sunAtNoon :: LightEmitter
sunAtNoon =
    LightEmitter
        { LightEmitter.position = sunPositionAtNoon
        , LightEmitter.color = noonSunLight
        }

-- | An 'AmbientLight' value corresponding to the ambient light at dawn.
ambientAtDawn :: AmbientLight
ambientAtDawn =
    AmbientLight
        { AmbientLight.color = dawnSunLight
        , AmbientLight.strength = 0.1
        }

ambientAtNoon :: AmbientLight
ambientAtNoon =
    AmbientLight
        { AmbientLight.color = noonSunLight
        , AmbientLight.strength = 0.15
        }

dawnSunLight :: V3 GLfloat
dawnSunLight = V3 (182 / 255) (126 / 255) (91 / 255)

noonSunLight :: V3 GLfloat
noonSunLight = V3 (192 / 255) (191 / 255) (173 / 255)

sunDistance :: GLfloat
sunDistance = 15000

sunPositionAtNoon :: V3 GLfloat
sunPositionAtNoon = rotateSun 0 ^* sunDistance

sunPositionAtDawn :: V3 GLfloat
sunPositionAtDawn = rotateSun (-70) ^* sunDistance

--sunPositionAtDusk :: V3 GLfloat
--sunPositionAtDusk = rotatedSun 70

-- | Zero degrees is at noon, -90 degrees is at eastern horizon and 90 degrees
-- is at western horizon. The vector given is a unit vector.
rotateSun :: GLfloat -> V3 GLfloat
rotateSun degrees =
    let rotM = mkRotate33 (V3 0 0 1) (toRadians degrees)
        rotated = rotM !* V3 0 1 0
    in normalize rotated
