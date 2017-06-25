-- |
-- Module: Graphics.Lights
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Lights
    ( sunAtDawn
    , ambientAtDawn
    ) where

import           BigE.Math                    (mkRotate33, toRadians)
import           Graphics.GL                  (GLfloat)
import           Graphics.Lights.AmbientLight (AmbientLight (..))
import qualified Graphics.Lights.AmbientLight as AmbientLight
import           Graphics.Lights.LightEmitter (LightEmitter (..))
import qualified Graphics.Lights.LightEmitter as LightEmitter
import           Linear                       (V3 (..), (!*))

-- | A 'LightEmitter' value corresponding to the sun at dawn.
sunAtDawn :: LightEmitter
sunAtDawn =
    LightEmitter
        { LightEmitter.position = sunPositionAtDawn
        , LightEmitter.color = dawnSunLight
        }

-- | An 'AmbientLight' value corresponding to the ambient light at dawn.
ambientAtDawn :: AmbientLight
ambientAtDawn =
    AmbientLight
        { AmbientLight.color = dawnSunLight
        , AmbientLight.strength = 0.1
        }

dawnSunLight :: V3 GLfloat
dawnSunLight = V3 (252 / 255) (209 / 255) (77 / 255)

sunDistance :: GLfloat
sunDistance = 15000

sunPositionAtNoon :: V3 GLfloat
sunPositionAtNoon = V3 0 sunDistance 0

sunPositionAtDawn :: V3 GLfloat
sunPositionAtDawn = rotatedSun (-70)

--sunPositionAtDusk :: V3 GLfloat
--sunPositionAtDusk = rotatedSun 70

rotatedSun :: GLfloat -> V3 GLfloat
rotatedSun degrees =
    let rotM = mkRotate33 (V3 0 0 1) (toRadians degrees)
    in rotM !* sunPositionAtNoon
