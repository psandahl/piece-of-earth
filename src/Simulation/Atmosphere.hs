-- |
-- Module: Simulation.Atmosphere
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Very simple - non physics based - simulation of the atmosphere.
module Simulation.Atmosphere
    ( TimeOfDay (..)
    , ambientLight
    , sunLight
    ) where

import           BigE.Math                    (mkRotate33, toRadians)
import           Graphics.GL                  (GLfloat)
import           Graphics.Lights.AmbientLight (AmbientLight (..))
import qualified Graphics.Lights.AmbientLight as AmbientLight
import           Graphics.Lights.LightEmitter (LightEmitter (..))
import qualified Graphics.Lights.LightEmitter as LightEmitter
import           Linear                       (V3 (..), lerp, normalize, (!*),
                                               (^*))

-- | Time of day.
data TimeOfDay
    = Sunrise
    | Morning
    | Noon
    | Afternoon
    | Sunset
    | Night
    deriving Show

-- | Give the ambient light given the time of day.
ambientLight :: TimeOfDay -> AmbientLight
ambientLight timeOfDay =
    AmbientLight
        { AmbientLight.color = lightColor timeOfDay
        , AmbientLight.strength = ambience timeOfDay
        }

-- | Give the sun light (or moon light) given the time of day.
sunLight :: TimeOfDay -> LightEmitter
sunLight timeOfDay =
    LightEmitter
        { LightEmitter.position = rotatedSun (sunAngle timeOfDay) ^* distanceToTheSun
        , LightEmitter.color = lightColor timeOfDay
        }

-- | The distance to the sun. Just a model space distance far enough away so
-- that its light direction, more or less, will be the same for all fragments.
distanceToTheSun :: GLfloat
distanceToTheSun = 100000

-- | The ambient lightning strength.
ambience :: TimeOfDay -> GLfloat
ambience Night = 0.2
ambience _     = 0.1

-- | The angle in degrees of the sun (or moon during night) at the different
-- times of day.
sunAngle :: TimeOfDay -> GLfloat
sunAngle Sunrise   = -80
sunAngle Morning   = -45
sunAngle Noon      = 0
sunAngle Afternoon = 45
sunAngle Sunset    = 80
sunAngle Night     = -30

-- | Rotate the sun the given angle in degrees. Zenith is at angle 0. East
-- horizon at -90 and west horizon at 90. The resulting vector is a unit vector.
rotatedSun :: GLfloat -> V3 GLfloat
rotatedSun angle =
    let rotM = mkRotate33 (V3 0 0 1) $ toRadians angle
        vec = rotM !* V3 0 1 0
    in normalize vec

lightColor :: TimeOfDay -> V3 GLfloat
lightColor Sunrise   = sunrise
lightColor Morning   = lerp 0.5 sunrise noon
lightColor Noon      = noon
lightColor Afternoon = lerp 0.5 noon sunset
lightColor Sunset    = sunset
lightColor Night     = night

sunrise :: V3 GLfloat
sunrise = V3 (182 / 255) (126 / 255) (91 / 255)

noon :: V3 GLfloat
noon = V3 (192 / 255) (191 / 255) (173 / 255)

sunset :: V3 GLfloat
sunset = sunrise

night :: V3 GLfloat
night = V3 (30 / 255) (30 / 255) (100 / 255)
