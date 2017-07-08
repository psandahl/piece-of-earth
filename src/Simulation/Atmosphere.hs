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
    , SkyGradient (..)
    , ambientLight
    , sunLight
    , skyGradient
    , fog
    ) where

import           BigE.Math                    (mkRotate33, toRadians)
import           Graphics.Fog                 (Fog (..))
import qualified Graphics.Fog                 as Fog
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

-- | Colors for a sky gradient.
data SkyGradient = SkyGradient
    { sky     :: !(V3 GLfloat)
    , horizon :: !(V3 GLfloat)
    } deriving Show

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

-- | Give the sky gradient given the time of day.
skyGradient :: TimeOfDay -> SkyGradient
skyGradient timeOfDay =
    case timeOfDay of
        Sunrise   -> sunriseSky
        Morning   -> morningSky
        Noon      -> daylightSky
        Afternoon -> morningSky
        Sunset    -> sunriseSky
        Night     -> nightSky
    where
        sunriseSky :: SkyGradient
        sunriseSky =
            SkyGradient
                { sky = V3 (70 / 255) (106 / 255) (200 / 255)
                , horizon = V3 (246 / 255) (176 / 255) (133 / 255)
                }

        morningSky :: SkyGradient
        morningSky =
            SkyGradient
                { sky = V3 0 (5 / 255) (25 / 255)
                , horizon = V3 (71 / 255) (118 / 255) (172 / 255)
                }

        daylightSky :: SkyGradient
        daylightSky =
            SkyGradient
                { sky = V3 (12 / 255) (94 / 255) (170 / 255)
                , horizon = V3 (170 / 255) (204 / 255) (204 / 255)
                }

        nightSky :: SkyGradient
        nightSky =
            SkyGradient
                { sky = V3 0 (5 / 255) (25 / 255)
                , horizon = V3 (12 / 255) (35 / 255) (87 / 255)
                }

-- | Fog. Not dependent on time of day.
fog :: Fog
fog = Fog  { Fog.color = V3 0.5 0.5 0.5
           , Fog.fogStart = 0
           , Fog.fogEnd = 1000
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
noon = V3 (175 / 255) (175 / 255) (175 / 255)

sunset :: V3 GLfloat
sunset = sunrise

night :: V3 GLfloat
night = V3 (40 / 255) (40 / 255) (60 / 255)
