-- |
-- Module: Graphics.Camera
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Camera
    ( init
    , animate
    ) where

import           BigE.Math        (mkRotate33, toRadians)
import           BigE.Runtime     (Render, frameDuration, getAppStateUnsafe)
import           BigE.Util        (clamp)
import           Engine.State     (State (terrain, userInput))
import           Graphics.GL      (GLfloat)
import qualified Graphics.Terrain as Terrain
import           Graphics.Types   (Camera (..), Terrain, UserInput (..))
import           Linear           (V3 (..), lookAt, normalize, zero, (!*),
                                   (!*!), (*^))
import           Prelude          hiding (init)

-- | Initialze the 'Camera'.
init :: Camera
init =
    Camera
        { viewMatrix = lookAt (V3 0 1 0) (V3 2 0 2) up
        , cameraPosition = V3 0 1 0
        , yaw = toRadians (-90)
        , pitch = 0
        }

-- | Animate the 'Camera'. Always perform all calculations, simpler code and
-- deterministic timing every frame.
animate :: Camera -> Render State Camera
animate camera = do
    -- Get some useful stuff from the runtime.
    state <- getAppStateUnsafe
    let userInp = userInput state
        terrain' = terrain state

    duration <- realToFrac <$> frameDuration

    -- Calculate the camera yaw (y-axis rotation) and the unit vector telling
    -- the direction in which the camera is heading.
    let yaw' = yaw camera + leftYaw duration userInp + rightYaw duration userInp
        yawMatrix = mkRotate33 up yaw'
        heading = normalize $ yawMatrix !* ahead

    -- From the heading calculate a new camera position.
    let forwardVector = forward duration heading userInp
        backwardVector = backward duration heading userInp
        cameraPosition' = cameraPosition camera + forwardVector + backwardVector

    let cameraPositionWithHeight = cameraHeight duration cameraPosition'
                                                terrain' userInp

    -- Calculate the pitch, thespot where the camera is looking and then
    -- make the new camera view matrix.
    let pitch' = normalizePitch $ pitch camera +
                                  upPitch duration userInp +
                                  downPitch duration userInp
        pitchMatrix = mkRotate33 (V3 1 0 0) pitch'
        pitchVector = (yawMatrix !*! pitchMatrix) !* ahead
        viewSpot = cameraPositionWithHeight + pitchVector
        viewMatrix' = lookAt cameraPositionWithHeight viewSpot up

    -- Give back the new camera.
    return camera { viewMatrix = viewMatrix'
                  , cameraPosition = cameraPositionWithHeight
                  , yaw = yaw'
                  , pitch = pitch'
                  }

-- | Make a left rotation angle proportional to the rotation speed.
leftYaw :: GLfloat -> UserInput -> GLfloat
leftYaw duration userInp
    | turnLeft userInp = duration * yawSpeed
    | otherwise = 0

-- | Make a right rotation angle proportional to the rotation speed.
rightYaw :: GLfloat -> UserInput -> GLfloat
rightYaw duration userInp
    | turnRight userInp = -(duration * yawSpeed)
    | otherwise = 0

-- | Calculate a forward motion vector from the heading, proportional to
-- frame duration and moving speed.
forward :: GLfloat -> V3 GLfloat -> UserInput -> V3 GLfloat
forward duration heading' userInp
    | goForward userInp =
        (duration * movingSpeed) *^ heading'
    | otherwise = zero

-- | Calculate a backward motion vector from the heading, proportional to
-- frame duration and moving speed.
backward :: GLfloat -> V3 GLfloat -> UserInput -> V3 GLfloat
backward duration heading' userInp
    | goBackward userInp =
        (-(duration * movingSpeed)) *^ heading'
    | otherwise = zero

-- | Make a pitch up angle proportional to the frame duration and pitch speed.
upPitch :: GLfloat -> UserInput -> GLfloat
upPitch duration userInp
    | lookUp userInp = duration * pitchSpeed
    | otherwise = 0

-- | Make a pitch down angle proportional to the frame duration and pitch speed.
downPitch :: GLfloat -> UserInput -> GLfloat
downPitch duration userInp
    | lookDown userInp = -(duration * pitchSpeed)
    | otherwise = 0

flyUp :: GLfloat -> UserInput -> GLfloat
flyUp duration userInp
    | flyMode userInp && goUp userInp = duration * movingSpeed
    | otherwise = 0

flyDown :: GLfloat -> UserInput -> GLfloat
flyDown duration userInp
    | flyMode userInp && goDown userInp = -(duration * movingSpeed)
    | otherwise = 0

-- | The unit vector pointing in the positive y-direction, i.e. up.
up :: V3 GLfloat
up = V3 0 1 0

-- | The unit vector pointing in the negative z-direction, i.e. ahead.
ahead :: V3 GLfloat
ahead = V3 0 0 (-1)

-- | Radians to yaw per second.
yawSpeed :: GLfloat
yawSpeed = toRadians 180

-- | Radians to pitch per second.
pitchSpeed :: GLfloat
pitchSpeed = toRadians 45

normalizePitch :: GLfloat -> GLfloat
normalizePitch = clamp (toRadians (-45)) (toRadians 45)

cameraHeight :: GLfloat -> V3 GLfloat -> Terrain -> UserInput -> V3 GLfloat
cameraHeight duration (V3 x y z) terrain' userInp =
    let terrainHeight = Terrain.terrainHeight (x, z) terrain'
        rawHeight = y + flyUp duration userInp + flyDown duration userInp
        height = adjustHeight rawHeight terrainHeight
    in V3 x height z
    where
        adjustHeight :: GLfloat -> GLfloat -> GLfloat
        adjustHeight rawHeight terrainHeight
            | flyMode userInp && terrainCollision userInp =
                max rawHeight (terrainHeight + 0.5)
            | not (flyMode userInp) && terrainCollision userInp =
                terrainHeight + 2
            | otherwise = rawHeight

-- Model space units to move per second.
movingSpeed :: GLfloat
movingSpeed = 10
