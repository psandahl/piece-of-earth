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

import           BigE.Math      (mkRotate, toRadians)
import           BigE.Runtime   (Render, frameDuration, getAppStateUnsafe)
--import           Control.Monad.IO.Class (liftIO)
import           Engine.State   (State (userInput))
import           Graphics.GL    (GLfloat)
import           Graphics.Types (Camera (..), UserInput (..))
import           Linear         (M33, M44, V3 (..), V4 (..), lookAt, normalize,
                                 zero, (!*), (*^))
import           Prelude        hiding (init)

-- | Initialze the 'Camera'.
init :: Camera
init =
    Camera
        { viewMatrix = lookAt (V3 0 1 0) (V3 2 0 2) up
        , cameraPosition = V3 0 1 0
        , yaw = toRadians (-90)
        }

-- | Animate the 'Camera'. Always perform all calculations, simpler code and
-- deterministic timing every frame.
animate :: Camera -> Render State Camera
animate camera = do
    -- Get some useful stuff from the runtime.
    userInp <- userInput <$> getAppStateUnsafe
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

    -- Calculate the spot where the camera is looking and then make the new
    -- camera view matrix.
    let viewSpot = cameraPosition' + heading
        viewMatrix' = lookAt cameraPosition' viewSpot up

    --liftIO $ print viewSpot

    -- Give back the new camera.
    return camera { viewMatrix = viewMatrix'
                  , cameraPosition = cameraPosition'
                  , yaw = yaw'
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

-- | The unit vector pointing in the positive y-direction, i.e. up.
up :: V3 GLfloat
up = V3 0 1 0

-- | The unit vector pointing in the negative z-direction, i.e. ahead.
ahead :: V3 GLfloat
ahead = V3 0 0 (-1)

-- | Radians to yaw/rotate per second.
yawSpeed :: GLfloat
yawSpeed = pi

-- Model space units to move per second.
movingSpeed :: GLfloat
movingSpeed = 1

-- | Make a M33 rotation matrix.
mkRotate33 :: V3 GLfloat -> GLfloat -> M33 GLfloat
mkRotate33 axis = toM33 . mkRotate axis
    where
        toM33 :: M44 GLfloat -> M33 GLfloat
        toM33 (V4 (V4 x1 y1 z1 _)
                  (V4 x2 y2 z2 _)
                  (V4 x3 y3 z3 _)
                  _) =
            V3 (V3 x1 y1 z1)
               (V3 x2 y2 z2)
               (V3 x3 y3 z3)
