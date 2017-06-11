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

import           BigE.Math      (mkRotate)
import           BigE.Runtime   (Render, frameDuration, getAppStateUnsafe)
--import           Control.Monad.IO.Class (liftIO)
import           Engine.State   (State (userInput))
import           Graphics.GL    (GLfloat)
import           Graphics.Types (Camera (..), UserInput (..))
import           Linear         (M33, M44, V3 (..), V4 (..), identity, lookAt,
                                 normalize, (!*), (!*!))
import           Prelude        hiding (init)

-- | Initialze the 'Camera'.
init :: Camera
init =
    Camera
        { viewMatrix = lookAt (V3 0 1 0) (V3 2 0 2) yAxis
        , cameraPosition = V3 0 1 0
        , heading = normalize $ V3 1 0 1
        }

-- | Animate the 'Camera'. Always perform all calculations, simpler code and
-- deterministic timing every frame.
animate :: Camera -> Render State Camera
animate camera = do
    -- Get some useful stuff from the runtime.
    userInp <- userInput <$> getAppStateUnsafe
    duration <- realToFrac <$> frameDuration

    -- Calculate the new camera heading direction.
    let leftMatrix = leftRotation duration userInp
        rightMatrix = rightRotation duration userInp
        heading' = normalize $ (leftMatrix !*! rightMatrix) !* heading camera

    --liftIO $ print heading'

    -- Calculate the spot where the camera is looking and then make the new
    -- camera view matrix.
    let viewSpot = cameraPosition camera + heading'
        viewMatrix' = lookAt (cameraPosition camera) viewSpot yAxis

    --liftIO $ print viewSpot

    -- Give back the new camera.
    return camera { viewMatrix = viewMatrix', heading = heading' }

-- | Make a left camera rotation matrix proportional to the frame duration
-- and rotation speed.
leftRotation :: GLfloat -> UserInput -> M33 GLfloat
leftRotation duration userInp
    | turnLeft userInp =
        let radians = duration * rotationSpeed
        in mkRotate33 yAxis radians
    | otherwise = identity

-- | Make a right camera rotation matrix proportional to the frame duration
-- and rotation speed.
rightRotation :: GLfloat -> UserInput -> M33 GLfloat
rightRotation duration userInp
    | turnRight userInp =
        let radians = duration * rotationSpeed
        in mkRotate33 yAxis (-radians)
    | otherwise = identity

-- | The unit vector pointing in the positive y-direction, i.e. up.
yAxis :: V3 GLfloat
yAxis = V3 0 1 0

-- | Radians to rotate per second.
rotationSpeed :: GLfloat
rotationSpeed = pi

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
