-- |
-- Module: Engine.State
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.State
    ( State (..)
    , getPerspectiveMatrix
    , getViewMatrix
    , getFrameCount
    , incFrameCount
    , getTimeOfDay
    , setTimeOfDay
    ) where

import           BigE.Runtime          (Render, getAppStateUnsafe,
                                        modifyAppState)
import           Graphics.GL           (GLfloat)
import           Graphics.Types        (Camera (viewMatrix), GUI, SkyBox,
                                        Terrain, UserInput)
import           Linear                (M44)
import           Simulation.Atmosphere (TimeOfDay)

-- | The state of the application. It will be carried by the runtime IORef
-- variable.
data State = State
    { resourceDir :: !FilePath
      -- ^ The base directory for all external resource files.

    , perspective :: !(M44 GLfloat)
      -- ^ The perspective matrix for the application. Will be updated when
      -- the screen resolution change.

    , timeOfDay   :: !TimeOfDay
      -- ^ The current time of day.

    , camera      :: !Camera
      -- ^ The application's camera.

    , skyBox      :: !SkyBox
      -- ^ The application's sky box.

    , terrain     :: !Terrain
      -- ^ The application's container holding all terrain.

    , gui         :: !GUI
      -- ^ The application's GUI.

    , frameCount  :: !Int
      -- ^ The current frame number.

    , frameRate   :: !Double
      -- ^ The current frame rate. Will only change if it's differ significantly
      -- from last frame's measured rate.

    , userInput   :: !UserInput
      -- ^ The user input valid for the frame.
    } deriving Show

getPerspectiveMatrix :: Render State (M44 GLfloat)
getPerspectiveMatrix = perspective <$> getAppStateUnsafe

getViewMatrix :: Render State (M44 GLfloat)
getViewMatrix = (viewMatrix . camera) <$> getAppStateUnsafe

getFrameCount :: Render State Int
getFrameCount = frameCount <$> getAppStateUnsafe

incFrameCount :: Render State ()
incFrameCount = modifyAppState $ \state ->
    state { frameCount = frameCount state + 1}

getTimeOfDay :: Render State TimeOfDay
getTimeOfDay = timeOfDay <$> getAppStateUnsafe

setTimeOfDay :: TimeOfDay -> Render State ()
setTimeOfDay tod = modifyAppState $ \state ->
    state { timeOfDay = tod }
