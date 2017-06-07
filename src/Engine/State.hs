-- |
-- Module: Engine.State
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.State
    ( State (..)
    ) where

import           Graphics.Camera (Camera)
import           Graphics.GL     (GLfloat)
import           Graphics.Types  (GUI, Terrain, UserInput)
import           Linear          (M44)

-- | The state of the application. It will be carried by the runtime IORef
-- variable.
data State = State
    { resourceDir :: !FilePath
      -- ^ The base directory for all external resource files.

    , perspective :: !(M44 GLfloat)
      -- ^ The perspective matrix for the application. Will be updated when
      -- the screen resulution change.

    , camera      :: !Camera
      -- ^ The application's camera.

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
