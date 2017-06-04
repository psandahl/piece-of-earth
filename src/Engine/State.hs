-- |
-- Module: Engine.State
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.State
    ( State (..)
    , UserInput (..)
    , defaultUserInput
    ) where

import           Graphics.Camera (Camera)
import           Graphics.GL     (GLfloat)
import           Graphics.Types  (Terrain)
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

    , frameRate   :: !Double
      -- ^ The current frame rate. Will only change if it's differ significantly
      -- from last frame's measured rate.

    , userInput   :: !UserInput
      -- ^ The user input valid for the frame.
    } deriving Show

-- | Values set by user input. Used in animate or renders callbacks.
data UserInput = UserInput
    { renderWireframe :: !Bool
      -- ^ Render the main models as wireframes.
    } deriving Show

-- | Set default values for the 'UserInput'.
defaultUserInput :: UserInput
defaultUserInput =
    UserInput
        { renderWireframe = False
        }
