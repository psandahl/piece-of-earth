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

import           Graphics.GL (GLfloat)
import           Linear      (M44)

-- | The state of the application. It will be carried by the runtime IORef
-- variable.
data State = State
    { resourceDir :: !FilePath
      -- ^ The base directory for all external resource files.

    , perspective :: !(M44 GLfloat)
      -- ^ The perspective matrix for the application. Will be updated when
      -- the screen resulution change.

    , frameRate   :: !Double
      -- ^ The current frame rate. Will only change if it's differ significantly
      -- from last frame's measured rate.
    }
    deriving Show
