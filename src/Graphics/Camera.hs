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

import           BigE.Runtime   (Render)
import           Engine.State   (State)
import           Graphics.Types (Camera (..))
import           Linear         (V3 (..), lookAt)
import           Prelude        hiding (init)

-- | Initialze the 'Camera'.
init :: Camera
init =
    Camera
        { viewMatrix = lookAt (V3 0 3 0) (V3 2 0 2) (V3 0 1 0)
        , cameraPosition = V3 0 3 0
        }

-- | Animate the 'Camera'
animate :: Camera -> Render State Camera
animate = return
