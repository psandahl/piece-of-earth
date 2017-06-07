-- |
-- Module: Graphics.Camera
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Camera
    ( init
    , matrix
    ) where

import           Graphics.GL    (GLfloat)
import           Graphics.Types (Camera (..))
import           Linear         (M44, V3 (..), lookAt)
import           Prelude        hiding (init)

-- | Initialze the camera.
init :: Camera
init =
    Camera { view = lookAt (V3 0 3 0) (V3 2 0 2) (V3 0 1 0) }

-- | Get the 'Camera's matrix.
matrix :: Camera -> M44 GLfloat
matrix = view
