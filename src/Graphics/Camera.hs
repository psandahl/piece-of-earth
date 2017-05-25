-- |
-- Module: Graphics.Camera
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Camera
    ( Camera
    , init
    , matrix
    ) where

import           Graphics.GL (GLfloat)
import           Linear      (M44)
import           Prelude     hiding (init)

data Camera = Camera

-- | Initialze the camera.
init :: Camera
init = Camera

-- | Get the 'Camera's matrix.
matrix :: M44 GLfloat
matrix = undefined
