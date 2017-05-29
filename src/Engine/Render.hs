-- |
-- Module: Engine.Render
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Render
    ( render
    ) where

import           BigE.Runtime     (Render, getAppStateUnsafe)
import           Engine.State     (State (..))
import           Graphics.Camera  (matrix)
import qualified Graphics.GL      as GL
import qualified Graphics.Terrain as Terrain
import           Linear           ((!*!))

-- | The master rendering callback.
render :: Render State ()
render = do
    state <- getAppStateUnsafe

    GL.glClearColor 0 0 0.4 0
    GL.glClear GL.GL_COLOR_BUFFER_BIT

    let vp = perspective state !*! matrix (camera state)

    Terrain.render vp $ terrain state
