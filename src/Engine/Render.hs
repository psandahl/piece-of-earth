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
import           Control.Monad    (when)
import           Data.Bits        ((.|.))
import           Engine.State     (State (..), UserInput (..))
import           Graphics.Camera  (matrix)
import qualified Graphics.GL      as GL
import qualified Graphics.Terrain as Terrain
import           Linear           ((!*!))

-- | The master rendering callback.
render :: Render State ()
render = do
    state <- getAppStateUnsafe

    -- Setting GL state for the rendering phase.
    GL.glEnable GL.GL_DEPTH_TEST
    GL.glEnable GL.GL_CULL_FACE
    GL.glCullFace GL.GL_BACK
    GL.glClearColor 0 0 0.4 0

    -- Clear the framebuffers.
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    -- Calculate the view/perspective matrix. Model matrices will be added
    -- in local renderers.
    let vp = perspective state !*! matrix (camera state)
        userInp = userInput state

    when (renderWireframe userInp) $
        GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE

    -- Render the terrain.
    Terrain.render vp $ terrain state

    when (renderWireframe userInp) $
        GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL
