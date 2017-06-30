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

import           BigE.Runtime           (Render, getAppStateUnsafe)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Bits              ((.|.))
import           Engine.State           (State (..))
import qualified Graphics.GL            as GL
import qualified Graphics.GUI           as GUI
import qualified Graphics.SkyBox        as SkyBox
import qualified Graphics.Terrain       as Terrain
import           Graphics.Types         (UserInput (..))

-- | The master rendering callback. Render one frame.
render :: Render State ()
render = do
    GL.glClearColor 0 0 0.4 0

    -- Clear the framebuffers.
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    -- Render the skybox.
    renderSkyBox

    -- Render the world.
    renderWorld

    -- Render the GUI.
    renderGUI

-- | Render the sky box.
renderSkyBox :: Render State ()
renderSkyBox = do
    disableDepth
    disableBackFaceCulling

    state <- getAppStateUnsafe
    SkyBox.render $ skyBox state

-- | Render the terrain and all its entities.
renderWorld :: Render State ()
renderWorld = do
    enableDepth
    enableBackFaceCulling

    state <- getAppStateUnsafe
    let userInp = userInput state

    when (renderWireframe userInp) $
        GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE

    -- Render the terrain.
    Terrain.render $ terrain state

    when (renderWireframe userInp) $
        GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_FILL

-- | Render the GUI.
renderGUI :: Render State ()
renderGUI = do
    state <- getAppStateUnsafe
    GUI.render $ gui state

enableDepth :: MonadIO m => m ()
enableDepth = GL.glEnable GL.GL_DEPTH_TEST

disableDepth :: MonadIO m => m ()
disableDepth = GL.glDisable GL.GL_DEPTH_TEST

enableBackFaceCulling :: MonadIO m => m ()
enableBackFaceCulling = do
    GL.glEnable GL.GL_CULL_FACE
    GL.glCullFace GL.GL_BACK

disableBackFaceCulling :: MonadIO m => m ()
disableBackFaceCulling =
    GL.glDisable GL.GL_CULL_FACE
