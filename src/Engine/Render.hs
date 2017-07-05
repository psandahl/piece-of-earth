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
import           Engine.State           (State (..))
import qualified Graphics.GL            as GL
import qualified Graphics.GUI           as GUI
import qualified Graphics.SkyBox        as SkyBox
import qualified Graphics.Terrain       as Terrain
import           Graphics.Types         (UserInput (..))

-- | The master rendering callback. Render one frame.
render :: Render State ()
render = do

    -- Clear the depth buffer. The color buffer not need to be cleared as
    -- the sky initially will render all pixels.
    GL.glClear GL.GL_DEPTH_BUFFER_BIT

    -- Render the skybox.
    renderSkyBox

    -- Render the world.
    renderWorld

    -- Render the GUI.
    renderGUI

-- | Render the sky box.
renderSkyBox :: Render State ()
renderSkyBox = do

    -- Render the sky without any depth information. Everything else rendered
    -- after the sky will be visible in front of it.
    disableDepth

    -- As the sky is rendered inside a sphere model we would like to cull the
    -- outside surface. I.e. the front faces of the model.
    enableFrontFaceCulling

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

enableFrontFaceCulling :: MonadIO m => m ()
enableFrontFaceCulling = do
    GL.glEnable GL.GL_CULL_FACE
    GL.glCullFace GL.GL_FRONT
