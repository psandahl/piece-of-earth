-- |
-- Module: Engine.Teardown
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Teardown
    ( teardown
    ) where

import           BigE.Runtime           (Render, getAppStateUnsafe)
import           Engine.State           (State (..))
import qualified Graphics.GUI           as GUI
import qualified Graphics.SkyDome       as SkyDome
import qualified Graphics.Terrain       as Terrain
import qualified Graphics.TerrainSocket as TerrainSocket

-- | Teardown the application. Clean-up time.
teardown :: Render State ()
teardown = do
    state <- getAppStateUnsafe

    SkyDome.delete $ skyDome state
    Terrain.delete $ terrain state
    TerrainSocket.delete $ terrainSocket state
    GUI.delete $ gui state
