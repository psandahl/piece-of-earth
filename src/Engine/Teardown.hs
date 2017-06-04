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

import           BigE.Runtime     (Render, getAppStateUnsafe)
import           Engine.State     (State (..))
import qualified Graphics.GUI     as GUI
import qualified Graphics.Terrain as Terrain

-- | Teardown the application. Clean-up time.
teardown :: Render State ()
teardown = do
    state <- getAppStateUnsafe

    Terrain.delete $ terrain state
    GUI.delete $ gui state
