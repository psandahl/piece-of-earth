-- |
-- Module: Engine.Setup
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Setup
    ( setup
    ) where

import           BigE.Math        (mkPerspective)
import           BigE.Runtime     (Render, displayDimensions)
import           Engine.Callback  (install)
import           Engine.Options   (Options)
import qualified Engine.Options   as Options
import           Engine.State     (State (..), defaultUserInput)
import qualified Graphics.Camera  as Camera
import qualified Graphics.Terrain as Terrain

-- | Setup the state for the application.
setup :: Options -> Render State (Either String State)
setup options = do
    -- Start by initializing things that can fail.
    eTerrain <- Terrain.init $ Options.resourceDir options

    case eTerrain of

        Right terrain' -> do

            -- Install callbacks.
            install

            dimensions <- displayDimensions

            -- Create the 'State' record.
            let state = State { resourceDir = Options.resourceDir options
                              , perspective = mkPerspective dimensions
                              , camera = Camera.init
                              , terrain = terrain'
                              , frameRate = 0
                              , userInput = defaultUserInput
                              }

            -- Done. We have built an initial state.
            return $ Right state

        -- No hope. Just return with the error string.
        Left err -> return $ Left err
