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

import           BigE.Math                    (mkPerspective)
import           BigE.Runtime                 (Render, displayDimensions)
import           BigE.Util                    (eitherTwo)
import           Engine.Callback              (install)
import           Engine.Options               (Options)
import qualified Engine.Options               as Options
import           Engine.State                 (State (..))
import qualified Graphics.Camera              as Camera
import qualified Graphics.GUI                 as GUI
import qualified Graphics.Lights.AmbientLight as AmbientLight
import qualified Graphics.Terrain             as Terrain
import           Graphics.Types               (defaultUserInput)

-- | Setup the state for the application.
setup :: Options -> Render State (Either String State)
setup options = do
    -- Start by initializing things that can fail.
    let resourceDir' = Options.resourceDir options
    eTerrain <- Terrain.init resourceDir'
    eGUI <- GUI.init resourceDir'

    case eitherTwo (eTerrain, eGUI) of

        Right (terrain', gui') -> do

            -- Install callbacks.
            install

            dimensions <- displayDimensions

            -- Create the 'State' record.
            let state = State { resourceDir = resourceDir'
                              , perspective = mkPerspective dimensions
                              , ambientLight = AmbientLight.defaultAmbientLight
                              , camera = Camera.init
                              , terrain = terrain'
                              , gui = gui'
                              , frameCount = 0
                              , frameRate = 0
                              , userInput = defaultUserInput
                              }

            -- Done. We have built an initial state.
            return $ Right state

        -- No hope. Just return with the error string.
        Left err -> return $ Left err
