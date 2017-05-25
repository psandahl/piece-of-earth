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

import           BigE.Math       (mkPerspective)
import           BigE.Runtime    (Render, displayDimensions)
import           Engine.Callback (install)
import           Engine.Options  (Options)
import qualified Engine.Options  as Options
import           Engine.State    (State (..))

-- | Setup the state for the application.
setup :: Options -> Render State (Either String State)
setup options = do
    -- Install callbacks.
    install

    dimensions <- displayDimensions

    -- Create the 'State' record.
    let state = State { resourceDir = Options.resourceDir options
                      , perspective = mkPerspective dimensions
                      , frameRate = 0
                      }

    return $ Right state
