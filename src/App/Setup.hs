-- |
-- Module: App.Setup
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module App.Setup
    ( setup
    ) where

import           App.Options  (Options)
import qualified App.Options  as Options
import           App.State    (State (..))
import           BigE.Math    (mkPerspective)
import           BigE.Runtime (Render, displayDimensions)

-- | Setup the state for the application.
setup :: Options -> Render State (Either String State)
setup options = do
    dimensions <- displayDimensions

    -- Create the 'State' record.
    let state = State { resourceDir = Options.resourceDir options
                      , perspective = mkPerspective dimensions
                      }

    return $ Right state
