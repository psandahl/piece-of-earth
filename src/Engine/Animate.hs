-- |
-- Module: Engine.Animate
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Animate
    ( animate
    ) where

import           BigE.Runtime  (Render, frameDuration, getAppStateUnsafe,
                                putAppState)
import           Control.Monad (when)
import           Engine.State  (State (..))

-- | Prepare things that will move or shake to next renderering phase.
animate :: Render State ()
animate =
    calculateFrameRate

-- | Calculate the frame rate for the frame. Only update the counter if it
-- differs more than five percent of the old counter's value.
calculateFrameRate :: Render State ()
calculateFrameRate = do
    state <- getAppStateUnsafe
    duration <- frameDuration

    let oldFps = frameRate state
        fps = 1.0 / (duration + 0.0000001) -- Add a small extra to avoid div with 0.
        fpsDiff = abs $ oldFps - fps
    when (fpsDiff > 0.05 * oldFps) $
        putAppState (state { frameRate = fps })
