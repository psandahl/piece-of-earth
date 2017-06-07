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
                                modifyAppState, putAppState)
import           Control.Monad (when)
import           Engine.State  (State (..))
import qualified Graphics.GUI  as GUI

-- | Prepare things that will move or shake to next renderering phase.
animate :: Render State ()
animate = do
    -- Frame count will be used by later animations. Perform early.
    increaseFrameCount

    -- Frame rate will be used by later animations. Perform early.
    calculateFrameRate

    -- Animate the GUI.
    animateGUI

-- | Increase the frame counter.
increaseFrameCount :: Render State ()
increaseFrameCount =
    modifyAppState $ \state ->
        state { frameCount = frameCount state + 1 }

-- | Calculate the frame rate for the frame. Only update the counter if it
-- differs more than five percent of the old counter's value.
calculateFrameRate :: Render State ()
calculateFrameRate = do
    state <- getAppStateUnsafe
    duration <- frameDuration

    let oldFps = frameRate state
        fps = 1.0 / (duration + 0.0000001) -- Add a small extra to avoid div with 0.
        fpsDiff = abs $ oldFps - fps
    when (fpsDiff > 0.01 * oldFps) $
        putAppState (state { frameRate = fps })

-- | Animate the GUI.
animateGUI :: Render State ()
animateGUI = do
    state <- getAppStateUnsafe

    gui' <- GUI.animate $ gui state
    putAppState (state { gui = gui' })
