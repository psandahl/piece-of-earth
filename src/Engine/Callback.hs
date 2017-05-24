-- |
-- Module: Engine.Callback
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Callback
    ( install
    ) where

import           BigE.Math    (mkPerspective)
import           BigE.Runtime (Render, modifyAppState, setWindowSizeCallback)
import           Engine.State (State (..))

-- | Install callbacks.
install :: Render State ()
install =
    setWindowSizeCallback (Just windowSizeCallback)

-- | Callback to handle application specific stuff when the window's size
-- is changed. Adjusting the viewport is handled automatically.
windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState $ \state ->
        state { perspective = mkPerspective (width, height) }
