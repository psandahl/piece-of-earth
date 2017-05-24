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

import           BigE.Runtime (Render)
import           Engine.State (State)

animate :: Render State ()
animate = return ()
