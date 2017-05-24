-- |
-- Module: App.Animate
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module App.Animate
    ( animate
    ) where

import           App.State    (State)
import           BigE.Runtime (Render)

animate :: Render State ()
animate = return ()
