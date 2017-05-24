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

import           BigE.Runtime (Render)
import           Engine.State (State)

teardown :: Render State ()
teardown = return ()
