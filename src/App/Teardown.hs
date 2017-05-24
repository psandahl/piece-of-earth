-- |
-- Module: App.Teardown
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module App.Teardown
    ( teardown
    ) where

import           App.State    (State)
import           BigE.Runtime (Render)

teardown :: Render State ()
teardown = undefined
