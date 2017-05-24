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
import           App.State    (State)
import           BigE.Runtime (Render)

setup :: Options -> Render State (Either String State)
setup = undefined
