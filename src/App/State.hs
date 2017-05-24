-- |
-- Module: App.State
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module App.State
    ( State (..)
    ) where

-- | The state of the application. It will be carried by the runtime IORef
-- variable.
data State = State !Int
    deriving Show
