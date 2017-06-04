-- |
-- Module: Graphics.Terrain
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Types
    ( Terrain (..)
    , GUI (..)
    ) where

import           BigE.Mesh  (Mesh)
import           BigE.Types (Location, Program)

-- | Terrain record.
data Terrain = Terrain
    { program :: !Program
     -- ^ The shader program for rendering of terrains.

    , mvpLoc  :: !Location

    , mesh    :: !Mesh
    } deriving Show

-- | GUI record.
data GUI = GUI
    deriving Show
