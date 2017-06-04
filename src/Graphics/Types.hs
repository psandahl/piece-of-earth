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
    , UserInput (..)
    , defaultUserInput
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

-- | Values set by user input. Used in animate or renders callbacks.
data UserInput = UserInput
    { renderWireframe :: !Bool
      -- ^ Render the main models as wireframes.
    } deriving Show

-- | Set default values for the 'UserInput'.
defaultUserInput :: UserInput
defaultUserInput =
    UserInput
        { renderWireframe = False
        }
