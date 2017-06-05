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
    , TextEntity (..)
    , UserInput (..)
    , defaultUserInput
    ) where

import           BigE.Mesh              (Mesh)
import           BigE.TextRenderer      (RenderParams, TextRenderer)
import           BigE.TextRenderer.Font (Font)
import           BigE.TextRenderer.Text (Text)
import           BigE.Types             (Location, Program)

-- | Terrain record.
data Terrain = Terrain
    { program :: !Program
     -- ^ The shader program for rendering of terrains.

    , mvpLoc  :: !Location
      -- ^ MVP matrix program location.

    , mesh    :: !Mesh
      -- ^ The mesh.
    } deriving Show

-- | GUI record.
data GUI = GUI
    { textRenderer    :: !TextRenderer
      -- ^ The GUI's 'TextRenderer'.

    , centerFlashFont :: !Font
      -- ^ The 'Font' used for center flash texts.

    , centerFlash     :: !(Maybe TextEntity)
      -- ^ The 'TextEntity' for the center flash.
    } deriving Show

-- | TextUnit record.
data TextEntity = TextEntity
    { text         :: !Text
    , renderParams :: !RenderParams
    } deriving Show

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
