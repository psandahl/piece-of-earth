-- |
-- Module: Graphics.Terrain
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.Types
    ( Camera (..)
    , Terrain (..)
    , GUI (..)
    , TextEntity (..)
    , UserInput (..)
    , defaultUserInput
    ) where

import           BigE.Mesh              (Mesh)
import           BigE.TerrainGrid       (TerrainGrid)
import           BigE.TextRenderer      (RenderParams, TextRenderer)
import           BigE.TextRenderer.Font (Font)
import           BigE.TextRenderer.Text (Text)
import           BigE.Types             (Location, Program)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44, V3 (..))

-- | The camera record.
data Camera = Camera
    { viewMatrix     :: !(M44 GLfloat)
      -- ^ The camera's view matrix. Calculated during init/animate.

    , cameraPosition :: !(V3 GLfloat)
      -- ^ The camera's current position in model space. Calculated during
      -- init/animate.
    } deriving Show

-- | Terrain record.
data Terrain = Terrain
    { program     :: !Program
     -- ^ The shader program for rendering of terrains.

    , mvpLoc      :: !Location
      -- ^ MVP matrix program location.

    , terrainGrid :: !TerrainGrid
      -- ^ The terrain's grid.

    , mesh        :: !Mesh
      -- ^ The mesh.
    } deriving Show

-- | GUI record.
data GUI = GUI
    { textRenderer    :: !TextRenderer
      -- ^ The GUI's 'TextRenderer'.

    , centerFlashFont :: !Font
      -- ^ The 'Font' used for center flash texts.

    , statusBar       :: !TextEntity
      -- ^ The top status bar. Always updated but not always shown.

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

    , renderStatusBar :: !Bool
      -- ^ Render a status bar at top of screen.
    } deriving Show

-- | Set default values for the 'UserInput'.
defaultUserInput :: UserInput
defaultUserInput =
    UserInput
        { renderWireframe = False
        , renderStatusBar = False
        }
