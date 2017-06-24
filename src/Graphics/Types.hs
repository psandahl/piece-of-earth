-- |
-- Module: Graphics.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- General types, and types for modules using the state module. Such types
-- must be here otherwise cycles occur.
module Graphics.Types
    ( Camera (..)
    , Terrain (..)
    , GUI (..)
    , TextEntity (..)
    , UserInput (..)
    , defaultUserInput
    ) where

import           BigE.Mesh                    (Mesh)
import           BigE.TerrainGrid             (TerrainGrid)
import           BigE.TextRenderer            (RenderParams, TextRenderer)
import           BigE.TextRenderer.Font       (Font)
import           BigE.TextRenderer.Text       (Text)
import           BigE.Types                   (Location, Program, Texture)
import           Graphics.GL                  (GLfloat)
import           Graphics.Lights.AmbientLight (AmbientLightLoc)
import           Graphics.Lights.LightEmitter (LightEmitterLoc)
import           Linear                       (M44, V3 (..))

-- | The camera record.
data Camera = Camera
    { viewMatrix     :: !(M44 GLfloat)
      -- ^ The camera's view matrix. Calculated during init/animate.

    , cameraPosition :: !(V3 GLfloat)
      -- ^ The camera's current position in model space. Calculated during
      -- init/animate.

    , yaw            :: !GLfloat
      -- ^ The yaw angle (the camera's rotation on the y-axis).

    , pitch          :: !GLfloat
      -- ^ The pitch angle (the camera's rotation of the x-axis).
    } deriving Show

-- | Terrain record.
data Terrain = Terrain
    { program          :: !Program
     -- ^ The shader program for rendering of terrains.

    , mvpLoc           :: !Location
      -- ^ MVP matrix program location.

    , groundTextureLoc :: !Location
      -- ^ Location for the ground texture.

    , ambientLightLoc  :: !AmbientLightLoc
      -- ^ Locations for the ambient light.

    , sunLightLoc      :: !LightEmitterLoc
      -- ^ Locations for the sun light.

    , terrainGrid      :: !TerrainGrid
      -- ^ The terrain's grid.

    , groundTexture    :: !Texture
      -- ^ The ground texture.

    , mesh             :: !Mesh
      -- ^ The mesh.
    } deriving Show

-- | GUI record.
data GUI = GUI
    { textRenderer    :: !TextRenderer
      -- ^ The GUI's 'TextRenderer'.

    , statusBarFont   :: !Font
      -- ^ The 'Font' used for status bar texts.

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

-- | Values set by user input. Used in animate or render callbacks.
data UserInput = UserInput
    { renderWireframe  :: !Bool
      -- ^ Render the main models as wireframes.

    , renderStatusBar  :: !Bool
      -- ^ Render a status bar at top of screen.

    , terrainCollision :: !Bool
      -- ^ Camera is using terrain collision.

    , flyMode          :: !Bool
      -- ^ Camera is able to fly.

    , turnLeft         :: !Bool
      -- ^ Turn (rotate) the camera left.

    , turnRight        :: !Bool
      -- ^ Turn (rotate) the camera right.

    , goForward        :: !Bool
      -- ^ Move the camera forward.

    , goBackward       :: !Bool
      -- ^ Move the camera backward.

    , lookUp           :: !Bool
      -- ^ Make the camera look up.

    , lookDown         :: !Bool
      -- ^ Make the camera look down.

    , goUp             :: !Bool
      -- ^ Increase the height of the camera.

    , goDown           :: !Bool
      -- ^ Decrease the height of the camera.
    } deriving Show

-- | Set default values for the 'UserInput'.
defaultUserInput :: UserInput
defaultUserInput =
    UserInput
        { renderWireframe = False
        , renderStatusBar = False
        , terrainCollision = False
        , flyMode = False
        , turnLeft = False
        , turnRight = False
        , goForward = False
        , goBackward = False
        , lookUp = False
        , lookDown = False
        , goUp = False
        , goDown = False
        }
