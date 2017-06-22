-- |
-- Module: Graphics.Lights.AmbientLight
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- AmbientLight is functionality for ambient light. The module is exporting
-- two records, one is the light itself and the other is the record of
-- locations tied to a program.
module Graphics.Lights.AmbientLight
    ( AmbientLight (..)
    , AmbientLightLoc
    , defaultAmbientLight
    , getAmbientLightLoc
    , setAmbientLight
    ) where

import qualified BigE.Program           as Program
import           BigE.Types             (Location, Program, setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat)
import           Linear                 (V3 (..))

-- | The components of an ambient light.
data AmbientLight = AmbientLight
    { color    :: !(V3 GLfloat)
      -- ^ The color of the light.

    , strength :: !GLfloat
      -- ^ The strength of the light.
    } deriving Show

-- | The collection of locations for an ambient light. Tied to a shader program
-- using ambient light.
data AmbientLightLoc = AmbientLightLoc
    { colorLoc    :: !Location
    , strengthLoc :: !Location
    } deriving Show

defaultAmbientLight :: AmbientLight
defaultAmbientLight =
    AmbientLight
        { color = V3 1 0 0
        , strength = 0.1
        }

-- | For the given program this function is expecting that there is a struct
-- type uniform named "ambientLight" with two members; color and strength.
getAmbientLightLoc :: MonadIO m => Program -> m AmbientLightLoc
getAmbientLightLoc program =
    AmbientLightLoc <$> Program.getUniformLocation program "ambientLight.color"
                    <*> Program.getUniformLocation program "ambientLight.strength"

-- | Set the values of the AmbientLight record.
setAmbientLight :: MonadIO m => AmbientLight -> AmbientLightLoc -> m ()
setAmbientLight ambientLight ambientLightLoc = do
    setUniform (colorLoc ambientLightLoc) (color ambientLight)
    setUniform (strengthLoc ambientLightLoc) (strength ambientLight)