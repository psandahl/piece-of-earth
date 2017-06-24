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

-- | For the given program this function is expecting that there is a struct
-- type uniform with two members; color and strength.
getAmbientLightLoc :: MonadIO m => Program -> String -> m AmbientLightLoc
getAmbientLightLoc program var =
    AmbientLightLoc <$> Program.getUniformLocation program (var ++ ".color")
                    <*> Program.getUniformLocation program (var ++ ".strength")

-- | Set the ambient light.
setAmbientLight :: MonadIO m => AmbientLightLoc -> AmbientLight -> m ()
setAmbientLight ambientLightLoc ambientLight = do
    setUniform (colorLoc ambientLightLoc) (color ambientLight)
    setUniform (strengthLoc ambientLightLoc) (strength ambientLight)
