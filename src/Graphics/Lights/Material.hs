-- |
-- Module: Graphics.Lights.LightEmitter
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Material parameters for specular lightning. The module is exporting two
-- records, one is the material itself and the other is the record of locations
-- tied to a program.
module Graphics.Lights.Material
    ( Material (..)
    , MaterialLoc
    , getMaterialLoc
    , setMaterial
    ) where

import qualified BigE.Program           as Program
import           BigE.Types             (Location, Program, setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat, GLint)

-- | Material with properties for specular lightning.
data Material = Material
    { shine    :: !GLint
      -- ^ The shininess of the material.

    , strength :: !GLfloat
      -- ^ The specular strength of the material.
    } deriving Show

-- | The collection of locations for the material.
data MaterialLoc = MaterialLoc
    { shineLoc    :: !Location
    , strengthLoc :: !Location
    } deriving Show

-- | For the given program the function is expecting there is a a struct type
-- uniform with the members; shine and strength.
getMaterialLoc :: MonadIO m => Program -> String -> m MaterialLoc
getMaterialLoc program var =
    MaterialLoc <$> Program.getUniformLocation program (var ++ ".shine")
                <*> Program.getUniformLocation program (var ++ ".strength")

-- | Set the material.
setMaterial :: MonadIO m => MaterialLoc -> Material -> m ()
setMaterial materialLoc material = do
    setUniform (shineLoc materialLoc) (shine material)
    setUniform (strengthLoc materialLoc) (strength material)
