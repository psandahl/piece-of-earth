-- |
-- Module: Graphics.Fog
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Functionality for Fog. The module is exporting two records, one is the
-- fog itself and the other is the record of locations tied to a program.
module Graphics.Fog
    ( Fog (..)
    , FogLoc (..)
    , getFogLoc
    , setFog
    ) where

import qualified BigE.Program           as Program
import           BigE.Types             (Location, Program, setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat)
import           Linear                 (V3 (..))

-- | The components of Fog.
data Fog = Fog
    { color    :: !(V3 GLfloat)
      -- ^ The color of the fog.

    , fogStart :: !GLfloat
      -- ^ The distance where the fog effect is starting.

    , fogEnd   :: !GLfloat
      -- ^ The distance where the fog distance reach max effect.
    } deriving Show

-- | The collection of locations tied to fog.
data FogLoc = FogLoc
    { colorLoc    :: !Location
    , fogStartLoc :: !Location
    , fogEndLoc   :: !Location
    } deriving Show

-- | For the given program this function is expecting that there is a struct
-- type uniform with three members; color, fogStart and fogEnd.
getFogLoc :: MonadIO m => Program -> String -> m FogLoc
getFogLoc program var =
    FogLoc <$> Program.getUniformLocation program (var ++ ".color")
           <*> Program.getUniformLocation program (var ++ ".fogStart")
           <*> Program.getUniformLocation program (var ++ ".fogEnd")

-- | Set the fog.
setFog :: MonadIO m => FogLoc -> Fog -> m ()
setFog fogLoc fog = do
    setUniform (colorLoc fogLoc) (color fog)
    setUniform (fogStartLoc fogLoc) (fogStart fog)
    setUniform (fogEndLoc fogLoc) (fogEnd fog)
