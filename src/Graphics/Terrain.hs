-- |
-- Module: Graphics.Terrain
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- The Terrain module holds the pieces of terrain together and is responsible
-- for the rendering of the terrain.
module Graphics.Terrain
    ( Terrain
    , init
    , delete
    , render
    ) where

import           BigE.Runtime           (Render)
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44)
import           Prelude                hiding (init)

-- | Terrain record.
data Terrain = Terrain

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init _resourceDir = return $ Right Terrain

delete :: MonadIO m => Terrain -> m ()
delete = undefined

-- | Render all terrain given the perspective/view matrix.
render :: M44 GLfloat -> Render State ()
render = undefined
