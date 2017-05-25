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

import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44)
import           Prelude                hiding (init)

-- | Terrain record.
data Terrain = Terrain
    deriving Show

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init _resourceDir = return $ Right Terrain

delete :: MonadIO m => Terrain -> m ()
delete _terrain = return ()

-- | Render all terrain given the perspective/view matrix.
render :: MonadIO m => M44 GLfloat -> m ()
render = undefined
