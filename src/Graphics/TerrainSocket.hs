-- |
-- Module: Graphics.TerrainSocket
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- TerrainSocket is a socket generated from a TerrainGrid. Its purpose is to
-- make sure that there are no "holes" in the side of the model when looking
-- from the side.
module Graphics.TerrainSocket
    ( init
    , delete
    ) where

import           BigE.Runtime           (Render)
import           BigE.TerrainGrid       (TerrainGrid)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Engine.State           (State)
import           Graphics.Types         (TerrainSocket (..))
import           Prelude                hiding (init)

-- | Initialize the 'TerrainSocket' from a 'TerrainGrid' and the path to
-- the base resource directory.
init :: MonadIO m => TerrainGrid -> FilePath -> m (Either String TerrainSocket)
init _ _ = do
    liftIO $ putStrLn "Foo"
    return $ Right TerrainSocket { dummy = 1 }

-- | Delete the 'TerrainSocket's resources.
delete :: TerrainSocket -> Render State ()
delete _ = return ()
