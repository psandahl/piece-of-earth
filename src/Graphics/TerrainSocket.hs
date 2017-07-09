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

import qualified BigE.Attribute.Vert_P_N_Tx as Vert_P_N_Tx
import           BigE.Runtime               (Render)
import           BigE.TerrainGrid           (TerrainGrid, lookup)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Engine.State               (State)
import           Graphics.Types             (TerrainSocket (..))
import           Linear                     (V2 (..), V3 (..))
import           Prelude                    hiding (init)

-- | Initialize the 'TerrainSocket' from a 'TerrainGrid' and the path to
-- the base resource directory.
init :: MonadIO m => TerrainGrid -> FilePath -> m (Either String TerrainSocket)
init terrainGrid _ = do
    --let (x, z) = verticeGridSize terrainGrid
    --liftIO $ putStrLn $ "X: " ++ show x
    --liftIO $ putStrLn $ "Z: " ++ show z
    let ww = mkWestSocketWall terrainGrid 3
    liftIO $ print ww
    return $ Right TerrainSocket { dummy = 1 }

-- | Delete the 'TerrainSocket's resources.
delete :: TerrainSocket -> Render State ()
delete _ = return ()

-- OMFG so ugly. Make it work. Make it beautiful.
mkWestSocketWall :: TerrainGrid -> Int -> [Vert_P_N_Tx.Vertex]
mkWestSocketWall terrainGrid num = reverse $ go [] 0
    where
        go :: [Vert_P_N_Tx.Vertex] -> Int -> [Vert_P_N_Tx.Vertex]
        go xs idx
            | idx >= (num - 1) = xs
            | even idx =
                let V3 x y z = BigE.TerrainGrid.lookup (0, idx) terrainGrid
                    upper = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x y z
                                               , Vert_P_N_Tx.normal = V3 (-1) 0 0
                                               , Vert_P_N_Tx.texCoord = V2 0 1
                                               }
                    lower = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x 0 z
                                               , Vert_P_N_Tx.normal = V3 (-1) 0 0
                                               , Vert_P_N_Tx.texCoord = V2 0 0
                                               }
                in go (lower:upper:xs) (idx + 1)
            | otherwise =
                let V3 x y z = BigE.TerrainGrid.lookup (0, idx) terrainGrid
                    upper = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x y z
                                               , Vert_P_N_Tx.normal = V3 (-1) 0 0
                                               , Vert_P_N_Tx.texCoord = V2 1 1
                                               }
                    lower = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x 0 z
                                               , Vert_P_N_Tx.normal = V3 (-1) 0 0
                                               , Vert_P_N_Tx.texCoord = V2 1 0
                                               }
                in go (lower:upper:xs) (idx + 1)
