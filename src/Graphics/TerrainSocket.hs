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
import qualified BigE.Mesh                  as Mesh
import           BigE.Runtime               (Render)
import           BigE.TerrainGrid           (TerrainGrid, lookup, quadGridSize,
                                             verticeGridSize)
import           BigE.Types                 (BufferUsage (..))
import           Control.Monad.IO.Class     (MonadIO)
import qualified Data.Vector.Storable       as Vector
import           Engine.State               (State)
import           Graphics.GL                (GLuint)
import           Graphics.Types             (TerrainSocket (..))
import           Linear                     (V2 (..), V3 (..))
import           Prelude                    hiding (init)

-- | Initialize the 'TerrainSocket' from a 'TerrainGrid' and the path to
-- the base resource directory.
init :: MonadIO m => TerrainGrid -> FilePath -> m (Either String TerrainSocket)
init terrainGrid _ = do
    let (_, z) = verticeGridSize terrainGrid
        (_, quads) = quadGridSize terrainGrid
        ww = mkWestSocketWall terrainGrid z
        is = indices quads
    mesh' <- Mesh.fromVector StaticDraw (Vector.fromList ww) (Vector.fromList is)
    return $ Right TerrainSocket { mesh = mesh' }

-- | Delete the 'TerrainSocket's resources.
delete :: TerrainSocket -> Render State ()
delete terrainSocket =
    Mesh.delete $ mesh terrainSocket

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

indices :: Int -> [GLuint]
indices quads = concatMap mkQuadIndices [0 .. quads - 1]
    where
        mkQuadIndices :: Int -> [GLuint]
        mkQuadIndices quad =
            let baseIndex = fromIntegral $ quad * 2 + 2
            in [ baseIndex, baseIndex - 2, baseIndex - 1
               , baseIndex, baseIndex - 1, baseIndex + 1
               ]
