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
    , render
    ) where

import qualified BigE.Attribute.Vert_P_N_Tx   as Vert_P_N_Tx
import           BigE.Mesh                    (Mesh)
import qualified BigE.Mesh                    as Mesh
import qualified BigE.Program                 as Program
import           BigE.Runtime                 (Render)
import           BigE.TerrainGrid             (TerrainGrid)
import qualified BigE.TerrainGrid             as TerrainGrid
import           BigE.Types                   (BufferUsage (..), Primitive (..),
                                               Program, ShaderType (..),
                                               setUniform)
import           Control.Monad.IO.Class       (MonadIO)
import qualified Data.Vector.Storable         as Vector
import           Engine.State                 (State, getPerspectiveMatrix,
                                               getTimeOfDay, getViewMatrix)
import           Graphics.GL                  (GLfloat, GLuint)
import           Graphics.Lights.AmbientLight (getAmbientLightLoc,
                                               setAmbientLight)
import           Graphics.Lights.LightEmitter (getLightEmitterLoc,
                                               setLightEmitter)
import           Graphics.Lights.Material     (Material (..), getMaterialLoc,
                                               setMaterial)
import           Graphics.Types               (TerrainSocket (..))
import           Linear                       (V2 (..), V3 (..), identity,
                                               (!*!))
import           Prelude                      hiding (init)
import           Simulation.Atmosphere        (ambientLight, sunLight)
import           System.FilePath              ((</>))

-- | Initialize the 'TerrainSocket' from a 'TerrainGrid' and the path to
-- the base resource directory.
init :: MonadIO m => TerrainGrid -> FilePath -> m (Either String TerrainSocket)
init terrainGrid resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' -> do

            mesh' <- loadMesh terrainGrid
            mvpMatrixLoc' <- Program.getUniformLocation program' "mvpMatrix"
            mvMatrixLoc' <- Program.getUniformLocation program' "mvMatrix"
            vMatrixLoc' <- Program.getUniformLocation program' "vMatrix"
            ambientLightLoc' <- getAmbientLightLoc program' "ambientLight"
            sunLightLoc' <- getLightEmitterLoc program' "sunLight"
            materialLoc' <- getMaterialLoc program' "material"

            return $
                Right TerrainSocket
                    { program = program'
                    , modelMatrix = identity
                    , mvpMatrixLoc = mvpMatrixLoc'
                    , mvMatrixLoc = mvMatrixLoc'
                    , vMatrixLoc = vMatrixLoc'
                    , ambientLightLoc = ambientLightLoc'
                    , sunLightLoc = sunLightLoc'
                    , material = Material { shine = 32, strength = 1 }
                    , materialLoc = materialLoc'
                    , mesh = mesh'
                    }

        Left err -> return $ Left err

-- | Delete the 'TerrainSocket's resources.
delete :: TerrainSocket -> Render State ()
delete terrainSocket = do
    Program.delete $ program terrainSocket
    Mesh.delete $ mesh terrainSocket

-- | Render the 'TerrainSocket'.
render :: TerrainSocket -> Render State ()
render terrainSocket = do
    Program.enable $ program terrainSocket

    -- Setting uniforms.
    pMatrix <- getPerspectiveMatrix
    vMatrix <- getViewMatrix
    let mv = vMatrix !*! modelMatrix terrainSocket
        mvp = pMatrix !*! mv
    setUniform (mvpMatrixLoc terrainSocket) mvp
    setUniform (mvMatrixLoc terrainSocket) mv
    setUniform (vMatrixLoc terrainSocket) vMatrix

    timeOfDay <- getTimeOfDay
    setAmbientLight (ambientLightLoc terrainSocket) ambientLight
    setLightEmitter (sunLightLoc terrainSocket) $ sunLight timeOfDay
    setMaterial (materialLoc terrainSocket) $ material terrainSocket

    -- Render stuff.
    Mesh.enable $ mesh terrainSocket
    Mesh.render Triangles $ mesh terrainSocket

    -- Clean up.
    Program.disable

-- | Load the mesh for 'TerrainGrid'.
loadMesh :: MonadIO m => TerrainGrid -> m Mesh
loadMesh terrainGrid = do
    let (_, z) = TerrainGrid.verticeGridSize terrainGrid
        walls = [ mkSocketWall terrainGrid z (V3 (-1) 0 0) westWallTraverse
                , mkSocketWall terrainGrid z (V3 0 0 1) southWallTraversal
                , mkSocketWall terrainGrid z (V3 1 0 0) eastWallTraversal
                , mkSocketWall terrainGrid z (V3 0 0 (-1)) northWallTraversal
                ]
        is = indices (z * 4)
    Mesh.fromVector StaticDraw (Vector.fromList $ concat walls) (Vector.fromList is)

-- | Load the program used for terrain socket rendering.
loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "entity.vert"
        fragmentShader = resourceDir </> "shaders" </> "entity.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]

type WallTraverse = Int -> Int -> (Int, Int)

-- | Make a socket wall of the given size, using the provided normal and
-- the given traversal.
mkSocketWall :: TerrainGrid -> Int -> V3 GLfloat -> WallTraverse -> [Vert_P_N_Tx.Vertex]
mkSocketWall terrainGrid verts normal trav = reverse $ go [] 0
    where
        go :: [Vert_P_N_Tx.Vertex] -> Int -> [Vert_P_N_Tx.Vertex]
        go xs idx
            | idx >= verts = xs -- Traversal done.
            | otherwise =
                let pos = TerrainGrid.lookup (trav verts idx) terrainGrid
                    (upper, lower) = mkVertexPair pos idx normal
                in go (lower:upper:xs) (idx + 1)

-- | West wall traversal. x = 0, z = idx.
westWallTraverse :: WallTraverse
westWallTraverse _verts idx = (0, idx)

-- | South wall traversal. x = idx, z = verts - 1.
southWallTraversal :: WallTraverse
southWallTraversal verts idx = (idx, verts - 1)

-- | East wall traversal. x = verts - 1. z = verts - 1 - idx.
eastWallTraversal :: WallTraverse
eastWallTraversal verts idx = (verts - 1, verts - 1 - idx)

-- | North wall traversal. x = verts - 1 - idx, z = 0.
northWallTraversal :: WallTraverse
northWallTraversal verts idx = (verts - 1 - idx, 0)

-- | Given the (vertex) index make two vertices. One at the same height as
-- the position and one at height zero.
mkVertexPair :: V3 GLfloat -> Int -> V3 GLfloat
             -> (Vert_P_N_Tx.Vertex, Vert_P_N_Tx.Vertex)
mkVertexPair pos@(V3 x _y z) idx normal
    | even idx =
        let upper = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = pos
                                       , Vert_P_N_Tx.normal = normal
                                       , Vert_P_N_Tx.texCoord = V2 0 1
                                       }
            lower = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x 0 z
                                       , Vert_P_N_Tx.normal = normal
                                       , Vert_P_N_Tx.texCoord = V2 0 0
                                       }
        in (upper, lower)
    | otherwise =
        let upper = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = pos
                                       , Vert_P_N_Tx.normal = normal
                                       , Vert_P_N_Tx.texCoord = V2 1 1
                                       }
            lower = Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 x 0 z
                                       , Vert_P_N_Tx.normal = normal
                                       , Vert_P_N_Tx.texCoord = V2 1 0
                                       }
        in (upper, lower)

-- | Generate the given number of indices.
indices :: Int -> [GLuint]
indices quads = concatMap mkQuadIndices [0 .. quads - 1]
    where
        mkQuadIndices :: Int -> [GLuint]
        mkQuadIndices quad =
            let baseIndex = fromIntegral $ quad * 2 + 2
            in [ baseIndex, baseIndex - 2, baseIndex - 1
               , baseIndex, baseIndex - 1, baseIndex + 1
               ]
