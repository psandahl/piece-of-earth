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

import qualified BigE.ImageMap          as ImageMap
import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import qualified BigE.TerrainGrid       as TerrainGrid
import           BigE.Types             (BufferUsage (..), Location,
                                         Primitive (..), Program,
                                         ShaderType (..), setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Vector            (fromList)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44)
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Terrain record.
data Terrain = Terrain
    { program :: !Program
      -- ^ The shader program for rendering of terrains.

    , mvpLoc  :: !Location

    , mesh    :: !Mesh
    } deriving Show

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' -> do
            mvpLoc' <- Program.getUniformLocation program' "mvp"
            mesh' <- dummyMesh

            return $
                Right Terrain { program = program'
                              , mvpLoc = mvpLoc'
                              , mesh = mesh'
                              }
        Left err -> return $ Left err

-- | Delete the terrain's resources.
delete :: MonadIO m => Terrain -> m ()
delete terrain =
    Program.delete $ program terrain

-- | Render all terrain given the perspective/view matrix.
render :: MonadIO m => M44 GLfloat -> Terrain -> m ()
render vp terrain = do
    Program.enable $ program terrain
    setUniform (mvpLoc terrain) vp
    Mesh.enable $ mesh terrain
    Mesh.render Triangles $ mesh terrain
    Mesh.disable
    Program.disable

-- | Load the program used for terrain rendering.
loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "terrain.vert"
        fragmentShader = resourceDir </> "shaders" </> "terrain.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]

dummyMesh :: MonadIO m => m Mesh
dummyMesh = do
    let Right imageMap = ImageMap.fromVector (5, 5) $
            fromList [ 0, 0, 0, 0, 0
                     , 0, 0, 0, 0, 0
                     , 0, 0, 0, 0, 0
                     , 0, 0, 0, 0, 0
                     , 0, 0, 0, 0, 0
                     ]
        Right terrainGrid = TerrainGrid.fromImageMap 1 imageMap
        (verts, indices) = TerrainGrid.asVertP terrainGrid
    Mesh.fromVector StaticDraw verts indices
