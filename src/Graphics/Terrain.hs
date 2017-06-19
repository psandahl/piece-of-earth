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
    ( init
    , delete
    , render
    , terrainHeight
    ) where

import           BigE.ImageMap          (ImageMap, PixelRGB8 (..),
                                         VectorSpec (..))
import qualified BigE.ImageMap          as ImageMap
import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime           (Render)
import           BigE.TerrainGrid       (TerrainGrid)
import qualified BigE.TerrainGrid       as TerrainGrid
import           BigE.Types             (BufferUsage (..), Primitive (..),
                                         Program, ShaderType (..), setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Vector            (fromList)
import           Engine.State           (State)
import           Graphics.GL            (GLfloat)
import           Graphics.Types         (Terrain (..))
import           Linear                 (M44)
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' -> do
            mvpLoc' <- Program.getUniformLocation program' "mvp"
            (terrainGrid', mesh') <- dummyMesh

            return $
                Right Terrain { program = program'
                              , mvpLoc = mvpLoc'
                              , terrainGrid = terrainGrid'
                              , mesh = mesh'
                              }
        Left err -> return $ Left err

-- | Delete the terrain's resources.
delete :: Terrain -> Render State ()
delete terrain =
    Program.delete $ program terrain

-- | Render all terrain given the perspective/view matrix.
render :: M44 GLfloat -> Terrain -> Render State ()
render vp terrain = do
    Program.enable $ program terrain
    setUniform (mvpLoc terrain) vp
    Mesh.enable $ mesh terrain
    Mesh.render Triangles $ mesh terrain
    Mesh.disable
    Program.disable

-- | Get the terrain height for the given x z spot in model space.
terrainHeight :: (GLfloat, GLfloat) -> Terrain -> GLfloat
terrainHeight pos = TerrainGrid.terrainHeight pos . terrainGrid

-- | Load the program used for terrain rendering.
loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "terrain.vert"
        fragmentShader = resourceDir </> "shaders" </> "terrain.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]

-- | Everything below is just dummy. No fault handling etc.
dummyMesh :: MonadIO m => m (TerrainGrid, Mesh)
dummyMesh = do
    let Right terrainGrid' = TerrainGrid.fromImageMap 3 dummyHeightMap
        Right (verts, indices) = TerrainGrid.asVertPNTxC dummyColorMap terrainGrid'
    mesh' <- Mesh.fromVector StaticDraw verts indices
    return (terrainGrid', mesh')

dummyHeightMap :: ImageMap
dummyHeightMap =
    let Right heightMap = ImageMap.fromVector (Raw16Vector (7, 7) $
            fromList [ 0, 0, 0, 0, 0, 0, 0
                     , 0, 0, 0, 1, 0, 0, 0
                     , 0, 0, 0, 2, 0, 0, 0
                     , 0, 1, 2, 3, 2, 1, 0
                     , 0, 0, 0, 2, 0, 0, 0
                     , 0, 0, 0, 1, 0, 0, 0
                     , 0, 0, 0, 0, 0, 0, 0
                     ])
    in heightMap

dummyColorMap :: ImageMap
dummyColorMap =
    let Right colorMap = ImageMap.fromVector (RGBVector (7, 7) $
            fromList
                [ red, red, red, red, red, red, red
                , red, red, red, green, red, red, red
                , red, red, red, blue, red, red, red
                , red, green, blue, white, blue, green, red
                , red, red, red, blue, red, red, red
                , red, red, red, green, red, red, red
                , red, red, red, red, red, red, red
                ])
    in colorMap

red :: PixelRGB8
red = PixelRGB8 255 0 0

green :: PixelRGB8
green = PixelRGB8 0 255 0

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

white :: PixelRGB8
white = PixelRGB8 255 255 255
