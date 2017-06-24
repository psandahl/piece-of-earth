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

import           BigE.ImageMap                (ImageMap, PixelRGB8 (..),
                                               VectorSpec (..))
import qualified BigE.ImageMap                as ImageMap
import           BigE.Mesh                    (Mesh)
import qualified BigE.Mesh                    as Mesh
import qualified BigE.Program                 as Program
import           BigE.Runtime                 (Render, getAppStateUnsafe)
import           BigE.TerrainGrid             (TerrainGrid)
import qualified BigE.TerrainGrid             as TerrainGrid
import qualified BigE.Texture                 as Texture
import           BigE.Types                   (BufferUsage (..), Primitive (..),
                                               Program, ShaderType (..),
                                               Texture, setUniform)
import           BigE.Util                    (eitherTwo)
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Vector                  (fromList)
import           Engine.State                 (State (ambientLight))
import           Graphics.GL                  (GLfloat, GLint)
import           Graphics.Lights.AmbientLight (getAmbientLightLoc,
                                               setAmbientLight)
import           Graphics.Types               (Terrain (..))
import           Linear                       (M44)
import           Prelude                      hiding (init)
import           System.FilePath              ((</>))

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init resourceDir = do
    eProgram <- loadProgram resourceDir
    eGroundTexture <- loadGroundTexture resourceDir

    case eitherTwo (eProgram, eGroundTexture) of
        Right (program', groundTexture') -> do
            mvpLoc' <- Program.getUniformLocation program' "mvp"
            groundTextureLoc' <- Program.getUniformLocation program' "groundTexture"
            ambientLightLoc' <- getAmbientLightLoc program'
            (terrainGrid', mesh') <- dummyMesh

            return $
                Right Terrain { program = program'
                              , mvpLoc = mvpLoc'
                              , groundTextureLoc = groundTextureLoc'
                              , ambientLightLoc = ambientLightLoc'
                              , terrainGrid = terrainGrid'
                              , groundTexture = groundTexture'
                              , mesh = mesh'
                              }
        Left err -> return $ Left err

-- | Delete the terrain's resources.
delete :: Terrain -> Render State ()
delete terrain = do
    Program.delete $ program terrain
    Texture.delete $ groundTexture terrain

-- | Render all terrain given the perspective/view matrix.
render :: M44 GLfloat -> Terrain -> Render State ()
render vp terrain = do
    Program.enable $ program terrain
    Texture.enable2D 0 $ groundTexture terrain

    -- Set uniforms.
    setUniform (mvpLoc terrain) vp
    setUniform (groundTextureLoc terrain) (0 :: GLint)
    ambientLight' <- ambientLight <$> getAppStateUnsafe
    setAmbientLight ambientLight' $ ambientLightLoc terrain

    -- Render stuff.
    Mesh.enable $ mesh terrain
    Mesh.render Triangles $ mesh terrain

    -- Clean up.
    Mesh.disable
    Texture.disable2D 0
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

-- | Load the texture used for ground structure.
loadGroundTexture :: MonadIO m => FilePath -> m (Either String Texture)
loadGroundTexture resourceDir = do
    let file = resourceDir </> "textures" </> "unwrap_helper.jpg"
    Texture.fromFile2D file Texture.defaultParams2D

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
                     , 1, 1, 1, 1, 1, 1, 1
                     , 2, 2, 2, 2, 2, 2, 2
                     , 3, 3, 3, 3, 3, 3, 3
                     , 2, 2, 2, 2, 2, 2, 2
                     , 1, 1, 1, 1, 1, 1, 1
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
