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

import           BigE.ImageMap                (FileSpec (..), ImageMap)
import qualified BigE.ImageMap                as ImageMap
import           BigE.Mesh                    (Mesh)
import qualified BigE.Mesh                    as Mesh
import qualified BigE.Program                 as Program
import           BigE.Runtime                 (Render)
import           BigE.TerrainGrid             (TerrainGrid)
import qualified BigE.TerrainGrid             as TerrainGrid
import           BigE.Texture                 (TextureParameters (lodBias))
import qualified BigE.Texture                 as Texture
import           BigE.Types                   (BufferUsage (..), Primitive (..),
                                               Program, ShaderType (..),
                                               Texture, setUniform)
import           BigE.Util                    (eitherThree, eitherTwo)
import           Control.Monad.IO.Class       (MonadIO)
import           Engine.State                 (State, getPerspectiveMatrix,
                                               getTimeOfDay, getViewMatrix)
import           Graphics.Fog                 (getFogLoc, setFog)
import           Graphics.GL                  (GLfloat, GLint)
import           Graphics.Lights.AmbientLight (getAmbientLightLoc,
                                               setAmbientLight)
import           Graphics.Lights.LightEmitter (getLightEmitterLoc,
                                               setLightEmitter)
import           Graphics.Lights.Material     (Material (..), getMaterialLoc,
                                               setMaterial)
import           Graphics.Types               (Terrain (..))
import           Linear                       (identity, (!*!))
import           Prelude                      hiding (init)
import           Simulation.Atmosphere        (ambientLight, fog, sunLight)
import           System.FilePath              ((</>))

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init resourceDir = do
    eProgram <- loadProgram resourceDir
    eGroundTexture <- loadGroundTexture resourceDir "dirt.tga"
    eModel <- loadMesh resourceDir
                       "ter1-1025x1025.bmp"
                       "ter1-1025x1025.r16"
                       (1025, 1025) 300
                       --"ter2-513x513.png"
                       --"ter2-513x513.r16"
                       --(513, 513) 300

    case eitherThree (eProgram, eGroundTexture, eModel) of
        Right (program', groundTexture', (terrainGrid', mesh')) -> do
            mvpMatrixLoc' <- Program.getUniformLocation program' "mvpMatrix"
            mvMatrixLoc' <- Program.getUniformLocation program' "mvMatrix"
            vMatrixLoc' <- Program.getUniformLocation program' "vMatrix"
            groundTextureLoc' <- Program.getUniformLocation program' "groundTexture"
            ambientLightLoc' <- getAmbientLightLoc program' "ambientLight"
            sunLightLoc' <- getLightEmitterLoc program' "sunLight"
            materialLoc' <- getMaterialLoc program' "material"
            fogLoc' <- getFogLoc program' "fog"

            return $
                Right Terrain { program = program'
                              , modelMatrix = identity
                              , mvpMatrixLoc = mvpMatrixLoc'
                              , mvMatrixLoc = mvMatrixLoc'
                              , vMatrixLoc = vMatrixLoc'
                              , groundTextureLoc = groundTextureLoc'
                              , ambientLightLoc = ambientLightLoc'
                              , sunLightLoc = sunLightLoc'
                              , material = Material { shine = 16
                                                    , strength = 0.8
                                                    }
                              , materialLoc = materialLoc'
                              , fogLoc = fogLoc'
                              , terrainGrid = terrainGrid'
                              , groundTexture = groundTexture'
                              , mesh = mesh'
                              }
        Left err -> return $ Left err

-- | Delete the terrain's resources.
delete :: Terrain -> Render State ()
delete terrain = do
    Program.delete $ program terrain
    Mesh.delete $ mesh terrain
    Texture.delete $ groundTexture terrain

-- | Render all terrain.
render :: Terrain -> Render State ()
render terrain = do
    Program.enable $ program terrain
    Texture.enable2D 0 $ groundTexture terrain

    -- Set uniforms.
    pMatrix <- getPerspectiveMatrix
    vMatrix <- getViewMatrix
    let mvMatrix = vMatrix !*! modelMatrix terrain
        mvpMatrix = pMatrix !*! mvMatrix

    setUniform (mvpMatrixLoc terrain) mvpMatrix
    setUniform (mvMatrixLoc terrain) mvMatrix
    setUniform (groundTextureLoc terrain) (0 :: GLint)
    setUniform (vMatrixLoc terrain) vMatrix

    timeOfDay <- getTimeOfDay
    setAmbientLight (ambientLightLoc terrain) ambientLight
    setLightEmitter (sunLightLoc terrain) $ sunLight timeOfDay
    setMaterial (materialLoc terrain) $ material terrain
    setFog (fogLoc terrain) fog

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
loadGroundTexture :: MonadIO m => FilePath -> FilePath -> m (Either String Texture)
loadGroundTexture resourceDir file = do
    let path = resourceDir </> "textures" </> file
    Texture.fromFile2D path $ Texture.defaultParams2D { lodBias = -0.2 }

-- | Load a 'Mesh' and make a 'TerrainGrid' from the external files in the
-- resource directory.
loadMesh :: MonadIO m => FilePath -> FilePath -> FilePath
         -> (Int, Int) -> Float -> m (Either String (TerrainGrid, Mesh))
loadMesh resourceDir colorFile heightFile heightDimensions heightScale = do

    -- Load maps.
    eColorMap <- loadColorMap resourceDir colorFile
    eHeightMap <- loadRawHeightMap resourceDir heightFile heightDimensions
    case eitherTwo (eColorMap, eHeightMap) of

        -- Succesful loading maps.
        Right (colorMap, heightMap) ->

            -- Convert the height map to a terrain grid.
            case TerrainGrid.fromImageMap heightScale heightMap of
                Right terrainGrid' ->

                    -- Export the grid to a mesh, using a color map.
                    case TerrainGrid.asVertPNTxC colorMap terrainGrid' of
                        Right (verts, indices) -> do

                            -- Yay. Done!
                            mesh' <- Mesh.fromVector StaticDraw verts indices
                            return $ Right (terrainGrid', mesh')

                        -- Failed exporting to a mesh.
                        Left err -> return $ Left err

                -- Failed conversion to terrain grid.
                Left err          -> return $ Left err

        -- Failed loading maps.
        Left err                    -> return $ Left err

-- | Load a heightmap from raw file.
loadRawHeightMap :: MonadIO m => FilePath -> FilePath -> (Int, Int)
                 -> m (Either String ImageMap)
loadRawHeightMap resourceDir file dimensions = do
    let path = resourceDir </> "heightmaps" </> file
        spec = Raw16File dimensions path
    ImageMap.fromFile spec

-- Load a color map from a RGB file.
loadColorMap :: MonadIO m => FilePath -> FilePath -> m (Either String ImageMap)
loadColorMap resourceDir file = do
    let path = resourceDir </> "colormaps" </> file
        spec = RGB8File path
    ImageMap.fromFile spec
{-}
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
                [ red, blue, red, blue, red, blue, red
                , red, red, blue, green, blue, red, red
                , red, red, red, blue, red, red, red
                , red, green, blue, white, blue, green, red
                , red, red, red, blue, red, red, red
                , red, red, blue, green, blue, red, red
                , red, blue, red, blue, red, blue, red
                ])
    in colorMap

red :: PixelRGB8
red = PixelRGB8 100 0 0

green :: PixelRGB8
green = PixelRGB8 0 100 0

blue :: PixelRGB8
blue = PixelRGB8 0 0 100

white :: PixelRGB8
white = PixelRGB8 255 255 255
-}
