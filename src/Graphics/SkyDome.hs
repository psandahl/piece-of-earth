-- |
-- Module: Graphics.SkyDome
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.SkyDome
    ( init
    , render
    , delete
    ) where

import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Model             as Model
import qualified BigE.Program           as Program
import           BigE.Runtime           (Render)
import           BigE.Types             (BufferUsage (..), Primitive (..),
                                         Program, ShaderType (..), setUniform)
import           BigE.Util              (eitherTwo)
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State, getPerspectiveMatrix,
                                         getTimeOfDay, getViewMatrix)
import           Graphics.GL            (GLfloat)
import           Graphics.Types         (SkyDome (..))
import           Linear                 (M44, V4 (..), (!*!))
import           Prelude                hiding (init)
import           Simulation.Atmosphere  (SkyGradient (..), fogColor,
                                         skyGradient)
import           System.FilePath        ((</>))

-- | Initialize the sky dome given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String SkyDome)
init resourceDir = do
    eProgram <- loadProgram resourceDir
    eMesh <- loadMesh resourceDir

    case eitherTwo (eProgram, eMesh) of
        Right (program', mesh') -> do
            vpMatrixLoc' <- Program.getUniformLocation program' "vpMatrix"
            horizonLoc' <- Program.getUniformLocation program' "horizon"
            skyLoc' <- Program.getUniformLocation program' "sky"
            fogColorLoc' <- Program.getUniformLocation program' "fogColor"

            return $ Right SkyDome
                { program = program'
                , vpMatrixLoc = vpMatrixLoc'
                , horizonLoc = horizonLoc'
                , skyLoc = skyLoc'
                , fogColorLoc = fogColorLoc'
                , mesh = mesh'
                }

        Left err -> return $ Left err

-- | Render the 'SkyDome'.
render :: SkyDome -> Render State ()
render skyDome = do
    Program.enable $ program skyDome

    -- Set uniforms.
    pMatrix <- getPerspectiveMatrix
    vMatrix <- removeTranslation <$> getViewMatrix
    setUniform (vpMatrixLoc skyDome) $ pMatrix !*! vMatrix

    skyGradients <- skyGradient <$> getTimeOfDay
    setUniform (horizonLoc skyDome) $ horizon skyGradients
    setUniform (skyLoc skyDome) $ sky skyGradients
    setUniform (fogColorLoc skyDome) fogColor

    -- Render stuff.
    Mesh.enable $ mesh skyDome
    Mesh.render Triangles $ mesh skyDome

    -- Clean up
    Mesh.disable
    Program.disable

-- | Delete the 'SkyDome' resources.
delete :: SkyDome -> Render State ()
delete = Program.delete . program

-- | Load the program used for 'SkyDome' rendering.
loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "skydome.vert"
        fragmentShader = resourceDir </> "shaders" </> "skydome.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]

-- | Load the model used for 'SkyDome' mesh.
loadMesh :: MonadIO m => FilePath -> m (Either String Mesh)
loadMesh resourceDir = do
    let modelFile = resourceDir </> "models" </> "sphere.obj"

    eModelVectors <- Model.vertPFromFile modelFile
    case eModelVectors of
        Right (verts, indices) ->
            Right <$> Mesh.fromVector StaticDraw verts indices

        Left err -> return $ Left err

removeTranslation :: M44 GLfloat -> M44 GLfloat
removeTranslation (V4 (V4 x1 y1 z1 _)
                      (V4 x2 y2 z2 _)
                      (V4 x3 y3 z3 _)
                      (V4 x4 y4 z4 w4)) =
    V4 (V4 x1 y1 z1 0)
       (V4 x2 y2 z2 0)
       (V4 x3 y3 z3 0)
       (V4 x4 y4 z4 w4)
