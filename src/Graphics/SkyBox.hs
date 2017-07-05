-- |
-- Module: Graphics.SkyBox
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.SkyBox
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
                                         getViewMatrix)
import           Graphics.GL            (GLfloat)
import           Graphics.Types         (SkyBox (..))
import           Linear                 (M44, V4 (..), (!*!))
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the skybox given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String SkyBox)
init resourceDir = do
    eProgram <- loadProgram resourceDir
    eMesh <- loadMesh resourceDir

    case eitherTwo (eProgram, eMesh) of
        Right (program', mesh') -> do
            vpMatrixLoc' <- Program.getUniformLocation program' "vpMatrix"

            return $ Right SkyBox
                { program = program'
                , vpMatrixLoc = vpMatrixLoc'
                , mesh = mesh'
                }

        Left err -> return $ Left err

-- | Render the 'SkyBox'.
render :: SkyBox -> Render State ()
render skyBox = do
    Program.enable $ program skyBox

    -- Set uniforms.
    pMatrix <- getPerspectiveMatrix
    vMatrix <- removeTranslation <$> getViewMatrix
    setUniform (vpMatrixLoc skyBox) $ pMatrix !*! vMatrix

    -- Render stuff.
    Mesh.enable $ mesh skyBox
    Mesh.render Triangles $ mesh skyBox

    -- Clean up
    Mesh.disable
    Program.disable

-- | Delete the 'SkyBox' resources.
delete :: SkyBox -> Render State ()
delete = Program.delete . program

-- | Load the program used for 'SkyBox' rendering.
loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "skybox.vert"
        fragmentShader = resourceDir </> "shaders" </> "skybox.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]

-- | Load the model used for 'SkyBox' mesh.
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
