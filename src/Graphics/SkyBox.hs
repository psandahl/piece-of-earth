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

import           BigE.Attribute.Vert_P  (Vertex (..))
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime           (Render)
import           BigE.Types             (BufferUsage (..), Primitive (..),
                                         Program, ShaderType (..), setUniform)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Vector.Storable   (Vector)
import qualified Data.Vector.Storable   as Vector
import           Engine.State           (State, getPerspectiveMatrix,
                                         getViewMatrix)
import           Graphics.GL            (GLfloat, GLuint)
import           Graphics.Types         (SkyBox (..))
import           Linear                 (V3 (..), (!*!))
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the skybox given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String SkyBox)
init resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' -> do
            vpMatrixLoc' <- Program.getUniformLocation program' "vpMatrix"
            mesh' <- Mesh.fromVector StaticDraw skyBoxVertices skyBoxIndices

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
    vMatrix <- getViewMatrix
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

-- | The sky box is 2, 2, 2 big. Origin is 0, 0, 0.
skyBoxVertices :: Vector Vertex
skyBoxVertices = Vector.fromList
    [ -- Front upper right (0).
      Vertex { position = V3 len len (-len) }
      -- Front upper left (1).
    , Vertex { position = V3 (-len) len (-len) }
      -- Front lower left (2).
    , Vertex { position = V3 (-len) (-len) (-len) }
      -- Front lower right (3).
    , Vertex { position = V3 len (-len) (-len) }
      -- Back upper right (4).
    , Vertex { position = V3 len len len }
      -- Back upper left (5).
    , Vertex { position = V3 (-len) len len }
      -- Back lower left (6).
    , Vertex { position = V3 (-len) (-len) len }
      -- Back lower right (7).
    , Vertex { position = V3 len (-len) len }
    ]
    where
        len :: GLfloat
        len = 1

skyBoxIndices :: Vector GLuint
skyBoxIndices = Vector.fromList
    [ -- Front quad.
      0, 1, 2, 0, 2, 3
      -- Left quad.
    , 1, 5, 6, 1, 6, 2
      -- Right quad.
    , 4, 0, 3, 4, 3, 7
      -- Back quad.
    , 5, 4, 7, 5, 7, 6
      -- Top quad.
    , 4, 5, 1, 4, 1, 0
      -- Bottom quad.
    , 3, 2, 6, 3, 6, 7
    ]
