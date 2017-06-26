-- |
-- Module: Graphics.SkyBox
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.SkyBox
    ( init
    , delete
    ) where

import qualified BigE.Program           as Program
import           BigE.Runtime           (Render)
import           BigE.Types             (Program, ShaderType (..))
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Graphics.Types         (SkyBox (..))
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the skybox given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String SkyBox)
init resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' -> do
            mvpMatrixLoc' <- Program.getUniformLocation program' "mvp"

            return $ Right SkyBox
                { program = program'
                , mvpMatrixLoc = mvpMatrixLoc'
                }

        Left err -> return $ Left err

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
