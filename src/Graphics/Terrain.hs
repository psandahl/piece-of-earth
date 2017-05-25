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

import qualified BigE.Program           as Program
import           BigE.Types             (Program, ShaderType (..))
import           Control.Monad.IO.Class (MonadIO)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44)
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Terrain record.
data Terrain = Terrain
    { program :: !Program
      -- ^ The shader program for rendering of terrains.
    } deriving Show

-- | Initialize the terrain given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String Terrain)
init resourceDir = do
    eProgram <- loadProgram resourceDir

    case eProgram of
        Right program' ->
            return $ Right Terrain { program = program' }
        Left err -> return $ Left err

-- | Delete the terrains resources.
delete :: MonadIO m => Terrain -> m ()
delete terrain =
    Program.delete $ program terrain

-- | Render all terrain given the perspective/view matrix.
render :: MonadIO m => M44 GLfloat -> m ()
render = undefined

loadProgram :: MonadIO m => FilePath -> m (Either String Program)
loadProgram resourceDir = do
    let vertexShader = resourceDir </> "shaders" </> "terrain.vert"
        fragmentShader = resourceDir </> "shaders" </> "terrain.frag"
    Program.fromFile [ (VertexShader, vertexShader)
                     , (FragmentShader, fragmentShader)
                     ]
