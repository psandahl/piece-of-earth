-- |
-- Module: Graphics.GUI
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.GUI
    ( init
    , delete
    , animate
    , render
    , setCenterFlash
    ) where

import           BigE.Runtime           (Render)
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Graphics.Types         (GUI (..))
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the GUI given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String GUI)
init resourceDir = do
    eCenterFlashFont <- loadCenterFlashFont resourceDir
    case eCenterFlashFont of
        Right centerFlashFont' ->
            return $
                Right GUI { centerFlashFont = centerFlashFont' }

        Left err -> return $ Left err

-- | Delete the GUI's resources.
delete :: GUI -> Render State ()
delete gui =
    Font.delete $ centerFlashFont gui

-- | Animate the GUI.
animate :: GUI -> Render State GUI
animate gui = return gui

render :: GUI -> Render State ()
render _ = return ()

setCenterFlash :: String -> GUI -> GUI
setCenterFlash = undefined

-- | Load the center flash font from the resource directory.
loadCenterFlashFont :: MonadIO m => FilePath -> m (Either String Font)
loadCenterFlashFont resourceDir = do
    let fontFile = resourceDir </> "fonts" </> "purisa-70.fnt"
    Font.fromFile fontFile
