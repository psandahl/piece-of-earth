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
import           BigE.TextRenderer      (Position (..), RenderParams (..))
import qualified BigE.TextRenderer      as TextRenderer
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import qualified BigE.TextRenderer.Text as Text
import           BigE.Util              (eitherTwo)
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Graphics.Types         (GUI (..))
import           Linear                 (V3 (..))
import           Prelude                hiding (init)
import           System.FilePath        ((</>))

-- | Initialize the GUI given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String GUI)
init resourceDir = do
    -- Start with things that can fail.
    eTextRenderer <- TextRenderer.init
    eCenterFlashFont <- loadCenterFlashFont resourceDir

    case eitherTwo (eTextRenderer, eCenterFlashFont) of
        Right (textRenderer', centerFlashFont') -> do
            centerFlashText' <- Text.init centerFlashFont' "[+Wireframe]"

            return $
                Right GUI { textRenderer = textRenderer'
                          , centerFlashFont = centerFlashFont'
                          , centerFlashParams = centerFlashRenderParams
                          , centerFlashText = centerFlashText'
                          }

        -- Nope. Didn't manage to init.
        Left err -> return $ Left err

-- | Delete the GUI's resources.
delete :: GUI -> Render State ()
delete gui = do
    Text.delete $ centerFlashText gui
    Font.delete $ centerFlashFont gui
    TextRenderer.delete $ textRenderer gui

-- | Animate the GUI.
animate :: GUI -> Render State GUI
animate gui = return gui

-- | Render the GUI.
render :: GUI -> Render State ()
render gui =
    TextRenderer.render (centerFlashText gui) (centerFlashParams gui) (textRenderer gui)

setCenterFlash :: String -> GUI -> GUI
setCenterFlash = undefined

-- | Load the center flash font from the resource directory.
loadCenterFlashFont :: MonadIO m => FilePath -> m (Either String Font)
loadCenterFlashFont resourceDir = do
    let fontFile = resourceDir </> "fonts" </> "purisa-70.fnt"
    Font.fromFile fontFile

-- | Setting 'RenderParams' for the center flash.
centerFlashRenderParams :: RenderParams
centerFlashRenderParams =
    TextRenderer.defaultRenderParams
        { size = 26
        , position = CenterAt 0 0
        , color = V3 1 1 0
        , alpha = 1
        }
