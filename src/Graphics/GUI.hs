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
    , requestCenterFlash
    ) where

import           BigE.Runtime           (Render, frameDuration,
                                         getAppStateUnsafe, putAppState)
import           BigE.TextRenderer      (Position (..), RenderParams (..))
import qualified BigE.TextRenderer      as TextRenderer
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import qualified BigE.TextRenderer.Text as Text
import           BigE.Util              (eitherTwo)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (fromJust, isJust)
import           Engine.State           (State (gui))
import           Graphics.Types         (GUI (..), TextEntity (..))
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
        Right (textRenderer', centerFlashFont') ->

            return $
                Right GUI { textRenderer = textRenderer'
                          , centerFlashFont = centerFlashFont'
                          , centerFlash = Nothing
                          }

        -- Nope. Didn't manage to init.
        Left err -> return $ Left err

-- | Delete the GUI's resources.
delete :: GUI -> Render State ()
delete gui' = do
    maybe (return ()) (Text.delete . text) $ centerFlash gui'
    Font.delete $ centerFlashFont gui'
    TextRenderer.delete $ textRenderer gui'

-- | Animate the GUI.
animate :: GUI -> Render State GUI
animate = animateCenterFlash

animateCenterFlash :: GUI -> Render State GUI
animateCenterFlash gui'
    | isJust (centerFlash gui') = do
        frameTime <- realToFrac <$> frameDuration
        let centerFlash' = fromJust $ centerFlash gui'
            decSpeed = 1.5
            currAlpha = alpha $ renderParams centerFlash'
            newAlpha = currAlpha - frameTime * decSpeed
        if newAlpha <= 0 then
            do Text.delete (text centerFlash')
               return gui' { centerFlash = Nothing }
        else
            do let newParams = (renderParams centerFlash') { alpha = newAlpha }
                   newFlash = centerFlash' { renderParams = newParams }
               return gui' { centerFlash = Just newFlash }

    -- No active center flash.
    | otherwise = return gui'

-- | Render the GUI.
render :: GUI -> Render State ()
render gui' = do
    let mCenterFlash = centerFlash gui'
    when (isJust mCenterFlash) $ do
        let centerFlash' = fromJust mCenterFlash
        TextRenderer.render (text centerFlash')
                            (renderParams centerFlash')
                            (textRenderer gui')

-- | Request a new center flash message. If there already is an active message
-- that will be replaced.
requestCenterFlash :: String -> Render State ()
requestCenterFlash str = do
    state <- getAppStateUnsafe
    let mCenterFlash = centerFlash $ gui state

    when (isJust mCenterFlash) $ do
        let centerFlash' = fromJust mCenterFlash
        Text.delete $ text centerFlash'

    centerFlash' <- newCenterFlash str $ gui state
    let gui' = (gui state) { centerFlash = Just centerFlash' }

    putAppState state { gui = gui' }

-- | Load the center flash font from the resource directory.
loadCenterFlashFont :: MonadIO m => FilePath -> m (Either String Font)
loadCenterFlashFont resourceDir = do
    let fontFile = resourceDir </> "fonts" </> "purisa-70.fnt"
    Font.fromFile fontFile

-- | Creata a new 'TextEntity' for the center flash.
newCenterFlash :: MonadIO m => String -> GUI -> m TextEntity
newCenterFlash str gui' = do
    text' <- Text.init (centerFlashFont gui') str
    return TextEntity { text = text', renderParams = centerFlashRenderParams }
    where
        centerFlashRenderParams :: RenderParams
        centerFlashRenderParams =
            TextRenderer.defaultRenderParams
                { size = 26
                , position = CenterAt 0 0
                , color = V3 1 1 0
                , alpha = 2 -- Value will be clamped to 1. But neat trick for animation.
                }
