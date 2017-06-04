-- |
-- Module: Engine.Callback
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Callback
    ( install
    ) where

import           BigE.Math      (mkPerspective)
import           BigE.Runtime   (Key (..), ModifierKeys, Render,
                                 getAppStateUnsafe, modifyAppState, putAppState,
                                 setKeyPressedCallback, setKeyReleasedCallback,
                                 setWindowSizeCallback)
import           Engine.State   (State (..))
import           Graphics.Types (UserInput (..))

-- | Install callbacks.
install :: Render State ()
install = do
    setWindowSizeCallback (Just windowSizeCallback)
    setKeyPressedCallback (Just keyPressedCallback)
    setKeyReleasedCallback (Just keyReleasedCallback)

-- | Callback to handle application specific stuff when the window's size
-- is changed. Adjusting the viewport is handled automatically.
windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState $ \state ->
        state { perspective = mkPerspective (width, height) }

-- | Callback to handle user pressing keys.
keyPressedCallback :: Key -> ModifierKeys -> Render State ()

-- Handle W, wireframe activation.
keyPressedCallback Key'W _modifierKeys = do
    state <- getAppStateUnsafe
    let userInp = userInput state

    if renderWireframe userInp then
        putAppState $ state { userInput = userInp { renderWireframe = False }}
    else putAppState $ state { userInput = userInp { renderWireframe = True }}

-- Default - no - action.
keyPressedCallback _key _modifierKeys  = return ()

-- | Callback to handle user releasing keys.
keyReleasedCallback :: Key -> ModifierKeys -> Render State ()

-- Default - no - action.
keyReleasedCallback _key _modifierKeys  = return ()
