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
import           Graphics.GUI   (requestCenterFlash)
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

-- Handle F1, wireframe toggle.
keyPressedCallback Key'F1 _modifierKeys = do
    state <- getAppStateUnsafe
    let userInp = userInput state

    if renderWireframe userInp then
        do putAppState $ state { userInput = userInp { renderWireframe = False }}
           requestCenterFlash "[-wireframe]"
    else
        do putAppState $ state { userInput = userInp { renderWireframe = True }}
           requestCenterFlash "[+wireframe]"

-- Handle F2, status bar toggle.
keyPressedCallback Key'F2 _modifierKeys = do
    state <- getAppStateUnsafe
    let userInp = userInput state

    if renderStatusBar userInp then
        do putAppState $ state { userInput = userInp { renderStatusBar = False }}
           requestCenterFlash "[-status bar]"
    else
        do putAppState $ state { userInput = userInp { renderStatusBar = True }}
           requestCenterFlash "[+status bar]"

-- Handle left arrow, activate turning left.
keyPressedCallback Key'Left _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnLeft = True }

-- Handle right arrow, activate turning right.
keyPressedCallback Key'Right _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnRight = True }

-- Default - no - action.
keyPressedCallback _key _modifierKeys  = return ()

-- | Callback to handle user releasing keys.
keyReleasedCallback :: Key -> ModifierKeys -> Render State ()

-- Handle left arrow, deactivate turning left.
keyReleasedCallback Key'Left _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnLeft = False }

-- Handle left arrow, deactivate turning right.
keyReleasedCallback Key'Right _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnRight = False }

-- Default - no - action.
keyReleasedCallback _key _modifierKeys  = return ()

-- Utility function to modify the state's 'UserInput'.
modifyUserInput :: (UserInput -> UserInput) -> Render State ()
modifyUserInput g =
    modifyAppState $ \state ->
        state { userInput = g (userInput state) }
