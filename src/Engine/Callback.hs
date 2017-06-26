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

import           BigE.Math             (mkPerspective)
import           BigE.Runtime          (Key (..), ModifierKeys (..), Render,
                                        getAppStateUnsafe, modifyAppState,
                                        putAppState, setKeyPressedCallback,
                                        setKeyReleasedCallback,
                                        setWindowSizeCallback)
import           Engine.State          (State (..), setTimeOfDay)
import           Graphics.GUI          (requestCenterFlash)
import           Graphics.Types        (Camera (pitch), UserInput (..))
import           Simulation.Atmosphere (TimeOfDay (..))

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

keyPressedCallback Key'F1 modifierKeys
      -- Handle Shift-F1. Set lightning to sunrise.
    | shift modifierKeys = do
        setTimeOfDay Sunrise
        requestCenterFlash "[timeOfDay: sunrise]"

      -- Handle F1, wireframe toggle.
    | otherwise = do
        state <- getAppStateUnsafe
        let userInp = userInput state

        if renderWireframe userInp then
            do putAppState $ state { userInput = userInp { renderWireframe = False }}
               requestCenterFlash "[-wireframe]"
        else
            do putAppState $ state { userInput = userInp { renderWireframe = True }}
               requestCenterFlash "[+wireframe]"

keyPressedCallback Key'F2 modifierKeys
      -- Handle Shift-F2. Set lightning to morning.
    | shift modifierKeys = do
        setTimeOfDay Morning
        requestCenterFlash "[timeOfDay : morning]"
    | otherwise = do

      -- Handle F2, status bar toggle.
        state <- getAppStateUnsafe
        let userInp = userInput state

        if renderStatusBar userInp then
            do putAppState $ state { userInput = userInp { renderStatusBar = False }}
               requestCenterFlash "[-status bar]"
        else
            do putAppState $ state { userInput = userInp { renderStatusBar = True }}
               requestCenterFlash "[+status bar]"

keyPressedCallback Key'F3 modifierKeys
      -- Handle Shift-F3. Set lightning to noon.
    | shift modifierKeys = do
        setTimeOfDay Noon
        requestCenterFlash "[timeOfDay: noon]"

    | otherwise = do
        -- Handle F3, terrain collision toggle.
        state <- getAppStateUnsafe
        let userInp = userInput state

        if terrainCollision userInp then
            do putAppState $ state { userInput = userInp { terrainCollision = False }}
               requestCenterFlash "[-terrain collision]"
        else
            do putAppState $ state { userInput = userInp { terrainCollision = True }}
               requestCenterFlash "[+terrain collision]"

keyPressedCallback Key'F4 modifierKeys
      -- Handle Shift-F4. Set lightning to afternoon.
    | shift modifierKeys = do
        setTimeOfDay Afternoon
        requestCenterFlash "[timeOfDay: afternoon]"

    | otherwise = do

      -- Handle F4, fly mode toggle.
      state <- getAppStateUnsafe
      let userInp = userInput state

      if flyMode userInp then
          do putAppState $ state { userInput = userInp { flyMode = False }}
             requestCenterFlash "[-fly mode]"
      else
          do putAppState $ state { userInput = userInp { flyMode = True }}
             requestCenterFlash "[+fly mode. w=up, s=down]"

keyPressedCallback Key'F5 modifierKeys
      -- Handle Shift-F5. Set lightning to sunset.
    | shift modifierKeys = do
        setTimeOfDay Sunset
        requestCenterFlash "[timeOfDay: sunset]"
    | otherwise = return ()

keyPressedCallback Key'F6 modifierKeys
      -- Handle Shift-F6. Set lightning to night.
    | shift modifierKeys = do
        setTimeOfDay Night
        requestCenterFlash "[timeOfDay: night]"
    | otherwise = return ()

-- Handle left arrow, activate turning left.
keyPressedCallback Key'Left _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnLeft = True }

-- Handle right arrow, activate turning right.
keyPressedCallback Key'Right _modifierKeys =
    modifyUserInput $ \userInp -> userInp { turnRight = True }

-- Handle up arrow, activate go forward.
keyPressedCallback Key'Up _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goForward = True }

-- Handle down arrow, activate go backward.
keyPressedCallback Key'Down _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goBackward = True }

-- Handle pgup, activate look up.
keyPressedCallback Key'PageUp _modifierKeys =
    modifyUserInput $ \userInp -> userInp { lookUp = True }

--  Handle pgdn, activate look down.
keyPressedCallback Key'PageDown _modifierKeys =
    modifyUserInput $ \userInp -> userInp { lookDown = True }

-- Handle w, activate go up.
keyPressedCallback Key'W _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goUp = True }

-- Handle s, activate go down.
keyPressedCallback Key'S _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goDown = True }

-- Handle home, force camera pitch to zero.
keyPressedCallback Key'Home _modifierKeys =
    modifyAppState $ \state ->
        state { camera = (camera state) { pitch = 0 } }

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

-- Handle up arrow, deacativate go forward.
keyReleasedCallback Key'Up _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goForward = False }

-- Handle down arrow, deactivate go backward.
keyReleasedCallback Key'Down _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goBackward = False }

-- Handle pgup, deactivate look up.
keyReleasedCallback Key'PageUp _modifierKeys =
    modifyUserInput $ \userInp -> userInp { lookUp = False }

-- Handle pgdn, deactivate look down.
keyReleasedCallback Key'PageDown _modifierKeys =
    modifyUserInput $ \userInp -> userInp { lookDown = False }

-- Handle w, deactivate go up.
keyReleasedCallback Key'W _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goUp = False }

-- Handle s, deactivate go down.
keyReleasedCallback Key'S _modifierKeys =
    modifyUserInput $ \userInp -> userInp { goDown = False }

-- Default - no - action.
keyReleasedCallback _key _modifierKeys  = return ()

-- Utility function to modify the state's 'UserInput'.
modifyUserInput :: (UserInput -> UserInput) -> Render State ()
modifyUserInput g =
    modifyAppState $ \state ->
        state { userInput = g (userInput state) }

shift :: ModifierKeys -> Bool
shift modifierKeys =
    modifierKeysShift modifierKeys &&
    not (modifierKeysAlt modifierKeys) &&
    not (modifierKeysControl modifierKeys) &&
    not (modifierKeysSuper modifierKeys)
