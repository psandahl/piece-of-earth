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
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Graphics.Types         (GUI (..))
import           Prelude                hiding (init)

-- | Initialize the GUI given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String GUI)
init _ = return (Right GUI)

-- | Delete the GUI's resources.
delete :: GUI -> Render State ()
delete _ = return ()

-- | Animate the GUI.
animate :: GUI -> Render State ()
animate _ = return ()

render :: GUI -> Render State ()
render _ = return ()

setCenterFlash :: String -> GUI -> GUI
setCenterFlash = undefined
