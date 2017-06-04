-- |
-- Module: Graphics.GUI
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Graphics.GUI
    ( GUI
    , init
    , delete
    , animate
    , render
    , setCenterFlash
    ) where

import           BigE.Runtime           (Render)
import           Control.Monad.IO.Class (MonadIO)
import           Engine.State           (State)
import           Prelude                hiding (init)

-- | GUI record.
data GUI = GUI
    deriving Show

-- | Initialize the GUI given the path to the resource base directory.
init :: MonadIO m => FilePath -> m (Either String GUI)
init = undefined

-- | Delete the GUI's resources.
delete :: MonadIO m => GUI -> m ()
delete = undefined

-- | Animate the GUI.
animate :: MonadIO m => GUI -> m GUI
animate = undefined

render :: GUI -> Render State ()
render = undefined

setCenterFlash :: String -> GUI -> GUI
setCenterFlash = undefined
