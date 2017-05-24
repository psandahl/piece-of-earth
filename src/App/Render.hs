-- |
-- Module: App.Render
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module App.Render
    ( render
    ) where

import           App.State    (State)
import           BigE.Runtime (Render)
import qualified Graphics.GL  as GL

-- | The master rendering callback.
render :: Render State ()
render = do
    GL.glClearColor 0 0 0.4 0
    GL.glClear GL.GL_COLOR_BUFFER_BIT
