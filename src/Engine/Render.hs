-- |
-- Module: Engine.Render
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Render
    ( render
    ) where

import           BigE.Runtime (Render)
import           Engine.State (State)
import qualified Graphics.GL  as GL

-- | The master rendering callback.
render :: Render State ()
render = do
    GL.glClearColor 0 0 0.4 0
    GL.glClear GL.GL_COLOR_BUFFER_BIT
