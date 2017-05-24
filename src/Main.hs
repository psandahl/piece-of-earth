module Main
    ( main
    ) where

import           BigE.Runtime    (Configuration (..), DisplayMode (..), runBigE)
import qualified Engine.Animate  as Animate
import           Engine.Options  (Options (..), parseOptions)
import qualified Engine.Render   as Render
import qualified Engine.Setup    as Setup
import qualified Engine.Teardown as Teardown
import           Text.Printf     (printf)

main :: IO ()
main = do
    -- Read the command line options. If the user is requesting help the
    -- program will terminate automatically.
    options <- parseOptions
    result <- runBigE Configuration
        { versionMajor = 3
        , versionMinor = 3
        , displayMode = selectDisplayMode options
        , windowCaption = caption
        , setup = Setup.setup options
        , animate = Animate.animate
        , render = Render.render
        , teardown = Teardown.teardown
        }

    case result of
        Right () -> return ()
        Left err -> printf "Error: %s\n" err


selectDisplayMode :: Options -> DisplayMode
selectDisplayMode options
    | fullscreen options = FullScreen
    | otherwise = SizedScreen (1024, 768)

caption :: String
caption = "-= Piece Of Earth =-"
