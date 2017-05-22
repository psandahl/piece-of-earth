module Main
    ( main
    ) where

import           App.Options (parseOptions)

main :: IO ()
main = print =<< parseOptions
