-- |
-- Module: Engine.Options
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Engine.Options
    ( Options (..)
    , parseOptions
    ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      progDesc, short, strOption, switch, value,
                                      (<**>))

-- | Command line options.
data Options = Options
    { fullscreen  :: !Bool
    , resourceDir :: !FilePath
    } deriving Show

-- | Parse the command line options.
parseOptions :: IO Options
parseOptions = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
    info (parser <**> helper)
        ( fullDesc
        <> progDesc "3D Terrain Visualisation"
        <> header "piece-of-earth - terrain visualisation game"
        )

parser :: Parser Options
parser = Options
    <$> switch (
        long "fullscreen"
     <> short 'f'
     <> help "Run in fullscreen mode (default = False)"
    )
    <*> strOption (
        long "resource-dir"
     <> short 'r'
     <> metavar "PATH"
     <> value defaultResourceDir
     <> help ("Resource directory root (default = " ++ defaultResourceDir ++ ")")
    )

defaultResourceDir :: FilePath
defaultResourceDir = "resources"
