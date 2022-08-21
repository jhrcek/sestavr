module Main where

import qualified Config
import qualified Server

main :: IO ()
main =
    Config.parseArgs >>= Server.run
