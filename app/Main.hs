module Main where

import qualified Model
import qualified Server

main :: IO ()
main = do
  --Model.createDemoData
  Server.run
