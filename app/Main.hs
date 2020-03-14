module Main where

--import qualified Model
import qualified Server

main :: IO ()
main =
  -- Model.createDemoData
  Server.run
