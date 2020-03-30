module Main where

import qualified Model
import qualified Server

main :: IO ()
main = do
  --TODO parse CLI args: 
  -- directory with images
  -- port number
  -- recreate demo data
                      
  --Model.createDemoData
  Server.run