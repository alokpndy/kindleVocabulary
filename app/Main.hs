module Main where

import ClipApi
import One
import FlashCards




import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)





main :: IO ()
main = do 
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  initDB 
  main3 (read port)
  run2 (read port)
