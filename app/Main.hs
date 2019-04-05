module Main where

import ClipApi
import One 




import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)





main :: IO ()
main = do 
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  putStrLn $ "Serving from port at " ++ port 
  main3 (read port) 
