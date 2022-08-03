module Main where

import Control.Monad
import qualified Data.Map as M
import qualified GraphiumLib as G

main :: IO ()
--main = GraphiumLib.start
main = do
  putStrLn "Do you want to load a graph from a file? (y/n)"
  answer <- getLine
  if answer == "y" 
    then do
      putStrLn "Type the name of the file"
      filename <- getLine
      G.startFromFile $ "saved-graphs/" ++ filename
    else G.startEmpty 
    {--
      putStrLn "Will you want to save the graph for future sessions? (y/n)"
      answer2 <- getLine
      if answer2 == "y"
        then do
          putStrLn "What do you want the file to be named?"
          newFilename <- getLine
          writeFile ("saved-graphs/" ++ newFilename) $ G.adjTable graph
          G.startEmpty
        else do
--}
