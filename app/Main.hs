module Main where

import Control.Monad
import qualified Data.Map as M
import qualified GraphiumLib as G

main :: IO ()
main = do
  putStrLn "Do you want to load a graph from a file? (y/n)"
  answer <- getLine
  if answer == "y" 
    then do
      putStrLn "Type the name of the file"
      filename <- getLine
      G.startFromFile $ "saved-graphs/" ++ filename
    else do
      putStrLn "Do you want to save the graph for future sessions? (y/n)"
      answer2 <- getLine
      if answer2 == "y"
      then do
        putStrLn "How many vertices do you want your graph to have?"
        nStr <- getLine
        let n = read nStr :: Int
            vertices = [1..n]
        putStrLn "Your graph will have the following vertices:"
        print vertices
        graphList <- forM vertices (\v -> do
          putStrLn $ "\nWhat vertices should " ++ show v ++ " be adjacent to?"
          putStrLn "Put the vertices between spaces, like so: 1 3 5"
          adjList <- getLine >>= (return . map head . words)
          return (head $ show v, adjList)
          )
        let graph = M.fromList graphList
        putStrLn "\nThis is your graph:"
        putStr $ G.adjTable graph
        putStrLn "Type the name of the graph"
        graphName <- getLine
        writeFile ("saved-graphs/" ++ graphName) $ G.adjTable graph
        G.start graph
      else G.startEmpty
