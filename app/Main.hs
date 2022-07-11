module Main where

import Control.Monad
import qualified Data.Map as M
import qualified GraphiumLib as G

main :: IO ()
--main = GraphiumLib.start
main = do
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
  G.start graph
