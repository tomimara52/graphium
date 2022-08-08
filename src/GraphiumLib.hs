module GraphiumLib (start, startFromFile, startEmpty, adjTable) where

import qualified Data.Map as M 
import Data.Char (isDigit, isLetter)
import qualified Data.List as L
import qualified Data.Text as T
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

start :: Graph -> IO ()
start gr = play window white 30 (initialWorld gr) render handle update

startFromFile :: String -> IO ()
startFromFile filename = do
  contents <- readFile filename
  let gr = tableToGraph contents
  start gr

startEmpty :: IO ()
startEmpty = play window white 30 initialWorldEmpty render handle update

window :: Display 
window = InWindow "Graphium" (300, 300) (0, 0) 

render :: World -> Picture 
render world = pictures [graphEdges gr, graphVertices gr, displayMode world]
  where gr = graphState world

handle :: Event -> World -> World
handle (EventKey (MouseButton LeftButton) Down _ (x, y)) world
  | putVerticesMode world = createVertex x y world
  | putEdgesMode world = joinVertices x y world
  | otherwise = 
    if L.null clickedResult
    then world 
    else world { isClicked = True, draggedV = unjust clickedResult }
      where clickedResult = clickedV x y $ graphState world
            unjust (Just x) = x
handle (EventKey (MouseButton LeftButton) Up _ (x, y)) world = world { isClicked = False }
handle (EventMotion (x, y)) world =
  if isClicked world 
  then moveV x y world
  else world
handle (EventKey (SpecialKey KeySpace) Down _ _) world
  | putVerticesMode world = world { putVerticesMode = False, putEdgesMode = True }
  | putEdgesMode world = world { putEdgesMode = False }
  | otherwise = world
handle _ world = world

createVertex :: Float -> Float -> World -> World
createVertex x y world = world { graphState = newGr }
  where n = head $ show $ L.genericLength $ M.keys actualGr
        actualGr = graphState world
        newGr = M.insert n ("", (x, y)) actualGr

joinVertices :: Float -> Float -> World -> World
joinVertices x y world 
  | L.null clickedResult = world
  | fst $ toJoinV world =
      let a = snd $ toJoinV world
          b = unjust clickedResult
          addEdges (v, _) (adj, pos) = (adj ++ v, pos)
          newGr = M.insertWith addEdges a ([b], (0,0)) $ M.insertWith addEdges b ([a], (0,0)) (graphState world)
      in if a == b
         then world
         else world { graphState = newGr, toJoinV = (False, 'x') } 
  | otherwise = world { toJoinV = (True, unjust clickedResult) }
  where clickedResult = clickedV x y $ graphState world
        unjust (Just x) = x


update :: Float -> World -> World 
update _ world = world

moveV :: Float -> Float -> World -> World 
moveV x y world = world { graphState = newGraphState }
  where newGraphState = M.adjust insertNewPos (draggedV world) (graphState world)
        insertNewPos (adjs, (currX, currY)) = (adjs, (x, y))

clickedV :: Float -> Float -> GraphState -> Maybe Char 
clickedV x y gr = L.foldr foldFunc Nothing $ M.toList gr
  where 
  foldFunc (k, (_, (vx, vy))) acc = 
    if x >= (vx-20) && x <= (vx+20) && y >= (vy-20) && y <= (vy+20)
    then Just k 
    else acc

data World = World
  { graphState :: GraphState 
  , isClicked :: Bool 
  , draggedV :: Char 
  , putVerticesMode :: Bool
  , putEdgesMode :: Bool
  , toJoinV :: (Bool, Char) 
  }

initialWorld :: Graph -> World 
initialWorld gr = World { graphState = grState
                        , isClicked = False
                        , draggedV = 'x'
                        , putVerticesMode = False
                        , putEdgesMode = False
                        , toJoinV = (False, 'x')
                        }
  where grState = createGraphState gr

initialWorldEmpty :: World
initialWorldEmpty = World { graphState = M.fromList []
                          , isClicked = False
                          , draggedV = 'x'
                          , putVerticesMode = True
                          , putEdgesMode = False
                          , toJoinV = (False, 'x')
                          }

type GraphState = M.Map Char ([Char], (Float, Float))

lookupAdj :: Char -> GraphState -> [Char] 
lookupAdj k gr = fst $ M.findWithDefault ("x", (0,0)) k gr

lookupPos :: Char -> GraphState -> (Float, Float) 
lookupPos k gr = snd $ M.findWithDefault ("x", (0,0)) k gr

createGraphState :: Graph -> GraphState
createGraphState gr = M.fromList $ L.zipWith vertex [0..] $ M.keys gr 
  where angle = (2*pi) / L.genericLength (M.keys gr) 
        edges k = M.findWithDefault "x" k gr
        vertex n k = (k, (edges k, (100 * cos (angle*n), 100 * sin (angle*n))))

graphStateToGraph :: GraphState -> Graph
graphStateToGraph grSt = M.fromList $ L.map dropPos $ M.toList grSt
  where dropPos (k, (v, _)) = (k, v)

initialGraphState :: GraphState 
initialGraphState = createGraphState myGraph

graphVertices :: GraphState -> Picture 
graphVertices gr = pictures $ L.concatMap mkVertex $ M.toList gr
  where 
    mkVertex (k, (_, (x, y))) = 
      [ translate x y $ color (dark red) $ circleSolid 20
      , translate (x-8) (y-8) $ scale 0.2 0.2 $ color white $ text [k]]

graphEdges :: GraphState -> Picture 
graphEdges gr = pictures $ L.concatMap mkEdge $ M.toList gr
  where mkEdge (k, (v, _)) = L.map (mkLine k) v 
        mkLine a b = line [ lookupPos a gr, lookupPos b gr]

displayMode :: World -> Picture
displayMode world
  | putVerticesMode world = translate (-200) 200 $ scale 0.15 0.15 $ text "Left click to put vertices (space when finish)"
  | putEdgesMode world = translate (-200) 200 $ scale 0.15 0.15 $ text "Click two vertices to join them (space when finish)"
  | otherwise = blank 

type Graph = M.Map Char [Char] 

myGraph :: Graph 
myGraph = M.fromList [ ('0', "23"), ('1', "34"), ('2', "04"), ('3', "01"), ('4', "12") ]

adjTable :: Graph -> String 
adjTable = M.foldlWithKey (\acc k v -> acc ++ [k] ++ "| " ++ v ++ "\n") [] 

tableToGraph :: String -> Graph
tableToGraph table = M.fromList listToMap 
  where tableText = T.pack table
        cleanText t = T.unpack $ T.tail $ T.replace (T.pack " ") T.empty t
        listToMap = L.map (\(v, vv) -> (T.head v, cleanText vv)) $ L.map (T.break (== '|')) $ T.lines $ T.strip tableText
       

isNumOrLetter :: Char -> Bool
isNumOrLetter c = isDigit c || isLetter c
