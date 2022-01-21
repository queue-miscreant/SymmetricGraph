--graph theory operations, primarily algebraic ones
module Cayley.Graph where

import Data.Maybe (catMaybes)
import Data.List ((\\), partition, nub, nubBy, intersect)
import Data.Array
import Data.Graph

import Data.Tuple (swap)
import Data.Function (on)

import Data.STRef
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

import Cayley.Matrix

--representation of a graph as a list of neighbors for node n
--data Graph = G {unG :: Array Int [Int]} deriving Show

data EdgeTo = LeftEdge | RightEdge | UndirEdge

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (==x) xs

--why this is left out of Data.List is a mystery
symdiff :: Eq a => [a] -> [a] -> [a]
symdiff a b = (a \\ b) ++ (b \\ a)

--square bounds
squareB :: Ix i => (i, i) -> ((i, i), (i, i))
squareB (s, e) = ((s,s),(e,e))

--GRAPH OPERATIONS--------------------------------------------------------------

emptyG :: Int -> Graph
emptyG n = listArray (0,n-1) $ repeat []

--count graph objects
numNodes :: Graph -> Int 
numNodes = (+1) . snd . bounds

numDirEdges :: Graph -> Int 
numDirEdges = sum . fmap length

numEdges' :: Graph -> Int 
numEdges' = (`div` 2) . numDirEdges

numEdges :: Graph -> Int 
numEdges = numEdges' . undirect

--edge comparisons

--whether two edges which differ by a swap
edgeUndirEq :: Edge -> Edge -> Bool
edgeUndirEq   (x,y) (z,w) = (x == z && y == w) || (x == w && y == z)

--whether two edges share a common vertex
edgeConnected :: Edge -> Edge -> Bool
edgeConnected (i,j) (k,l) = (i == k || i == l) || (j == k || j == l)

--whether an edge exists from (directed from m to n)
edgeBetween :: Graph -> Vertex -> Vertex -> Bool
edgeBetween gr m n = n `elem` gr!m

--whether a directed edge exists between m and n
dirEdgeBetween :: Graph -> Vertex -> Vertex -> Maybe EdgeTo
dirEdgeBetween gr m n
  | right && left = Just UndirEdge
  | left          = Just LeftEdge
  | right         = Just RightEdge
  | otherwise     = Nothing
   where right = n `elem` gr!m
         left  = m `elem` gr!n

--add new node
addNode :: Graph -> Graph
addNode gr = array (0,ab+1) $ (ab+1,[]):assocs gr where
  ab = snd $ bounds gr

--connect/remove edge between nodes m and n
addDirEdge :: Graph -> Vertex -> Vertex -> Graph
addDirEdge gr m n = gr // [(m, n:(gr!m))]

addEdge :: Graph -> Vertex -> Vertex -> Graph
addEdge gr m n = gr // [(m, n:(gr!m)), (n, m:(gr!n))]

--add new node and a directed edge pointing toward it
addNodeAndDirEdge :: Graph -> Vertex -> Graph
addNodeAndDirEdge gr m = addDirEdge (addNode gr) m (ab+1) where
  ab = snd $ bounds gr

--add new node and undirected edge
addNodeAndEdge :: Graph -> Vertex -> Graph
addNodeAndEdge gr = addEdge (addNode gr) (ab+1) where
  ab = snd $ bounds gr

--remove (directed) edge, if it exists, between two nodes
removeDirEdge :: Graph -> Vertex -> Vertex -> Graph
removeDirEdge gr m n = gr // [(m, (gr!m) \\ [n])]

removeEdge :: Graph -> Vertex -> Vertex -> Graph
removeEdge gr m n = gr // [(m, (gr!m) \\ [n]), (n, (gr!n) \\ [m])]

--remove totally unconnected nodes
prune gr = array (0, length remaining-1) $ remapped where
  (removed, remaining) = partition (null . snd) $ assocs gr
  remap i              = i - (sum $ map (fromEnum . (< i) . fst) $ removed)
  remapped             = [(remap x,map remap y) | (x,y) <- remaining]

--build neighbor list of words on lowercase letters,
--treating entries as indexed starting with 'a'
fromWordList :: [String] -> Graph
fromWordList xs = array (0, xn-1) $ stuff where
  xn    = length xs
  stuff = zip [0..] $ flip map xs $ map (\x -> fromEnum x - fromEnum 'a')

--ALTERNATE GRAPH REPRESENTATIONS-----------------------------------------------

--list of directed edges, as 2-tuples of initial and terminal vertices
toDirEdgeList :: Graph -> [Edge]
toDirEdgeList gr = concat $ snd $ foldl neighToEdges (0, []) gr where 
  neighToEdges (n, l) x = (n+1, (:l) $ map ((,) n) x)

--list of undirected edges, as unordered pairs of vertices
toEdgeList :: Graph -> [Edge]
toEdgeList gr = concat $ runST $ do 
  graph <- thaw gr :: ST s (STArray s Int [Int])
  --remove nodes from the graph as we read across its array
  forM (indices gr) (across graph)
    where across graph x = do 
            neighbors <- readArray graph x
            forM_ neighbors (remove graph x)
            return $ map ((,) x) neighbors
          remove :: (STArray s Int [Int]) -> Int -> Int -> ST s ()
          remove graph x y = do 
            coneighbors <- readArray graph y
            writeArray graph y (coneighbors \\ [x])

toUndirEdgeList :: Graph -> [Edge]
toUndirEdgeList = nubBy edgeUndirEq . toDirEdgeList

--convert (directed) edge list to graph
fromDirEdgeList :: [Edge] -> Graph
fromDirEdgeList edges = foldl (\a x -> uncurry (addDirEdge a) $ x) empty' edges where 
   empty' = emptyG $ 1 + (maximum $ map (uncurry max) edges)

fromEdgeList :: [Edge] -> Graph
fromEdgeList edges = foldl (\a x -> uncurry (addEdge a) $ x) empty' edges where 
   empty' = emptyG $ 1 + (maximum $ map (uncurry max) edges)

--for really big graphs, using mutable arrays is mandatory
adjacencyFromEdgeList :: Int -> [Edge] -> Matrix Int
adjacencyFromEdgeList n xs 
  = runSTArray $ do arr <- newArray ((0,0), (n-1, n-1)) 0
                    forM_ xs (\x -> do cur <- readArray arr x
                                       writeArray arr x (cur+1))
                    return arr

--get adjacency matrix for an undirected graph (as neighbor array)
toAdjacency :: Graph -> Matrix Int
toAdjacency g = adjacencyFromEdgeList (numNodes g) (toDirEdgeList g)

toAdjacencyUndir :: Graph -> Matrix Int
toAdjacencyUndir g
  = adjacencyFromEdgeList (numNodes g) $ concat [[edge, swap edge] | edge <- toEdgeList g]

--convert adjacency matrix to neighbor array
fromAdjacency :: Matrix Int -> Graph
fromAdjacency arr = listArray (0,ab) neigh where
  ab    = snd $ snd $ bounds arr
  rows  = [[(m,n) | n <- [0..ab]] | m <- [0..ab]]
  neigh = map (>>= (\c@(a,b) -> replicate (arr!c) b)) rows

heatmapG :: Graph -> IO () 
heatmapG = heatmap . toAdjacency

--GRAPH OPERATIONS-------------------------------------------------------------

--lift an adjacency matrix operation into one on graphs
liftG :: (Matrix Int -> Matrix Int -> Matrix Int) 
  -> Graph -> Graph -> Graph
liftG = ((fromAdjacency .) .) . (`on` toAdjacency)

--tensor product on graphs
tensorG :: Graph -> Graph -> Graph
tensorG = liftG tensor
--box product on graphs
boxG :: Graph -> Graph -> Graph
boxG    = liftG box
--strong product (box plus tensor)
strongG :: Graph -> Graph -> Graph
strongG = liftG strong
--direct sum
dPlusG :: Graph -> Graph -> Graph
dPlusG  = liftG dPlus
--sum
plusG :: Graph -> Graph -> Graph
plusG   = liftG (+)

--convert all directed edges to undirected edges
undirect :: Graph -> Graph
undirect = fromEdgeList . toEdgeList

--generate the complement graph
complement :: Graph -> Graph
complement gr = listArray (bounds gr) $ map diff vertices where
  vertices = indices gr
  diff vertex = gr!vertex `symdiff` (vertices \\ [vertex])

--generate the medial or "line" graph from another graph
--this is sometimes equivalent to "ambo" in Conway polyhedron notation (planar graphs)
medial :: Graph -> Graph
medial gr = array (0,len-1) $ map neigh edges' where
  edges' = zip [0..] $ toEdgeList gr
  len = length edges'
  neigh (n,end) = (n, foldl (populate n end) [] edges')
  populate n end acc (m,end2)
    | m /= n && edgeConnected end end2 = m:acc
    | otherwise                        = acc


--from a particular node (indexed by number), partition nodes into disjoint
--classes by graph distance
neighbors :: Graph -> Vertex -> [[Vertex]]
neighbors gr n = ([n]:) $ neighbors' [n] visited where
  visited = (listArray (bounds gr) $ repeat True) // [(n, False)]
  neighbors' ns mask 
        | null layer = [] --mask fixed
        | otherwise  = layer:neighbors' layer next where
            (layer, next) = filterFold [] mask $ map (gr!) ns
  --accumulate list entries which are "True" in the mask
  --and generate a new mask with those entries set to "False"
  filterFold ret mask []     = (concat ret, mask)
  filterFold ret mask (x:xs) = filterFold (unvisited:ret) newMask xs where
    unvisited = filter (mask!) x
    newMask   = mask // (zip unvisited $ repeat False)

--ST variant of above code 
--unoptimized, this performs worse (even in space) than `neighbors` above, and 
--even when optimized, performs only marginally better
{-
neighborsST (G graph) n = reverse $ runST $ do 
  mask <- newArray (bounds graph) True :: ST s (STUArray s Int Bool)
  writeArray mask n False

  ret <- newSTRef [[n]]
  let neighbors' = filterM (\y -> do 
          y' <- readArray mask y 
          if y'
          then writeArray mask y False >> return True
          else return False
        ) . concatMap (graph!)
  let go ns = do 
      lay <- neighbors' ns
      if null lay
      then return ()
      else (modifySTRef ret (lay:)) >> go lay

  go [n]
  readSTRef ret
-}

--number of pairs of vertices on an undirected graph `gr` which satisfy a predicate `p`
sharedEach :: (Graph -> Vertex -> Vertex -> Bool) -> Graph -> Either [Int] Int
sharedEach p gr
  | allEqual ret = Right $ if null ret then 0 else head ret
  | otherwise    = Left $ nub $ ret where
    ret = do 
      i@(m,n) <- range $ squareB (bounds gr) --all pairs of nodes (oh no, n^2)
      guard $ m < n    --only one copy, (lower, higher)
      guard $ p gr m n --do they satisfy the predicate?
      let intersection = (gr!m) `intersect` (gr!n)
      return $ length intersection --number of common neighbors

--adjacent vertices share these many neighbors
sharedAdjacent :: Graph -> Either [Int] Int
sharedAdjacent = sharedEach (\a m n -> n `elem` a!m)

--nonadjacent vertices share these many neighbors
sharedNonAdjacent :: Graph -> Either [Int] Int
sharedNonAdjacent = sharedEach (\a m n -> not $ n `elem` a!m)

--tests if g is a strongly regular graph
--Right (lambda, mu) values are strongly regular,
--Left (list of lambdas, list of mus) values are weakly regular
regularity :: Graph -> Either ([Int], [Int]) (Int, Int)
regularity g = case adj of  {
  Left adj' -> case nonadj of {
    Left nonadj'  -> Left (adj', nonadj');
    Right nonadj' -> Left (adj', [nonadj']); };
  Right adj' -> case nonadj of {
    Left nonadj'  -> Left  ([adj'], nonadj');
    Right nonadj' -> Right (adj', nonadj') } } where
      adj    = sharedAdjacent g
      nonadj = sharedNonAdjacent g
