--graph theory operations, primarily algebraic ones
module Graph where

import Data.Array
import Data.Array.ST

import Data.List ((\\), partition, nubBy)
import Data.Tuple (swap)
import Data.Function (on)

import Control.Monad
import Control.Monad.ST

import Matrix

--why this is left out of Data.List is a mystery
symdiff a b = (a \\ b) ++ (b \\ a)

edgeUndirEq   (x,y) (z,w) = (x == z && y == w) || (x == w && y == z)
edgeConnected (i,j) (k,l) = (i == k || i == l) || (j == k || j == l)

--representation of a (undirected) graph as a list of neighbors for node n
--though this could be used for directed graphs equally as well, many functions assume undirectedness
data Graph = G {unG :: Array Int [Int]} deriving Show

emptyG n = G $ listArray (0,n-1) $ repeat []

numNodes = length . unG
numDirEdges = sum . fmap length . unG
numEdges = (`div` 2) . numDirEdges . undirect

--connect/remove edge between nodes m and n
addDirEdge (G a) m n = G $ a // [(m, n:(a!m))]
addEdge (G a) m n = G $ a // [(m, n:(a!m)), (n, m:(a!n))]

removeDirEdge (G a) m n = G $ a // [(m, (a!m) \\ [n])]
removeEdge (G a) m n = G $ a // [(m, (a!m) \\ [n]), (n, (a!n) \\ [m])]

--remove totally unconnected nodes
prune (G a) = G $ array (0, length remaining-1) $ remapped where
  (removed, remaining) = partition (null . snd) $ assocs a
  remap i              = i - (sum $ map (fromEnum . (< i) . fst) $ removed)
  remapped             = [(remap x,map remap y) | (x,y) <- remaining]

--build neighbor list of words on lowercase letters,
--treating entries as indexed starting with 'a'
fromWordList xs = G $ array (0, xn-1) $ stuff where
  xn    = length xs
  stuff = zip [0..] $ flip map xs $ map (\x -> fromEnum x - fromEnum 'a')

--list of directed edges, as 2-tuples of initial and terminal vertices
toDirEdgeList (G arr) = concat $ snd $ foldl neighToEdges (0, []) arr where 
  neighToEdges (n, l) x = (n+1, (:l) $ map ((,) n) x)

--list of undirected edges, as unordered pairs of vertices
toEdgeList (G arr) = concat $ runST $ do 
  graph <- thaw arr :: ST s (STArray s Int [Int])
  --remove nodes from the graph as we read across its array
  forM (indices arr) (across graph)
    where across graph x = do 
            neighbors <- readArray graph x
            forM_ neighbors (remove graph x)
            return $ map ((,) x) neighbors
          remove :: (STArray s Int [Int]) -> Int -> Int -> ST s ()
          remove graph x y = do 
            coneighbors <- readArray graph y
            writeArray graph y (coneighbors \\ [x])

toEdgeList' = nubBy edgeUndirEq . toDirEdgeList

--convert (undirected) edge list to graph
fromEdgeList edges = foldl (\a x -> uncurry (addEdge a) $ x) empty' edges where 
   empty' = emptyG $ 1 + (maximum $ map (uncurry max) edges)

--convert directed edge list to graph
fromDirEdgeList edges = foldl (\a x -> uncurry (addDirEdge a) $ x) empty' edges where 
   empty' = emptyG $ 1 + (maximum $ map (uncurry max) edges)

undirect = fromEdgeList . toEdgeList

--for really big graphs, using mutable arrays is mandatory
adjacencyFromEdgeList n xs 
  = runSTArray $ do arr <- thaw $ zero n
                    forM_ xs (\x -> do cur <- readArray arr x
                                       writeArray arr x (cur+1))
                    return arr

--get adjacency matrix for an undirected graph (as neighbor array)
toAdjacency g = adjacencyFromEdgeList (numNodes g) (toDirEdgeList g)

toAdjacencyUndirected g
  = adjacencyFromEdgeList (numNodes g) $ concat $ [[edge, swap edge] | edge <- toEdgeList g]

--convert adjacency matrix to neighbor array
fromAdjacency arr = G $ listArray (0,ab) neigh where
  ab    = snd $ snd $ bounds arr
  rows  = [[(m,n) | n <- [0..ab]] | m <- [0..ab]]
  neigh = map (>>= (\c@(a,b) -> replicate (arr!c) b)) rows

heatmapG = heatmap . toAdjacency

--GRAPH OPERATIONS-------------------------------------------------------------

--lift an adjacency matrix operation into one on graphs
liftG = ((fromAdjacency .) .) . (`on` toAdjacency)

--tensor product on graphs
tensorG = liftG tensor
--box product on graphs
boxG    = liftG box
--strong product (box plus tensor)
strongG = liftG strong
--direct sum
dPlusG  = liftG dPlus
--sum
plusG   = liftG (+)

--generate the complement graph
complement (G arr) = G $ listArray (bounds arr) $ map diff vertices where
  vertices = indices arr
  diff vertex = arr!vertex `symdiff` (vertices \\ [vertex])

--generate the medial or "line" graph from another graph
--this is sometimes equivalent to "ambo" in Conway polyhedron notation (planar graphs)
medial graph = G $ array (0,len-1) $ map neigh edges' where
  edges' = zip [0..] $ toEdgeList graph
  len = length edges'
  neigh (n,end) = (n, foldl (populate n end) [] edges')
  populate n end acc (m,end2)
    | m /= n && edgeConnected end end2 = m:acc
    | otherwise                        = acc
  --TODO: use intermediate array so each edge is unique, even if the pair isn't

--accumulate list entries which are "False" in the mask
--and generate a new mask with those entries set to "True"
filterFold ret mask []     = (concat ret, mask)
filterFold ret mask (x:xs) = filterFold (unvisited:ret) newMask xs where
  unvisited = filter (not . (mask!)) x
  newMask   = mask // (zip unvisited $ repeat True)

--from a particular node (indexed by number), partition nodes into disjoint
--classes by graph distance
neighbors (G graph) n = ([n]:) $ neighbors' [n] mask where
  mask = (listArray (0, gb) $ repeat False) // [(n, True)]
  gb    = snd $ bounds graph
  neighbors' ns mask 
        | mask == next = [] --mask fixed
        | otherwise    = layer:neighbors' layer next where
            (layer, next) = filterFold [] mask $ map (graph!) ns

--GRAPH FAMILIES---------------------------------------------------------------

--complete graphs (simplex skeleta)
k n = G $ array (0, n-1) $ [(i, [0..i-1] ++ [i+1..n-1]) | i <- [0..n-1]]

--complete n-partite graphs
--when written pointfree and pattern matched, this complains about arity
npartite xs | null $ tail xs = k $ head xs
            | otherwise      = complement . foldl1 (dPlusG) $ map k xs

--complete bipartite graphs
bipartite m n = npartite [m,n]

--crown graphs - bipartite graphs which lack a connection to the 'antipodal' node
crown m = G $ array (0, (2*m-1)) $ [0..m-1] >>= neighbors where
  neighbors x = let (a,b) = unzip [(i+m,i) | i <- [0..m-1], i /= x]
                in  [(x,a),(x+m,b)]

--cycle graphs
cycleG 2 = k 2
cycleG n = G $ array (0, n-1) $ [(i, [(i-1) `mod` n, (i+1) `mod` n]) | i <- [0..n-1]]

--path graphs
path 1 = k 1
path 2 = k 2
path n = G $ let (G a) = cycleG n in a // [(0, [1]), (n-1, [n-2])]

--star graphs
star n = G $ array (0, n-1) $ (0, [1..n-1]):[(i, [0]) | i <- [1..n-1]]

--wheel graphs
wheel n = star n `plusG` (k 1 `dPlusG` cycleG (n-1))
