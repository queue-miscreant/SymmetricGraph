--graph theory operations, primarily algebraic ones
module Graph where

import Data.Array
import Data.List ((\\), partition, nubBy)
import Data.Tuple (swap)
import Data.Function (on)

import Matrix

--why this is left out of Data.List is a mystery
symdiff a b = (a \\ b) ++ (b \\ a)

--representation of a (undirected) graph as a list of neighbors for node n
--though this could be used for directed graphs equally as well, many functions assume undirectedness
data Graph = G {unG :: Array Int [Int]} deriving Show

emptyG n = G $ listArray (0,n-1) $ repeat []

equalUndirected (x,y) (z,w) = (x == z && y == w) || (x == w && y == z)

numNodes = length . unG
numEdges = (`div` 2) . sum . fmap length . unG

--connect/remove edge between nodes m and n
addDirEdge (G a) m n = G $ a // [(m, n:(a!m))]
addEdge (G a) m n = G $ a // [(m, n:(a!m)), (n, m:(a!n))]

removeDirEdge (G a) m n = G $ a // [(m, (a!m) \\ [n])]
removeEdge (G a) m n = G $ a // [(m, (a!m) \\ [n]), (n, (a!n) \\ [m])]

--remove null nodes
prune (G a) = G $ array (0, length remaining-1) $ remapped where
  (removed, remaining) = partition (null . snd) $ assocs a
  remap i              = i - (sum $ map (fromEnum . (< i) . fst) $ removed)
  remapped             = [(remap x,map remap y) | (x,y) <- remaining]

--build neighbor list of words on lowercase letters,
--treating entries as indexed starting with 'a'
fromWordList xs = G $ array (0, xn-1) $ stuff where
  xn    = length xs
  stuff = zip [0..] $ flip map xs $ map (\x -> fromEnum x - fromEnum 'a')

--list of undirected edges, as 2-tuples of initial and terminal vertices
toEdgeList (G arr) = fst $ foldl removeNode ([], arr) $ indices arr where
  --for every edge starting from a node, find the terminal end and disconnect
  removeNode (l, a) i = foldl (removeEdgeFromNode i) (l, a) (a!i)
  removeEdgeFromNode i (l, a) j = ((i,j):l, a // [(j, (a!j \\ [i]))])

--same but in reverse (UNDIRECTED)
fromEdgeList edges = foldl (\a x -> uncurry (addEdge a) $ x) empty' edges where 
   empty' = emptyG $ 1 + (maximum $ map (uncurry max) edges)

undirect = fromEdgeList . nubBy equalUndirected . toEdgeList

--get adjacency matrix for an undirected graph (as neighbor array)
toAdjacency g = adjacencyFromEdgeList (numNodes g) (toEdgeList g)

adjacencyFromEdgeList n = foldl ( \a x -> (a // [(x, a!x + 1), (swap x, a!x + 1)]) ) (zero n)

--convert adjacency matrix to neighbor array
fromAdjacency arr = G $ listArray (0,ab) neigh where
  ab = snd $ snd $ bounds arr
  rows = [[(m,n) | n <- [0..ab]] | m <- [0..ab]]
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
    | m /= n && connected end end2 = m:acc
    | otherwise          = acc
  --TODO: use intermediate array so each edge is unique, even if the pair isn't
  connected (i,j) (k,l) = (i == k || i == l) || (j == k || j == l)

--accumulate list entries which are "False" in the mask
--and generate a new mask with those entries set to "True"
filterFold ret mask []     = (concat ret, mask)
filterFold ret mask (x:xs) = filterFold (unvisited:ret) newMask xs where
  unvisited = filter (not . (mask!)) x
  newMask   = mask // (zip unvisited $ repeat True)

--from a particular node (indexed by number), partition nodes into disjoint
--classes by graph distance
neighbors (G graph) n = ([n]:) $ neighbors' [n] $ false // [(n, True)] where
  false = listArray (0, gb) $ repeat False
  gb    = snd $ bounds graph
  neighbors' ns arr 
        | arr == next = [] --mask fixed
        | otherwise   = layer:neighbors' layer next where
            (layer, next) = filterFold [] arr $ map (graph!) ns

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
