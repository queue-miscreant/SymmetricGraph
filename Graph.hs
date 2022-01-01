module Graph where
--TODO: replace all `snd bounds` nonsense with Data.Array.range, where possible

import Data.Array
import Data.Maybe (isJust)
import Data.Tuple (swap)
import Data.List ((\\), nubBy)
import Control.Monad (foldM)
import Data.Function (on)

import Algebra
import Matrix

--why this is left out of Data.List is a mystery
symdiff a b = (a \\ b) ++ (b \\ a)

--representation of a graph as a list of neighbors for node n
data Graph = G {unG :: Array Int [Int]} deriving Show

numNodes = length . unG
numEdges = (`div` 2) . sum . fmap length . unG

--connect nodes m and n
addEdge (G a) m n = G $ a // [(m, n:(a!m)), (n, m:(a!n))]

--remove edge between nodes m and n, if such an edge exists
removeEdge (G a) m n = G $ a // [(m, (a!m) \\ [n]), (n, (a!n) \\ [m])]

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

--get adjacency matrix for an undirected graph (as neighbor array)
toAdjacency g = foldl ( \a x -> (a // [(x, a!x + 1), (swap x, a!x + 1)]) ) zero' edges where
  zero' = zero $ numNodes g
  edges = toEdgeList g

--convert adjacency matrix to neighbor array
fromAdjacency arr = G $ listArray (0,ab) neigh where
  ab = snd $ snd $ bounds arr
  --TODO use Data.Array iteration method
  rows = map (\i -> map ((,) i) [0..ab]) [0..ab]
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
complement (G arr) = G $ listArray (0, ab) $ map diff [0..ab] where
  ab = snd $ bounds arr
  diff vertex = arr!vertex `symdiff` ([0..ab] \\ [vertex])

--a somewhat natural graph arithmetic where addition is disjoint union,
--multiplication is tensor product, and negation is graph complement
instance Num Graph where
  (+) = dPlusG
  (*) = tensorG
  negate = complement

  abs = undefined
  signum = undefined
  fromInteger = undefined

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

--GRAPH "FLOW" OPERATIONS------------------------------------------------------

--sum classes of neighbors together
sumClasses = sumClasses' [] [] where
  sumClasses' []  temp []     = []
  sumClasses' zip temp []     = (concat temp):sumClasses' [] [] zip
  sumClasses' zip temp (x:xs)
    | null x                  = sumClasses' zip temp xs
    | otherwise               = sumClasses' (tail x:zip) (head x:temp) xs

-- from node 0, find all possible n-distance 2 step walks
-- ((alg !! x) !! y) represents all nodes achieved by taking one step of length x and another of length y
flowClasses (G arr) = (alg, classes) where
  --neighbors of each node
  nodeN = let n = snd $ bounds arr in listArray (0,n) $ map (neighbors (G arr)) [0..n]
  classes = nodeN!0
  --algebra elements corresponding to neighbors of neighbors
  alg = map (sumClasses . map (nodeN!)) classes

-- extract only the flow types
flow = fst . flowClasses

--decompose a list into `classes` represented by `AlgebraElement`s
match classes = match' (z 0) where
  match' ret []         = ret
  match' ret xxs@(x:xs) = match' (ret .+ c cnum) (xxs \\ class_) where 
    --find x in the class list, subtract out other members
    (cnum, class_) = either id undefined $ foldM findClass 0 classes
    --continue if n contains x, otherwise increment and proceed
    findClass m n  = if x `elem` n then Left (m, n) else Right (m+1)

--create cayley table based on neighbor classes
--this is only valid for certain on general platonic graphs -- i.e., those which 
--represent an n-dimensional regular figure
flowAlg graph = array ((0,0),(dim-1, dim-1)) $ [((x,z), w) | (x,y) <- flow', (z,w) <- y] where
  flow' = zip [0..] $ map (zip [0..] . map (match classes)) alg
  dim = length flow'
  (alg, classes) = flowClasses graph

--an induced algebra should be commutative, so list differences should not "miss"
--elements. If this is the case for all elements, return the AlgebraElement sum
maybeMatch classes = match' (z 0) where
  match' ret []         = Just ret
  match' ret xxs@(x:xs)
    | length diff == length xxs - length class_ = match' (ret .+ c cnum) diff
    | otherwise                                 = Nothing where
    diff = xxs \\ class_
    --find x in the class list, subtract out other members
    (cnum, class_) = either id undefined $ foldM findClass 0 classes
    --continue if n contains x, otherwise increment and proceed
    findClass m n  = if x `elem` n then Left (m, n) else Right (m+1)

--flowAlg, but courteous of the output of maybeMatch
maybeFlowAlg graph
  | allJust   = Just $ array ((0,0),(dim-1, dim-1)) $ [((x,z), w) | (x,y) <- flow', (z,w) <- y]
  | otherwise = Nothing where
  allJust = all (isJust) $ alg >>= map (maybeMatch classes)
  flow' = zip [0..] $ map (zip [0..] . map (match classes)) alg
  dim = length flow'
  (alg, classes) = flowClasses graph


--GRAPH FAMILIES---------------------------------------------------------------

--complete graphs (simplex skeleta)
k n = G $ array (0, n-1) $ [(i, [0..i-1] ++ [i+1..n-1]) | i <- [0..n-1]]

--complete n-partite graphs
--when written pointfree and pattern matched, this complains about arity
npartite xs | null $ tail xs = k $ head xs
            | otherwise      = negate . foldl1 (+) $ map k xs

--complete bipartite graphs
bipartite m n = npartite [m,n]

--crown graphs - bipartite graphs which lack a connection to the 'antipodal' node
crown m = G $ array (0, (2*m-1)) $ [0..m-1] >>= neighbors where
  neighbors x = let (a,b) = unzip [(i+m,i) | i <- [0..m-1], i /= x]
                in  [(x,a),(x+m,b)]

--cycle graphs
cycGraph 2 = k 2
cycGraph n = G $ array (0, n-1) $ [(i, [(i-1) `mod` n, (i+1) `mod` n]) | i <- [0..n-1]]

--path graphs
path 1 = k 1
path 2 = k 2
path n = G $ let (G a) = cycGraph n in a // [(0, [1]), (n-1, [n-2])]

--star graphs
star n = G $ array (0, n-1) $ (0, [1..n-1]):[(i, [0]) | i <- [1..n-1]]

--wheel graphs
wheel n = star n `plusG` (k 1 `dPlusG` cycGraph (n-1))
