module Graph where

import Algebra

import Data.Array
import Data.Maybe (isJust)
import Data.List ((\\))
import Control.Monad (foldM)

--representation of a graph as a list of neighbors for node n
data Graph = G (Array Int [Int]) deriving Show

--complete graphs (simplex skeletons)
k n = G $ array (0, n-1) $ [(i, [0..i-1] ++ [i+1..n-1]) | i <- [0..n-1]]

--complete n-partite graphs
npartite [x] = k x
npartite xs  = G $ array (0, sum xs - 1) connect where
  lb = length xs - 1
  ls = [0..lb]
  bounds = listArray (0, lb) $ (zip <*> tail) $ 0:scanl1 (+) xs
  toList l = let (a,b) = bounds!l in [a..b-1]
  parts = [(,) (toList l) $ (ls \\ return l) >>= toList | l <- ls]
  connect = parts >>= (\(a,b) -> map (flip (,) b) a)

--complete bipartite graphs
bipartite m n = npartite [m,n]

--cycle graphs
cycGraph 2 = k 2
cycGraph n = G $ array (0, n-1) $ [(i, [(i-1) `mod` n, (i+1) `mod` n]) | i <- [0..n-1]]

--build neighbor list of words on lowercase letters,
--treating entries as indexed starting with 'a'
wordList xs = G $ array (0, xn-1) $ stuff where
  xn    = length xs
  stuff = zip [0..] $ flip map xs $ map (\x -> fromEnum x - fromEnum 'a')

--get adjacency matrix for a graph (as neighbor array)
toAdjacency (G arr) = zero // zip indices (repeat 1) where
  zero = listArray ((0,0),(ab,ab)) $ repeat 0
  ab = snd $ bounds arr
  indices = [0..ab] >>= (\i -> map ((,) i) $ arr!i)

--print grayscale (ansi-escape) heatmap of 2-dim array
heatMap arr = mapM_ printRow rows where
  ab = snd $ snd $ bounds arr
  rows = map (\i -> map ((,) i) [0..ab]) [0..ab]
  max = maximum arr
  min = minimum arr
  lerp i = ((24*(i - min)) `div` (max - min)) - 1
  grayscale (-1) = putStr $ "\x1b[40m  "
  grayscale i    = putStr $ "\x1b[48;5;" ++ show (232 + i) ++ "m  "
  reset = "\x1b[m\n"
  printRow tuples = mapM_ (grayscale . lerp . (arr!)) tuples >> putStr reset

--GRAPH OPERATIONS-------------------------------------------------------------

symdiff a b = (a \\ b) ++ (b \\ a)

--list of edges, as 2-tuples of initial and terminal vertices
edges (G arr) = fst $ foldl removeNode ([], arr) ab where
  ab = [0..(snd $ bounds arr)]
  removeNode (l, a) i = foldl (removeEdge i) (l, a) (a!i)
  removeEdge i (l, a) j = ((i,j):l, a // [(j, (a!j \\ [i]))])

--generate the medial or "line" graph from another graph
--this is sometimes equivalent to "ambo" in Conway polyhedron notation (planar graphs)
medial graph = G $ array (0,len-1) $ map neigh edges' where
  edges' = zip [0..] $ edges graph
  len = length edges'
  neigh (n,end) = (n, foldl (populate n end) [] edges')
  populate n end acc (m,end2)
    | m /= n && connected end end2 = m:acc
    | otherwise          = acc
  --TODO: use intermediate array so each edge is unique, even if the pair isn't
  connected (i,j) (k,l) = (i == k || i == l) || (j == k || j == l)

--generate the complement graph from another graph
complement (G arr) = G $ listArray (0, ab) $ map diff [0..ab] where
  ab = snd $ bounds arr
  diff vertex = arr!vertex `symdiff` ([0..ab] \\ [vertex])

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
        | and arr   = [] --all nodes visited
        | otherwise = layer:neighbors' layer next where
            (layer, next) = filterFold [] arr $ map (graph!) ns

--GRPAH "FLOW" OPERATIONS------------------------------------------------------

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