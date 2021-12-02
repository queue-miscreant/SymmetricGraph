module Graph where

import Algebra

import Data.Array
import Data.List ((\\))
import Control.Monad (foldM)

--representation of a graph as a list of neighbors for node n
data Graph = G (Array Int [Int]) deriving Show

--build neighbor list of words on lowercase letters, treating entries as indexed starting with 'a'
wordList xs = G $ array (0, xn-1) $ stuff where
  xn    = length xs
  stuff = zip [0..] $ flip map xs $ map (\x -> fromEnum x - fromEnum 'a')

--accumulate list entries which are "False" in the mask, and generate a new mask with those entries set to "True"
filterFold ret mask []     = (concat ret, mask)
filterFold ret mask (x:xs) = filterFold (unvisited:ret) newMask xs where
  unvisited = filter (not . (mask!)) x
  newMask   = mask // (zip unvisited $ repeat True)

--from a particular node (indexed by number), partition nodes into disjoint classes by graph distance
neighbors (G graph) n = ([n]:) $ neighbors' [n] $ false // [(n, True)] where
  false = listArray (0, gb) $ repeat False
  gb    = snd $ bounds graph
  neighbors' ns arr 
        | and arr   = [] --all nodes visited
        | otherwise = layer:neighbors' layer next where
            (layer, next) = filterFold [] arr $ map (graph!) ns

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

-- extract only the 
flow = fst . flowClasses

--match a contents of `flowClasses` with to sum of classes represented by `AlgebraElement`s
--this is only valid for certain on general platonic graphs -- i.e., those which 
--represent an n-dimensional regular figure
match classes = match' (z 0) where
  match' ret []         = ret
  match' ret xxs@(x:xs) = match' (ret .+ c cnum) (xxs \\ class_) where 
    --find x in the class list, subtract out other members
    (cnum, class_) = either id undefined $ foldM findClass 0 classes
    --continue if n contains x, otherwise increment and proceed
    findClass m n  = if x `elem` n then Left (m, n) else Right (m+1)

--create cayley table based on neighbor classes
flowAlg graph = array ((0,0),(dim-1, dim-1)) $ [((x,z), w) | (x,y) <- flow', (z,w) <- y] where
  flow' = zip [0..] $ map (zip [0..] . map (match classes)) alg
  dim = length flow'
  (alg, classes) = flowClasses graph
