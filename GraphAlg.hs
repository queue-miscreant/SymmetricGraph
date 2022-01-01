module GraphAlg where
--TODO: maybeFlowAlg needs to bind its elements instead of what it currently does

import Data.Array
import Data.List ((\\))
import Data.Maybe (isJust)
import Control.Monad (foldM)

import Graph
import Algebra

xor True = not
xor False = id

--undirected cayley graph with generating set `basis`
--the generating set is assumed to not contain the identity, if it contains an
--element, also contains its inverse (if it is not an involution)
cayleyGraph group basis = G $ fixMask false graph $ step basis' where
  gb    = snd $ snd $ bounds group
  false = (listArray (0, gb) $ repeat False) // (zip basis' $ repeat True)
  graph = listArray (0, gb) $ repeat []
  basis'       = map unX basis
  step current = [[((x, y), unX $ times group (X x) (X y)) | x <- current] | y <- basis']
  fixMask mask acc current
    | mask == newMask = acc
    | otherwise       = fixMask newMask newAcc (step next) where
      --append b to the neighbors of a and vice versa
      newAcc  = foldr addNeigh acc current
      addNeigh nodes arr = arr // (concat [[(a, b:arr!a), (b, a:arr!b)] | ((a,_), b) <- nodes, (mask!a) `xor` (mask!b)])
  
      (next, newMask) = filterFold [] mask $ map (map snd) current

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

