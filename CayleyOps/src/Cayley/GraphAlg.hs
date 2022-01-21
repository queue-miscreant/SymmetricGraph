{-# LANGUAGE BangPatterns#-}
--operations in the interstice of graph theory and abstract algebra
module Cayley.GraphAlg where

import Control.Monad (forM)
import Control.Monad.ST

import Data.List ((\\), nub, findIndex)
import Data.Maybe (fromJust, catMaybes)
import Data.Ratio

import Data.Graph
import Data.Array
import Data.Array.ST

import Cayley.Graph
import Cayley.Algebra
import Cayley.Symmetric 

--CAYLEY GRAPHS----------------------------------------------------------------

-- cayley graph with group operation `f`, number of nodes `n`, and generating set `basis`
-- the generating set is assumed to not contain the group identity
cayleyGraph :: (GroupElement -> GroupElement -> GroupElement) -> Int -> [GroupElement] 
  -> Graph
cayleyGraph f n basis 
  = runSTArray $ do !graph <- newArray (0, n-1) []
                    !mask  <- newArray (0, n-1) False :: ST s (STArray s Int Bool)

                    let basis' = map unX basis
                    let step nodes = [(x, unX $ y `f` (X x)) | x <- nodes, y <- basis]
                    let updateMask = mapM_ (\x -> writeArray mask x True)
                    updateMask basis'

                    let fixMask cNodes
                          = do next <- forM cNodes (\(m,n) -> do 
                                  neigh <- readArray graph m
                                  writeArray graph m $ n:neigh
                                  visited <- readArray mask n
                                  return $ if not visited
                                           then Just n
                                           else Nothing)

                               let !next' = catMaybes next
                               updateMask next'
                               if null next'
                               then return graph
                               else fixMask $ step $ nub next'
                    fixMask $ step basis'

-- cayley graph from cayley table
cayleyTGraph :: CayleyTable GroupElement -> [GroupElement] -> Graph
cayleyTGraph t = cayleyGraph (times t) (1 + (snd $ snd $ bounds t))

{--cayley graph with generating set `basis`
--the generating set is assumed to not contain the identity
cayleyGraph t basis = fixMask dirNodes (emptyG $ gb+1) $ step basis' where
  basis'      = map unX basis          --nodes in generating set
  gb          = snd $ snd $ bounds t   --graph bounds
  update mask = (mask //) . flip zip (repeat True)   --set indices in the mask to True
  dirNodes = update (listArray (0, gb) $ repeat False) basis'
  step nodes = [(x, unX $ times t (X x) y) | x <- nodes, y <- basis]
  fixMask mask acc current
    | mask == newMask = newAcc
    | otherwise       = fixMask newMask newAcc (step $ nub next) where
      newMask          = update mask next
      (next, newAcc)   = foldr addNeigh ([], acc) current
      addNeigh (x,y) (loc, g) 
        | not $ mask!y = (y:loc, addDirEdge g x y)
        | otherwise    = (loc, addDirEdge g x y)
-}

-- cayley graph based on a generating set of permutations
cayleyPGraph' :: Int -> [Perm] -> Graph
cayleyPGraph' n = cayleyGraph algebra (product [1..n]) . toAlgebra' unlookup n where
  (!lookup, unlookup)     = toFromSym n
  algebra (X el1) (X el2) = unlookup $ unPerm $ (Perm $ lookup!el2) <> (Perm $ lookup!el1)

cayleyPGraph :: [Perm] -> Graph
cayleyPGraph xs = cayleyPGraph' ( maximum (map (length . unPerm) xs) ) xs

-- generate the subgroup before producing a pruned cayley graph
-- may take longer than cayleyPGraph
cayleyPGraphPruned :: [Perm] -> Graph
cayleyPGraphPruned xs = cayleyGraph algebra (length lookup) $ map unlookup xs where
  (!lookup, unlookup)     = toFromPerm xs
  algebra (X el1) (X el2) = unlookup $ (lookup!el2) <> (lookup!el1)

-- graph to list of 2-cycles
graphSwaps :: Graph -> [Perm]
graphSwaps gr = map (read . show) $ nub swaps where 
  ab         = snd $ bounds gr
  swaps      = assocs gr >>= (\(x, ys) -> map ((,) (x+1) . (+1)) ys)

-- symmetric group cayley graph from a "swap" graph
-- beware that 9 nodes is too many!
factorialG :: Graph -> Graph
factorialG gr = cayleyPGraph' (ab+1) generators where
  !generators = graphSwaps gr
  ab          = snd $ bounds gr

--GRAPH "FLOW" OPERATIONS------------------------------------------------------

-- from a node, find all possible n-distance 2 step walks
-- ((alg !! x) !! y) represents all nodes achieved by taking one step of length x and another of length y
flow' :: Graph -> Int -> ([[Vertex]], [[[Vertex]]])
flow' gr n = (classes, alg) where
  -- neighbors of each node
  nodeN = listArray (bounds gr) $ map (neighbors gr) $ indices gr
  classes = nodeN!n
  -- algebra elements corresponding to neighbors of neighbors
  alg = map (sumClasses [] [] . map (nodeN!)) classes
  -- coalesce classes
  sumClasses []  temp []     = []
  sumClasses zip temp []     = (concat temp):sumClasses [] [] zip
  sumClasses zip temp (x:xs)
    | null x                 = sumClasses zip temp xs
    | otherwise              = sumClasses (tail x:zip) (head x:temp) xs

-- extract only the flow types
flow = (snd .) . flow'

-- Map nodes in the same flow class to the same AlgebraElement, then aggregate
-- Only valid for vertex-transitive graphs
eachOfClass' :: Graph -> ([Int], [[AlgebraElement Int]])
eachOfClass' gr = (map length classes, map (map totalClass) alg) where 
  (classes, alg) = flow' gr 0
  n              = length classes
  toClass x      = c $ fromJust $ findIndex (x `elem`) classes
  totalClass     = mconcat . map toClass

-- only information about the classes
eachOfClass = snd . eachOfClass'

-- integer division of every element of eachOfClass by the size of the class
residues :: [Int] -> [[AlgebraElement Int]] -> Array (Int, Int) (AlgebraElement Int, AlgebraElement Int)
residues classes = listArray ((0,0),(n-1,n-1)) . concat . map (map (biplus . divMods)) where
  n  = length classes
  ns = listArray (0, n-1) $ map (flip divMod) classes
  asAlg m (x,y) = (x .* (c m), y .* (c m))
  divMods xs    = unzip $ map (\x -> asAlg x $ (ns!x) (xs.!x) ) [0..n-1]
  biplus (x,y)  = (mconcat x, mconcat y)

residuesG :: Graph -> Array (Int, Int) (AlgebraElement Int, AlgebraElement Int)
residuesG = uncurry residues . eachOfClass'

-- create cayley table based on neighbor classes
-- this is only valid for certain on general platonic graphs -- i.e., those which 
-- represent an n-dimensional regular figure
flowAlg :: Graph -> CayleyTable (AlgebraElement Int)
flowAlg = fmap fst . residuesG

-- flowAlg, but which returns Nothing if there is no algebra
maybeFlowAlg :: Graph -> Maybe (CayleyTable (AlgebraElement Int))
maybeFlowAlg gr
  | residueSum == 0 = Just $ fmap fst res
  | otherwise       = Nothing where
    res        = residuesG gr
    residueSum = sum $ unAlg $ mconcat $ elems $ fmap snd res

-- pseudo-character for a graph: the probabilities of ending up in each class
-- after two steps of a certain radius
pseudoCharacter gr = array (bounds alg) [(a, fmap (% denom x y) $ (alg!a)) | a@(x,y) <- indices alg] where
  asArray xs      = listArray ((0,0),((length $ head xs)-1, (length xs)-1)) $ concat xs
  (classes, alg') = eachOfClass' gr
  alg             = asArray alg'
  denom x y       = (classes!!x)*(classes!!y)
