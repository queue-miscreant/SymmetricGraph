{-# LANGUAGE BangPatterns #-}
--operations on the symmetric group
module Symmetric where

import Data.List
import Data.Array
import Data.Array.ST

import Data.Maybe
import qualified Data.Set
import Control.Monad (foldM)

import Algebra
import Graph
import GraphAlg

--all permutations of the set [1..x]
--plain changes without the reversing
{-
permutation x = permute [1..x] where
  permute [x]    = [[x]]
  permute (x:xs) = concat [reinsert x p | p <- permute xs]
  reinsert y ys  = [(\(a,b) c -> a ++ (c:b)) (splitAt z ys) y | z <- [0..(length ys)]]
-}

--TODO: better?
plainChanges n = change n where 
  change 1 = [[1]]
  change n = concat $ zipWith (drag n) (cycle [reverse, id]) $ change $ n-1
  drag n f x  = f [let (a,b) = splitAt m x in a ++ (n:b) | m <- [0..n-1]]
--permutation n = permutations [1..n]
permutation = plainChanges

--apply one permutation over another (1-based)
--since the symmetric group grows so quickly, efficiency can be disregarded
apply xs = map ((xs !!) . (+(-1)))

data Perm = Perm {unPerm :: [Int]} --not deriving eq, since it can be difficult to tell

instance Semigroup Perm where
  (<>) (Perm xs) (Perm ys) = Perm $ case lxs `compare` lys of {
    GT -> apply xs $ ys ++ [lys + 1..lxs];
    LT -> apply (xs ++ [lxs + 1..lys]) ys;
    EQ -> apply xs ys;
  } where lxs = length xs
          lys = length ys

app a b = show $ (read a <> read b :: Perm)

--show permutation in cycle notation
--TODO: move the code to convert to/from cycle notation to their own functions
instance Show Perm where
  show (Perm xs) = showPerm' Data.Set.empty 1 xs
    where showPerm' ss n xs 
              | n >= lx   = ""
              | length exhaust == 1 = next
              | otherwise = '(':(intercalate "," $ map show exhaust) ++ ")" ++ next
            where exhaust = navCycle n xs
                  ss'     = (Data.Set.fromList exhaust) `Data.Set.union` ss
                  next    = showPerm' ss' (head $ dropWhile (`Data.Set.member` ss') [n..]) xs
          navCycle n xs = n:(takeWhile (/=n) $ tail $ iterate ((xs !!) . (+(-1))) n)
          lx = length xs

--read cycle notation
--this is abjectly terrible, but it gets the job done
instance Read Perm where
  readsPrec _ = (\a -> [(a, "")]) . Perm . buildList . parseCycle []
    where parseCycle [] "" = []
          parseCycle ds "" = [ds]
          parseCycle ds (s:ss)
            | s == '('     = parseCycle [] ss
            | s == ')'     = ds:parseCycle [] ss
            | otherwise    = convert ds $ break notInt (s:ss)
          notInt = not . (`elem` ['0'..'9'])
          convert ds (x,y) = parseCycle ((read x :: Int):ds) $ dropWhile (\x -> notInt x && x /= ')') y
          --x1 tells us where x2 should go
          applyCycle x2 [x1] ys = rejoin x1 $ splitAt (x2 - 1) ys
          applyCycle s (x1:x2:xs) ys = applyCycle s (x2:xs) $ rejoin x1 $ splitAt (x2 - 1) ys
          rejoin n (xs, ys) = xs ++ n:tail ys
          --build result
          buildList [] = [1]
          buildList xs = foldl (flip $ \(y:ys) -> applyCycle y (y:ys)) [1..maximum (map maximum xs)] xs

--graph to list of two cycles
graphSwaps :: Graph -> [Perm]
graphSwaps (G a) = map (read . show) swaps where 
  ab         = snd $ bounds a
  swaps      = assocs a >>= (\(x, ys) -> map ((,) (x+1) . (+1)) ys)

inverse (Perm xs) = Perm $ map ((+1) . fromJust . (`findIndex` xs) . (==)) [1..length xs]

evenperms n = nubBy (\x y -> unPerm x == unPerm y) $ [a <> (b <> (inverse a <> inverse b)) | a <- p n, b <- p n] where
  p = map Perm . permutation

--ALGEBRAIC OPERATIONS----------------------------------------------------------

--pair of array containing every permutation of order n and
--getter function which obtains a permutation's corresponding algebra element
toFromSym n    = (lookup, unlookup) where
  order        = product [1..n] - 1
  lookup       = listArray (0, order) $ permutation n
  unlookup' eq = foldM (\_ x -> if (lookup!x == eq) then Left x else Right (x+1)) 0 [0..order]
  unlookup     = X . either id undefined . unlookup'

symmetric' n = (lookup, unlookup, products) where
  (lookup, unlookup)     = toFromSym n
  order                  = snd $ bounds lookup
  idx                    = ((0,0),(order,order))
  algebra el1 el2        = unlookup $ unPerm $ (Perm $ lookup!el2) <> (Perm $ lookup!el1)
  applyLookup el1 el2    = ((el1,el2), el1 `algebra` el2)
  products               = array idx $ map (uncurry applyLookup) $ range idx

--symmetric group of order n cayley table
symmetric = (\(_,_,cayley) -> cayley) . symmetric'

--symmetric groups grow so rapidly that storing the cayley table is infeasible
symmetricCayleyG n = cayleyGraph' algebra (product [1..n]) . toAlgebra' unlookup n where
  (!lookup, unlookup)     = toFromSym n
  algebra (X el1) (X el2) = unlookup $ unPerm $ (Perm $ lookup!el2) <> (Perm $ lookup!el1)

--convert permutation to purely algebraic representation in S_n
toAlgebra' from n = nub . map (from . unPerm . (Perm [1..n] <>))
toAlgebra n = let (_, from) = toFromSym n in toAlgebra' from n

--derived cayley graph on symmetric group from a "swap" graph
--beware that 8 nodes is too many!
factorialG g@(G a) = symmetricCayleyG (ab+1) generators where
  !generators = graphSwaps g 
  ab          = snd $ bounds a

--number of nodes who share a distance from the identity
factorialClasses = map length . flip neighbors 0 . factorialG
