{-# LANGUAGE BangPatterns #-}
--operations on the symmetric group
module Symmetric where

import Data.List
import Data.Graph
import Data.Array
import Data.Array.ST

import Data.Maybe
import qualified Data.Set
import Control.Monad (foldM)

import Algebra
import Graph

--enumerate all permutations of order n by plain changes
--TODO: better?
plainChanges n = change n where 
  change 1 = [[1]]
  change n = concat $ zipWith (drag n) (cycle [reverse, id]) $ change $ n-1
  drag n f x  = f [let (a,b) = splitAt m x in a ++ (n:b) | m <- [0..n-1]]
--permutation n = permutations [1..n]
permutation = plainChanges

applyOne xs = ((xs !!) . (+(-1)))
--apply one permutation over another (1-based)
--since the symmetric group grows so quickly, efficiency can be disregarded
apply xs = map (applyOne xs)

data Perm = Perm {unPerm :: [Int]} --not deriving eq, since it can be difficult to tell

inverse' (Perm xs) = Perm $ map ((+1) . fromJust . (`findIndex` xs) . (==)) [1..length xs]

symmetric = map Perm . permutation

derivedGroup ps = nubBy (\x y -> unPerm x == unPerm y) [a <> (b <> (inverse a <> inverse b)) | a <- ps, b <- ps] 

--alternating group 
alternating = derivedGroup . symmetric

--extract a list of cycles from a Perm object
extractCycles (Perm xs) = extractCycles' Data.Set.empty 1 where 
  navCycle n xs = n:(takeWhile (/=n) $ tail $ iterate (applyOne xs) n)
  lx = length xs
  extractCycles' mask n 
    | n >= lx   = []
    | length exhaust == 1 = extractCycles' newMask next 
    | otherwise           = exhaust:extractCycles' newMask next where 
        exhaust = navCycle n xs
        newMask = (Data.Set.fromList exhaust) `Data.Set.union` mask
        next    = head $ dropWhile (`Data.Set.member` newMask) [n..]

--build a permutation from a list of (reversed) cycles
--for example, permFromCycles [[1,2,3]] = (3 2 1) rather than (1 2 3)
permFromRevCycles = Perm . buildList where
  --build result
  buildList [] = [1]
  buildList xs = foldl (flip $ \(y:ys) -> applyCycle y (y:ys)) [1..maximum (map maximum xs)] xs
  --x1 tells us where x2 should go
  applyCycle x2 [x1] ys = rejoin x1 $ splitAt (x2 - 1) ys
  applyCycle s (x1:x2:xs) ys = applyCycle s (x2:xs) $ rejoin x1 $ splitAt (x2 - 1) ys
  rejoin n (xs, ys) = xs ++ n:tail ys

inverse = permFromRevCycles . extractCycles

--normalize lengths to the longer of two lists
normalizePair xs ys = case lx `compare` ly of {
    GT -> (xs, ys ++ [ly+1..lx]);
    LT -> ((xs ++ [lx+1..ly]), ys);
    EQ -> (xs, ys);
  } where lx = length xs
          ly = length ys

instance Semigroup Perm where
  (<>) (Perm xs) (Perm ys) = Perm $ (uncurry apply) $ normalizePair xs ys 

instance Monoid Perm where
  mempty = Perm [1,2]

instance Eq Perm where 
  (==) (Perm xs) (Perm ys) = uncurry (==) $ normalizePair xs ys

--show permutation in cycle notation
instance Show Perm where
  show x = (concat . mapM showCycle (extractCycles x)) "" where 
    showCycle x = showParen (length x > 1) ((intercalate "," $ map show x)++)

--read cycle notation
--this is abjectly terrible, but it gets the job done
instance Read Perm where
  readsPrec _ = (\a -> [(a, "")]) . permFromRevCycles . parseCycle []
    where parseCycle [] "" = []
          parseCycle ds "" = [ds]
          parseCycle ds (s:ss)
            | s == '('     = parseCycle [] ss
            | s == ')'     = ds:parseCycle [] ss
            | otherwise    = convert ds $ break notInt (s:ss)
          notInt = not . (`elem` ['0'..'9'])
          convert ds (x,y) = parseCycle ((read x :: Int):ds) $ dropWhile (\x -> notInt x && x /= ')') y

--apply a string of cycles over another string of cycles
--effectively lifts `apply` to strings of cycles
app a b = show $ (read a <> read b :: Perm)

--run generating set `xs`, producing all members of a subgroup
generatingSet xs = fixList xs xs where 
  fixList ss ys
    | ss == ss' = ss' 
    | otherwise = fixList ss' advance where 
        ss'     = ss `union` advance
        advance = nub $ (<>) <$> xs <*> ys 

generatingTable xs = listArray (0,n-1) $ (Perm [1,2]):noId gset where
  n = length gset
  gset = generatingSet xs
  noId = filter (/= mempty)

--ALGEBRAIC OPERATIONS----------------------------------------------------------

unlookupArr arr n eq = X $ either id undefined $ foldM unlookup' 0 [0..n] where 
  unlookup' _ x = if (arr!x == eq) then Left x else Right (x+1)

--pair of array containing every permutation of order n and
--getter function which obtains a permutation's corresponding algebra element
toFromSym n    = (lookup, unlookupArr lookup order) where
  order        = product [1..n] - 1
  lookup       = listArray (0, order) $ permutation n

toFromPerm xs = (lookup, unlookupArr lookup (length lookup)) where
  lookup = generatingTable xs

symmetricTable' n = (lookup, unlookup, products) where
  (lookup, unlookup)     = toFromSym n
  order                  = snd $ bounds lookup
  idx                    = ((0,0),(order,order))
  algebra el1 el2        = unlookup $ unPerm $ (Perm $ lookup!el2) <> (Perm $ lookup!el1)
  applyLookup el1 el2    = ((el1,el2), el1 `algebra` el2)
  products               = array idx $ map (uncurry applyLookup) $ range idx

--symmetric group of order n cayley table
symmetricTable = (\(_,_,cayley) -> cayley) . symmetricTable'

--convert permutation to purely algebraic representation in S_n
toAlgebra' from n = nub . map (from . unPerm . (Perm [1..n] <>))
toAlgebra n = let (_, from) = toFromSym n in toAlgebra' from n
