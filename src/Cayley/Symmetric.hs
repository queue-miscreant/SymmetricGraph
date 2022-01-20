{-# LANGUAGE BangPatterns #-}
--operations on the symmetric group
module Cayley.Symmetric where

import Control.Monad (foldM)

import Data.List
import Data.Array
import Data.Graph

import Data.Maybe
import qualified Data.Set

import Cayley.Algebra
import Cayley.Graph

--enumerate all permutations of order n by plain changes
--TODO: do this better better?
plainChanges :: Int -> [[Int]]
plainChanges 1 = [[1]]
plainChanges n = concat $ zipWith (drag n) (cycle [reverse, id]) $ plainChanges $ n-1 where
  drag n f x  = f [let (a,b) = splitAt m x in a ++ (n:b) | m <- [0..n-1]]

permutation :: Int -> [[Int]]
--permutation n = permutations [1..n]
permutation = plainChanges

--destination of an index according to a permutation
applyOne :: [Int] -> Int -> Int
applyOne xs = ((xs !!) . (+(-1)))

--apply one permutation over another (1-based)
--disregarding efficiency since the symmetric group grows so quickly
apply :: [Int] -> [Int] -> [Int]
apply xs = map (applyOne xs)

--SYMMETRIC GROUP ELEMENT OPERATIONS--------------------------------------------

--permutations, invariant w.r.t. length of underlying list, supporting natural
--monoidal composition and displaying as cycle notation
data Perm = Perm {unPerm :: [Int]}

--extract a list of cycles from a Perm
extractCycles :: Perm -> [[Int]]
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
permFromRevCycles :: [[Int]] -> Perm
permFromRevCycles = Perm . buildList where
  --build result
  buildList [] = [1]
  buildList xs = foldl (flip $ \(y:ys) -> applyCycle y (y:ys)) [1..maximum (map maximum xs)] xs
  --x1 tells us where x2 should go
  applyCycle x2 [x1] ys = rejoin x1 $ splitAt (x2 - 1) ys
  applyCycle s (x1:x2:xs) ys = applyCycle s (x2:xs) $ rejoin x1 $ splitAt (x2 - 1) ys
  rejoin n (xs, ys) = xs ++ n:tail ys

--inversePerm :: Perm -> Perm
--inversePerm = permFromRevCycles . extractCycles

--invert a permutation
inversePerm :: Perm -> Perm
inversePerm (Perm xs) = Perm $ map ((+1) . fromJust . (`findIndex` xs) . (==)) [1..length xs]

--symmetric group of a particular order, as a list of permutation objects
symmetric :: Int -> [Perm]
symmetric = map Perm . permutation

--derived subgroup of a list of permutations
--remember that not all derived series terminate in the trivial group
derivedGroup :: [Perm] -> [Perm]
derivedGroup ps = nub [commutator a b | a <- ps, b <- ps] where 
  commutator a b = a <> (b <> (inversePerm a <> inversePerm b))

--alternating group 
alternating :: Int -> [Perm]
alternating = derivedGroup . symmetric

--normalize lengths to the longer of two lists
normalizePair :: [Int] -> [Int] -> ([Int], [Int])
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
app :: String -> String -> String
app a b = show $ (read a <> read b :: Perm)

--run generating set `xs`, producing all members of a subgroup
generatingSet :: [Perm] -> [Perm]
generatingSet xs = fixList xs xs where 
  fixList ss ys
    | ss == ss' = ss' 
    | otherwise = fixList ss' advance where 
        ss'     = ss `union` advance
        advance = nub $ (<>) <$> xs <*> ys 

--enumeration of the group formed by the generating set `xs`
generatingTable :: [Perm] -> Array Int Perm
generatingTable xs = listArray (0,n-1) $ (Perm [1,2]):noId gset where
  n = length gset
  gset = generatingSet xs
  noId = filter (/= mempty)

--ALGEBRAIC OPERATIONS----------------------------------------------------------

--find the index (wrapped as a GroupElement) of a value
--this could probably be improved with a Map
unlookupArr :: Eq a => Array Int a -> a -> GroupElement
unlookupArr arr eq = X $ either id undefined $ foldM unlookup' 0 $ indices arr where 
  unlookup' _ x = if (arr!x == eq) then Left x else Right (x+1)

--pair of array containing every permutation of order n and
--getter function which obtains a permutation's corresponding algebra element
toFromSym :: Int -> (Array Int [Int], [Int] -> GroupElement)
toFromSym n    = (lookup, unlookupArr lookup) where
  order        = product [1..n] - 1
  lookup       = listArray (0, order) $ permutation n

--generatingTable with a getter
--same pair as for `toFromSym`, but uses `Perm` rather than [Int] since the
--lengths of the underlying lists are not regularized
toFromPerm :: [Perm] -> (Array Int Perm, Perm -> GroupElement)
toFromPerm xs = (lookup, unlookupArr lookup) where
  lookup = generatingTable xs

symmetricTable' :: Int -> (Array Int [Int], [Int] -> GroupElement, CayleyTable GroupElement)
symmetricTable' n = (lookup, unlookup, products) where
  (lookup, unlookup)     = toFromSym n
  order                  = snd $ bounds lookup
  idx                    = ((0,0),(order,order))
  algebra el1 el2        = unlookup $ unPerm $ (Perm $ lookup!el2) <> (Perm $ lookup!el1)
  applyLookup el1 el2    = ((el1,el2), el1 `algebra` el2)
  products               = array idx $ map (uncurry applyLookup) $ range idx

--symmetric group of order n cayley table
symmetricTable :: Int -> CayleyTable GroupElement
symmetricTable = (\(_,_,cayley) -> cayley) . symmetricTable'

--convert permutation to purely algebraic representation in S_n
toAlgebra' :: ([Int] -> GroupElement) -> Int -> [Perm] -> [GroupElement]
toAlgebra' from n = nub . map (from . unPerm . (Perm [1..n] <>))

toAlgebra :: Int -> [Perm] -> [GroupElement]
toAlgebra n = toAlgebra' (snd $ toFromSym n) n
