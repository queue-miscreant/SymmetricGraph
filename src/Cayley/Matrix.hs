{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--matrix operations for use in graph theory
module Cayley.Matrix where

import Data.Array

--I *could* just use GADTs here, but I don't know if it's worth it
type Matrix a = Array (Int, Int) a

mapRange :: Ix i => (i -> e) -> (i, i) -> [(i, e)]
mapRange g r = map (\x -> (x, g x)) $ range r

arrayRange :: Ix i => (i -> e) -> (i, i) -> Array i e
arrayRange g = array <*> (mapRange g)

--fill a square matrix with a value
fillMat :: Num a => a -> Int -> Matrix a
fillMat m n = listArray ((0,0),(n-1,n-1)) $ repeat m

zero :: Num a => Int -> Matrix a
zero = fillMat 0

one :: Num a => Int -> Matrix a
one = fillMat 1

--identity matrix
eye :: Num a => Int -> Matrix a
eye n = arrayRange (p . uncurry (==)) ((0,0),(n-1,n-1)) where
  p True  = 1
  p False = 0

zipArr :: Ix i => Array i a -> Array i b -> Array i (a,b)
zipArr a b
  | ab == bb  = arrayRange (\x -> ((a!x),(b!x))) ab
  | otherwise = error "Array dimension mismatch" where
    ab = bounds a
    bb = bounds b

--composition of two arrays by function `f`
zipWithArr :: Ix i => (a -> b -> c) 
  -> Array i a -> Array i b -> Array i c
zipWithArr f = (fmap (uncurry f) .) . zipArr

instance Num a => Num (Matrix a) where
  (+) = zipWithArr (+)
  (*) = zipWithArr (*) --NOT matrix multiplication
  abs = fmap abs
  negate = fmap negate
  signum = undefined
  fromInteger = listArray ((0,0),(0,0)) . pure . fromInteger

--tensor product of matrices
tensor :: Num a => Matrix a -> Matrix a -> Matrix a
tensor a b = arrayRange address ((0,0),(s,t)) where
  (m, n) = snd $ bounds a
  (p, q) = snd $ bounds b
  (s, t) = ((m+1)*(p+1)-1,(n+1)*(q+1)-1) --new dimensions
  address (x,y) = a!(x `quot` (p+1), y `quot` (q+1)) * b!(x `rem` (p+1), y `rem` (q+1)) 

--box product (identity tensor b + a tensor identity) on square matrices
box :: Num a => Matrix a -> Matrix a -> Matrix a
box a b = (eye (m+1) `tensor` b) + (a `tensor` eye (p+1)) where
  (m, n) = snd $ bounds a
  (p, q) = snd $ bounds b

--strong product (box plus tensor)
strong :: Num a => Matrix a -> Matrix a -> Matrix a
strong a b = (a `box` b) + (a `tensor` b)

--direct sum
dPlus :: Num a => Matrix a -> Matrix a -> Matrix a
dPlus a b = base // elements where
  (ab_m, ab_n) = snd $ bounds a
  (bb_m, bb_n) = snd $ bounds b
  elements = assocs a ++ (map (\((m,n), x) -> ((ab_m+m+1,ab_n+n+1), x)) $ assocs b)
  base     = listArray ((0,0), (ab_m+bb_m+1, ab_n+bb_n+1)) $ repeat 0

--print grayscale (ansi-escape) heatmap of matrix
heatmap :: (Integral a) => Matrix a -> IO ()
heatmap arr = mapM_ printRow rows where
  (ab_m, ab_n) = snd $ bounds arr
  rows = map (\i -> map ((,) i) [0..ab_n]) [0..ab_m]
  max  = maximum arr
  min' = minimum arr
  min | min' == max = max - 1
      | min' < 0    = min'
      | otherwise   = 0
  lerp i = ((24*(i - min)) `div` (max - min)) - 1
  grayscale (-1) = putStr $ "\x1b[40m  "
  grayscale i    = putStr $ "\x1b[48;5;" ++ show (232 + fromIntegral i) ++ "m  "
  reset = "\x1b[m\n"
  printRow tuples = mapM_ (grayscale . lerp . (arr!)) tuples >> putStr reset
