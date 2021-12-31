--matrix operations for use in graph theory
module Matrix where

import Data.Array

transIdx g x = (x, g x)
mapRange g r = map (\x -> (x, g x)) $ range r
arrayRange g = array <*> (mapRange g)

--composition of two arrays by function `f`
zipArr a b
  | ab == bb  = arrayRange (\x -> ((a!x),(b!x))) ab
  | otherwise = error "Array dimension mismatch" where
    ab = bounds a
    bb = bounds b

zipWithArr f = (fmap (uncurry f) .) . zipArr

plus :: (Num a, Ix i) => Array i a -> Array i a -> Array i a
plus = zipWithArr (+)

--matrix of all 1's
one :: (Num a, Num i, Ix i) => i -> Array (i,i) a
one n = arrayRange (const 1) range' where
  range' = ((0,0),(n-1,n-1))

--identity matrix
eye :: (Num a, Eq i, Num i, Ix i) => i -> Array (i,i) a
eye n = arrayRange (p . uncurry (==)) range' where
  range' = ((0,0),(n-1,n-1))
  p True  = 1
  p False = 0

--tensor product of matrices
tensor :: (Num a, Integral i, Ix i) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
tensor a b = arrayRange address ((0,0),(s,t)) where
  (m, n) = snd $ bounds a
  (p, q) = snd $ bounds b
  (s, t) = ((m+1)*(p+1)-1,(n+1)*(q+1)-1) --new dimensions
  address (x,y) = a!(x `quot` (p+1), y `quot` (q+1)) * b!(x `rem` (p+1), y `rem` (q+1)) 

--direct sum (upper-left tensor a + lower-right tensor b)
dPlus :: (Num a, Integral i, Ix i) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
dPlus a b = (ul `tensor` a) `plus` (lr `tensor` b) where
  ul = listArray ((0,0),(1,1)) $ [1,0,0,0]
  lr = listArray ((0,0),(1,1)) $ [0,0,0,1]

--box product (identity tensor b + a tensor identity)
box :: (Num a, Integral i, Ix i) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
box a b = (eye (m+1) `tensor` b) `plus` (a `tensor` eye (p+1)) where
  (m, n) = snd $ bounds a
  (p, q) = snd $ bounds b

--strong product (box plus tensor)
strong :: (Num a, Integral i, Ix i) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
strong a b = (a `box` b) `plus` (a `tensor` b)

--combine two products with an addition-like operation `p`
--addProd p = liftM2 (\x -> (p . x <*>))
--strong' = addProd plus box tensor

--print grayscale (ansi-escape) heatmap of 2-dim array
heatmap arr = mapM_ printRow rows where
  ab = snd $ snd $ bounds arr
  rows = map (\i -> map ((,) i) [0..ab]) [0..ab]
  max = maximum arr
  min' = minimum arr
  min | min' == max = max - 1
      | min' < 0    = min'
      | otherwise   = 0
  lerp i = ((24*(i - min)) `div` (max - min)) - 1
  grayscale (-1) = putStr $ "\x1b[40m  "
  grayscale i    = putStr $ "\x1b[48;5;" ++ show (232 + i) ++ "m  "
  reset = "\x1b[m\n"
  printRow tuples = mapM_ (grayscale . lerp . (arr!)) tuples >> putStr reset
