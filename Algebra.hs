module Algebra where
--abstract algebra types: elements, cayley table

import Data.Array
import Data.List (stripPrefix, intercalate)

--CAYLEY TABLES-----------------------------------------------------------------

--cayley table index helper object
data TableIndex = C Int Int | N Int Int --commutative vs noncommutative
tuplify (C i j) x = [((i,j),x),((j,i),x)]
tuplify (N i j) x = [((i,j),x)]

unIndex (C i j) = (i,j)
unIndex (N i j) = (i,j)

--object sugar for cayley table entry
data CayleyEntry a = E {
  cayAdd :: TableIndex,
  cayEl :: a
}

--sugar for nice expressions
(.=) = E
infixl 5 .=

--cayley table definition and helper functions
type CayleyTable a = Array (Int, Int) a

buildCayley :: a -> (Int -> a) -> [CayleyEntry a] -> CayleyTable a
buildCayley none g xs = empty (max mX mY) // (xs >>= (\(E x y) -> tuplify x y)) where
  (mX, mY) = maximum $ map (unIndex . cayAdd) xs
  zeros n = listArray ((0,0),(n,n)) $ repeat none
  --identity column/row
  empty n = zeros n // concat [[((0, n), g n), ((n, 0), g n)] | n <- [0..n]]

class Multiplication a where 
  times :: CayleyTable a -> a -> a -> a

--GROUPS------------------------------------------------------------------------

--generic "group" elements (which do not enforce associativity and invertibility)
--X 0 is assumed to be the group identity
data GroupElement = X {unX :: Int} deriving Eq

instance Show GroupElement where 
  show (X x) = "X_" ++ show x

instance Read GroupElement where 
  readsPrec d = pure . flip (,) "" . X . read . (\(Just a) -> a) . stripPrefix "X_"

instance Multiplication GroupElement where 
  times cay (X a) (X b) = cay!(a,b)

cayleyGrp = buildCayley (X 0) X

--RINGS-------------------------------------------------------------------------

--generic "algebra" elements, with generic coefficients and standard addition
--c 0 is assumed to be the multiplicative identity
data AlgebraElement = Alg {unAlg :: Array Int Int} deriving Eq
c :: Int -> AlgebraElement
c n = Alg $ array (0,n) $ (n,1):(zip [0..] $ replicate n 0)
z n = Alg $ listArray (0, n+1) $ repeat 0

--ringlike addition and multiplication
(.+) (Alg a) (Alg b)
  | ab >= bb  = Alg $ a // addIn bb
  | otherwise = Alg $ b // addIn ab where
    addIn n = map (\x -> (x, a!x + b!x)) [0..n]
    ab = snd $ bounds a
    bb = snd $ bounds b

(.*) a (Alg b) = Alg $ fmap (*a) b

--array indexing which defaults to 0
(.!) (Alg a) n 
  | inRange (bounds a) n = a!n
  | otherwise            = 0

cayley = buildCayley (z 0) c

infixl 6 .+
infixl 7 .*

instance Semigroup AlgebraElement where
  (<>) = (.+)

instance Monoid AlgebraElement where
  mempty = z 0

instance Show AlgebraElement where
  show (Alg a) = intercalate " + " $ coeff $ assocs a where
    coeff [] = []
    coeff ((i,c):xs)
      | c == 0    = coeff xs
      | c == 1    = ("C_" ++ show i):coeff xs
      | otherwise = (show c ++ "C_" ++ show i):coeff xs

--the intent is to have the haskell code resemble how it should be written
--but if I'm writing a show instance, then why not write a read one
--it should also be more decision tree like, but I can't be bothered to use Either right now
instance Read AlgebraElement where
  readsPrec d s = pure $ (flip (,) "") $ case (strip maybeCoeff) of {
      Nothing -> let subscript = read $ maybe undefined id (strip lex2) in
                 if lex3 /= "+" then (read maybeCoeff .* (c subscript)) --next lex has the coefficient
                                else (read maybeCoeff .* (c subscript)) <> read rest3;
      Just n  -> if lex2 /= "+" then c $ read n
                                else (c $ read n) <> (read rest2);
   } where
    [(maybeCoeff, rest)]  = lex s
    [(lex2,       rest2)] = lex rest
    [(lex3,       rest3)] = lex rest2
    strip       = stripPrefix "C_"

instance Multiplication AlgebraElement where 
  --multiply the algebra elements `a` and `b` respecting distributivity
  times cay (Alg a) (Alg b) = foldl (\acc (x,y) -> acc .+ (y .* (cay!x))) (z 0) prod where
    nonzero = filter ((/=) 0 . snd) . assocs
    --distribute, courteous of coefficients
    prod = map (uncurry unseq) $ ((,) <$> nonzero a <*> nonzero b)
    unseq (x,y) (z,w) = ((x,z), y*w)
