module Algebra where
--abstract algebra types: cayley tables, group elements, ring elements (as dynamic arrays)

import Data.Array
import Data.Maybe (fromJust)
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

buildCayleyT :: a -> (Int -> a) -> [CayleyEntry a] -> CayleyTable a
buildCayleyT none g xs = empty (max mX mY) // (xs >>= (\(E x y) -> tuplify x y)) where
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
  readsPrec d = pure . flip (,) "" . X . read . fromJust . stripPrefix "X_"

instance Multiplication GroupElement where 
  times cay (X a) (X b) = cay!(a,b)

cayleyTGroup = buildCayleyT (X 0) X

--RINGLIKE----------------------------------------------------------------------

--generic "algebra" elements, with generic coefficients and standard addition
--c 0 is assumed to be the multiplicative identity
data AlgebraElement a = Alg {unAlg :: Array Int a}

c :: Num a => Int -> AlgebraElement a
c n = Alg $ array (0,n) $ (n,1):(zip [0..] $ replicate n 0)

z :: Num a => Int -> AlgebraElement a
z n = Alg $ listArray (0, n+1) $ repeat 0

--ringlike addition and multiplication
liftAlg :: Num a => (a -> a -> a) -> AlgebraElement a -> AlgebraElement a 
  -> AlgebraElement a
liftAlg f (Alg a) (Alg b)
  | ab >= bb  = Alg $ a // addIn bb
  | otherwise = Alg $ b // addIn ab where
    addIn n = map (\x -> (x, (a!x) `f` (b!x))) [0..n]
    ab = snd $ bounds a
    bb = snd $ bounds b

(.+) :: Num a => AlgebraElement a -> AlgebraElement a -> AlgebraElement a
(.+) = liftAlg (+)

(.*) :: Num a => a -> AlgebraElement a -> AlgebraElement a
(.*) a (Alg b) = Alg $ fmap (*a) b

--array indexing which defaults to 0
(.!) (Alg a) n 
  | inRange (bounds a) n = a!n
  | otherwise            = 0

cayleyTRing = buildCayleyT (z 0) c

infixl 6 .+
infixl 7 .*

instance Functor AlgebraElement where 
  fmap f = Alg . fmap f . unAlg

instance Num a => Semigroup (AlgebraElement a) where
  (<>) = (.+)

instance Num a => Monoid (AlgebraElement a) where
  mempty = z 0

instance (Num a, Eq a) => Eq (AlgebraElement a) where 
  (==) x y = all (==0) $ unAlg $ (x <> fmap negate y)

instance (Num a, Eq a, Show a) => Show (AlgebraElement a) where
  show (Alg a) = intercalate " + " $ coeff $ assocs a where
    coeff [] = []
    coeff ((i,c):xs)
      | c == 0    = coeff xs
      | c == 1    = ("C_" ++ show i):coeff xs
      | otherwise = (showParen True (show c ++) $ "C_" ++ show i):coeff xs

--the intent is to have the haskell code resemble how an algebra element is written
--but if I'm writing a show instance, then why not write a read one
instance (Read a, Num a) => Read (AlgebraElement a) where
  readsPrec d s = pure $ (flip (,) "") $ case (unparen $ dropWhile (==' ') s) of {
      Right (coeff, rest) -> let [(ele, rest2)] = lex rest in
          (read coeff .* (readEle ele)) <> read rest2;
      Left rest  -> if null rest then mempty
                    else (let [(ele, rest2)] = lex rest in
                      if ele == "+" then read rest2
                      else readEle ele <> read rest2);
   } where
    readEle xs     = case (stripPrefix "C_" xs) of 
      Just ys -> c $ read ys
      Nothing -> error $ "Could not parse indexed element '" ++ xs ++ "'"
    --hacky since this doesn't respect Nums which have parens in their reads
    unparen ('(':xs) = Right $ fmap tail $ break (==')') xs
    unparen xs       = Left xs

instance (Num a, Eq a) => Multiplication (AlgebraElement a) where 
  --multiply the algebra elements `a` and `b` respecting distributivity
  times cay (Alg a) (Alg b) = foldl (\acc (x,y) -> acc .+ (y .* (cay!x))) (z 0) prod where
    nonzero = filter ((/=) 0 . snd) . assocs
    --distribute, courteous of coefficients
    prod = map (uncurry unseq) $ ((,) <$> nonzero a <*> nonzero b)
    unseq (x,y) (z,w) = ((x,z), y*w)
