module Cayley.Examples where

import Data.Array
import Data.Graph

import Cayley.Graph

--GRAPH FAMILIES---------------------------------------------------------------

--complete graphs (simplex skeleta)
kG :: Int -> Graph
kG n = array (0, n-1) $ [(i, [0..i-1] ++ [i+1..n-1]) | i <- [0..n-1]]

--complete n-partite graphs
--when written pointfree and pattern matched, this complains about arity
npartiteG :: [Int] -> Graph
npartiteG xs | null $ tail xs = kG $ head xs
             | otherwise      = complement . foldl1 (dPlusG) $ map kG xs

--complete bipartite graphs
bipartiteG :: Int -> Int -> Graph
bipartiteG m n = npartiteG [m,n]

--crown graphs - bipartite graphs which lack a connection to the 'antipodal' node
crownG :: Int -> Graph
crownG m = array (0, (2*m-1)) $ [0..m-1] >>= neigh where
  neigh x = let (a,b) = unzip [(i+m,i) | i <- [0..m-1], i /= x]
            in  [(x,a),(x+m,b)]

--cycle graphs
cycleG :: Int -> Graph
cycleG 2 = kG 2
cycleG n = array (0, n-1) $ [(i, [(i-1) `mod` n, (i+1) `mod` n]) | i <- [0..n-1]]

--path graphs
pathG :: Int -> Graph
pathG 1 = kG 1
pathG 2 = kG 2
pathG n = let gr = cycleG n in gr // [(0, [1]), (n-1, [n-2])]

--star graphs
starG :: Int -> Graph
starG n = array (0, n-1) $ (0, [1..n-1]):[(i, [0]) | i <- [1..n-1]]

--wheel graphs
wheelG :: Int -> Graph
wheelG n = starG n `plusG` (kG 1 `dPlusG` cycleG (n-1))

--PLATONIC GRAPHS--------------------------------------------------------------
--well, and highly regular graphs

-- Tetrahedral graph
tetG   = kG 4

-- Cubical graph
cubeG  = fromWordList ["bcd", 
                       "aef", "aeg", "afg", 
                       "bch", "bdh", "cdh", 
                       "efg"]
-- cube = crownG 4

--Octahedral graph
octG   = fromWordList ["bcde",
                       "acef", "abdf", "acef", "abdf",
                       "bcde"]
-- oct = npartite [2,2,2]

-- Dodecahedral graph
dodecG = fromWordList ["bcd", 
                       "aef", "agh", "aij",
                       "bjk", "bgl", "cfm", "cin", "dho", "dep",
                       "elq", "fkr", "gnr", "hms", "ips", "joq",
                       "kpt", "lmt", "not",
                       "qrs"]

-- Icosahedral graph
icosG  = fromWordList ["bcdef", 
                       "acfgh", "abdhi", "aecij", "adfjk", "abekg",
                       "bfhkl", "bcgil", "cdhjl", "deikl", "efgjl",
                       "ghijk"]

-- Petersen graph
petersenG = fromWordList ["bef", "acg", "bdh", "cei", "adj",
                          "aih", "bij", "cfj", "dfg", "egh" ]

-- The Petersen graph is related to the 5-cell (4 simplex) in the following way
-- petersen = complement $ medial $ k 5

--the petersen graph is also the skeleton of the hemi-dodecahedron
--the hemi-icosahedron is the complete graph K_6
--the hemi-cube is the complete graph K_4 (i.e., the tetrahedron)
--the hemi-octahedron is 2 * K_3

--DOES NOT INDUCE AN ALGEBRA

-- Wagner graph, a.k.a. 4-mobius ladder
wagnerG = fromWordList ["bdg", "acf", "bdh", "ace", "dfh", "beg", "afh", "ceg"]
