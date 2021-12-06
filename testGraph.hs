import Graph
import Algebra

-- testSumClasses = map sumClasses $ map (map (neighbors cube)) $ neighbors cube 0
-- [[0],[1,2,3],[5,4,6],[7]]
-- [[3,2,1],[0,4,6,0,4,5,0,6,5],[7,1,2,7,1,3,7,2,3],[5,6,4]]
-- [[6,4,5],[1,3,7,1,2,7,2,3,7],[0,4,5,5,0,6,0,4,6],[2,3,1]]
-- [[7],[4,5,6],[3,1,2],[0]]

tet   = k 4

cube  = wordList ["bcd", 
                  "aeg", "aef", "agf", 
                  "bch", "bdh", "cdh", 
                  "efg"]

oct   = wordList ["bcde",
                  "acef", "abdf", "acef", "abdf",
                  "bcde"]

dodec = wordList ["bcd", 
                  "aef", "agh", "aij",
                  "bjk", "bgl", "cfm", "cin", "dho", "dep",
                  "elq", "fkr", "gnr", "hms", "ips", "joq",
                  "kpt", "lmt", "not",
                  "qrs"]

icos  = wordList ["bcdef", 
                  "acfgh", "abdhi", "aecij", "adfjk", "abekg",
                  "bfhkl", "bcgil", "cdhjl", "deikl", "efgjl",
                  "ghijk"]

--the petersen graph is related to the 5-cell (4 simplex) in the following way
--petersen = complement $ medial $ k 5
petersen = wordList ["bef", "acg", "bdh", "cei", "adj",
                     "aih", "bij", "cfj", "dfg", "egh" ]

--DOES NOT INDUCE AN ALGEBRA
wagner = wordList ["bdg", "acf", "bdh", "ace", "dfh", "beg", "afh", "ceg"]

pairs [] = []
pairs [x] = [(x,x)]
pairs (x:y:xs) = (x,y):pairs xs

powersOf x graph = iterate (times (flowAlg graph) x) (c 0)
  
limRat :: Integral a => [a] -> [Double]
limRat = zipWith (\a b -> fromIntegral b / fromIntegral a) <*> tail
