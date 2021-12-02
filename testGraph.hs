import Graph
import Algebra
import Data.Array

-- testSumClasses = map sumClasses $ map (map (neighbors cube)) $ neighbors cube 0
-- [[0],[1,2,3],[5,4,6],[7]]
-- [[3,2,1],[0,4,6,0,4,5,0,6,5],[7,1,2,7,1,3,7,2,3],[5,6,4]]
-- [[6,4,5],[1,3,7,1,2,7,2,3,7],[0,4,5,5,0,6,0,4,6],[2,3,1]]
-- [[7],[4,5,6],[3,1,2],[0]]

--complete graphs (simplex skeletons)
k n = G $ array (0, n-1) $ [(i, [0..i-1] ++ [i+1..n-1]) | i <- [0..n-1]]

cycGraph 2 = k 2
cycGraph n = G $ array (0, n-1) $ [(i, [(i-1) `mod` n, (i+1) `mod` n]) | i <- [0..n-1]]

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

--DOES NOT INDUCE AN ALGEBRA
petersen = wordList ["bef", "acg", "bdh", "cei", "adj",
                     "aih", "bij", "cfg", "dfg", "egh" ]
pairs [] = []
pairs [x] = [(x,x)]
pairs (x:y:xs) = (x,y):pairs xs
