import Graph
import Algebra
import Symmetric

import Data.Array

import System.Process
import System.Posix.Signals

-- testSumClasses = map sumClasses $ map (map (neighbors cube)) $ neighbors cube 0
-- [[0],[1,2,3],[5,4,6],[7]]
-- [[3,2,1],[0,4,6,0,4,5,0,6,5],[7,1,2,7,1,3,7,2,3],[5,6,4]]
-- [[6,4,5],[1,3,7,1,2,7,2,3,7],[0,4,5,5,0,6,0,4,6],[2,3,1]]
-- [[7],[4,5,6],[3,1,2],[0]]

tet   = k 4

-- cube = crown 4
cube  = wordList ["bcd", 
                  "aeg", "aef", "afg", 
                  "bch", "bdh", "cdh", 
                  "efg"]

-- oct = npartite [2,2,2]
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
--the petersen graph is also the skeleton of the hemi-dodecahedron
--the hemi-icosahedron is the complete graph K_6
--the hemi-cube is the complete graph K_4 (i.e., the tetrahedron)
--the hemi-octahedron is 2 * K_3

--DOES NOT INDUCE AN ALGEBRA
--a.k.a. 4-mobius ladder
wagner = wordList ["bdg", "acf", "bdh", "ace", "dfh", "beg", "afh", "ceg"]

powersOf x graph = iterate (times (flowAlg graph) x) (c 0)
  
limRat :: Integral a => [a] -> [Double]
limRat = zipWith (\a b -> fromIntegral b / fromIntegral a) <*> tail

--compare two graphs by their algebras
--returns false if both graphs have no algebra
sameAlg a b = maybe False id $ do aa <- maybeFlowAlg a
                                  ba <- maybeFlowAlg b
                                  return $ aa == ba

showGraphS   g = "import networkx as nx \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ") \n\
                 \nx.draw(g) \n\
                 \plt.show()"

runPython script = do hand <- spawnCommand $ "python -c '" ++ script ++ "'"
                      _ <- waitForProcess hand
                      return ()

showGraph = runPython . showGraphS

pseudoReplS  g = "import networkx as nx \n\
                 \import sympy \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ")"

--ignore ctrl-c (which leaves the shell in an unstable state), then bootstrap python repl
pseudoRepl g = do oldCtrlC <- installHandler sigINT Ignore $ Nothing
                  let cmd = proc "python" ["-i", "-c", pseudoReplS g]
                  (_, _, _, hand) <- createProcess cmd
                  waitForProcess hand
                  installHandler sigINT oldCtrlC $ Nothing
                  return ()
