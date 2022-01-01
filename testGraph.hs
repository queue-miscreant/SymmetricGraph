--some assorted graphs, with functions for starting up a Python
--environment with networkx loaded

import Algebra
import Symmetric
import Graph
import GraphAlg

import System.Process
import System.Posix.Signals

tet   = k 4

-- cube = crown 4
cube  = fromWordList ["bcd", 
                      "aeg", "aef", "afg", 
                      "bch", "bdh", "cdh", 
                      "efg"]

-- oct = npartite [2,2,2]
oct   = fromWordList ["bcde",
                      "acef", "abdf", "acef", "abdf",
                      "bcde"]

dodec = fromWordList ["bcd", 
                      "aef", "agh", "aij",
                      "bjk", "bgl", "cfm", "cin", "dho", "dep",
                      "elq", "fkr", "gnr", "hms", "ips", "joq",
                      "kpt", "lmt", "not",
                      "qrs"]

icos  = fromWordList ["bcdef", 
                      "acfgh", "abdhi", "aecij", "adfjk", "abekg",
                      "bfhkl", "bcgil", "cdhjl", "deikl", "efgjl",
                      "ghijk"]

--the petersen graph is related to the 5-cell (4 simplex) in the following way
--petersen = complement $ medial $ k 5
petersen = fromWordList ["bef", "acg", "bdh", "cei", "adj",
                         "aih", "bij", "cfj", "dfg", "egh" ]

--the petersen graph is also the skeleton of the hemi-dodecahedron
--the hemi-icosahedron is the complete graph K_6
--the hemi-cube is the complete graph K_4 (i.e., the tetrahedron)
--the hemi-octahedron is 2 * K_3

--DOES NOT INDUCE AN ALGEBRA
--a.k.a. 4-mobius ladder
wagner = fromWordList ["bdg", "acf", "bdh", "ace", "dfh", "beg", "afh", "ceg"]

showGraphS   g = "import networkx as nx \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ") \n\
                 \nx.draw(g) \n\
                 \plt.show()"

runPython script = do hand <- spawnProcess "python" ["-c", script]
                      _ <- waitForProcess hand
                      return ()

showGraph = runPython . showGraphS

pseudoReplS  g = "import networkx as nx \n\
                 \import sympy \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ")"

--ignore ctrl-c (which leaves the shell in an unstable state), then bootstrap python repl
pseudoRepl g = do let cmd = proc "python" ["-i", "-c", pseudoReplS g]
                  (_, _, _, hand) <- createProcess cmd
                  oldCtrlC <- installHandler sigINT Ignore $ Nothing
                  waitForProcess hand
                  installHandler sigINT oldCtrlC $ Nothing
                  return ()
