module PythonNX where
--some assorted graphs, with functions for starting up a Python
--environment with networkx loaded

import Data.Graph

import System.Process
import System.Posix.Signals

import Cayley.Algebra
import Cayley.Symmetric
import Cayley.Graph
import Cayley.GraphAlg
import Cayley.Examples

runPython script = do hand <- spawnProcess "python" ["-c", script]
                      _ <- waitForProcess hand
                      return ()

showGraphS   g = "import networkx as nx \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ") \n\
                 \nx.draw(g) \n\
                 \plt.show()"

showDigraphS g = "import networkx as nx \n\
                 \import matplotlib.pyplot as plt \n\
                 \g = nx.DiGraph() \n\
                 \g.add_edges_from(" ++ (show $ toDirEdgeList g) ++ ") \n\
                 \nx.draw(g) \n\
                 \plt.show()"

showSpecS    g = "import networkx as nx \n\
                 \from compute_spectrum import mult_spectrum\n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ") \n\
                 \[print(i,j) for i,j in mult_spectrum(g).items()]"

showGraph = runPython . showGraphS
showDigraph = runPython . showDigraphS
showSpec  = runPython . showSpecS

pseudoReplS  g = "import networkx as nx \n\
                 \import sympy \n\
                 \import matplotlib.pyplot as plt \n\
                 \from compute_spectrum import *\n\
                 \g = nx.Graph() \n\
                 \g.add_edges_from(" ++ (show $ toEdgeList g) ++ ")"

--ignore ctrl-c (which leaves the shell in an unstable state), then bootstrap python repl
pseudoRepl g = do let cmd = proc "python" ["-i", "-c", pseudoReplS g]
                  (_, _, _, hand) <- createProcess cmd
                  oldCtrlC <- installHandler sigINT Ignore $ Nothing
                  waitForProcess hand
                  installHandler sigINT oldCtrlC $ Nothing
                  return ()
