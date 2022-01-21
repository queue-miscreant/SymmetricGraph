import Cayley.Graph (toEdgeList)
import Cayley.GraphAlg (factorialG)

cube  = fromWordList ["bcd", 
                      "aeg", "aef", "afg", 
                      "bch", "bdh", "cdh", 
                      "efg"]

main = writeFile "cube edges.txt" $ show $ toEdgeList $ factorialG $ cube
