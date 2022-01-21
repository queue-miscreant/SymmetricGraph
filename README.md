# SymmetricGraph
Some nice graph/group theory tools.

Two cabal projects are included: the tools themselves (CayleyOps) and output
functions and executables (GraphOut). Also includes a Python file 
(`compute_spectrum.py`) for running eigenvalue algorithms on files composed 
of edge lists.

While there are executables included in the builds, I intended it mainly to be
some study tools to be used from the GHCi REPL.

CayleyOps
---------

This package is composed of the following modules:

- Cayley
  - Cayley.Algebra
  - Cayley.Matrix
  - Cayley.Graph
  - Cayley.Symmetric
  - Cayley.GraphAlg
  - Cayley.Examples

### Cayley.Algebra
Abstract algebra types and manipulations. Supports finite groups and rings (over
a numeric type) with fully-generic multiplications, specified by Cayley table.

### Cayley.Matrix
Matrix operations pertinent to Graph theory, particularly direct sums, tensor
products, and box products.

### Cayley.Graph
Various graph manipulations based on `Data.Graph`. Includes adjacency matrix 
and edge list conversions, medial graphs, partitioning nodes by radii from a 
particular node, weak vs. strong regularity testing.

### Cayley.Symmetric
Definitions and tools useful in building groups out of permutations, particularly 
symmetric group of a certain order.

### Cayley.GraphAlg
Graph- and group-theoretical operations, like Cayley graph generation. Can also 
build probability (fusion?) algebras from graphs, where the elements of the 
algebra are steps on the graph of a particular size.

### Cayley.Examples
Some example graph families and graphs. Includes complete graphs, paths, cycles, 
wheels, and skeleta of Platonic solids.


GraphOut
--------

This package is composed of the following modules:

- Cayley.SymOut
- PythonNX

### Cayley.SymOut
Various file-writing capabilities for both edge lists and adjacency matrices.

### PythonNX
Tools for launching a Python REPL from Haskell. These scripts depend on
NetworkX and `compute_spectrum.py`. Move the latter file around if it's not
found by `cabal repl`.
