--algebras derived by hand from platonic graphs
import Algebra

--2d platonic solids (i.e., regular polygons/cycle graphs)
triangle = cayley [C 1 1 .= 2.*c 0 .+ c 1]

square = cayley [C 1 1 .= 2.*c 0 .+ 2.*c 2, 
                 C 2 2 .= c 0,
                 C 1 2 .= c 1]

pentagon = cayley [C 1 1 .= 2.*c 0 .+ c 2,
                   C 2 2 .= 2.*c 0 .+ c 1,
                   C 1 2 .= c 1 .+ c 2]

hexagon = cayley [C 1 1 .= 2.*c 0 .+ c 2,
                  C 2 2 .= 2.*c 0 .+ c 2,
                  C 3 3 .= c 0,
                  C 1 2 .= c 1 .+ 2.*c 3,
                  C 1 3 .= c 2,
                  C 2 3 .= c 1]

heptagon = cayley [C 1 1 .= 2.*c 0 .+ c 2,
                  C 2 2 .= 2.*c 0 .+ c 3,
                  C 3 3 .= 2.*c 0 .+ c 1,
                  C 1 2 .= c 1 .+ c 3,
                  C 1 3 .= c 2 .+ c 3,
                  C 2 3 .= c 1 .+ c 2]

--true platonic solids
k4 = cayley [C 1 1 .= 3.*c 0 .+ 2.*c 1]

octahedron = cayley [C 1 1 .= 4.*c 0 .+ 2.*c 1 .+ 4.*c 2,
                     C 2 2 .= c 0,
                     C 1 2 .= c 1]

cube = cayley [C 1 1 .= 3.*c 0 .+ 2.*c 2,
               C 2 2 .= 3.*c 0 .+ 2.*c 2,
               C 3 3 .= c 0,
               C 1 2 .= 3.*c 3 .+ 2.*c 1,
               C 1 3 .= c 2,
               C 2 3 .= c 1]

icos = cayley [C 1 1 .= 5.*c 0 .+ 2.*c 1 .+ 2.*c 2,
               C 2 2 .= 5.*c 0 .+ 2.*c 1 .+ 2.*c 2,
               C 3 3 .= c 0,
               C 1 2 .= 5.*c 3 .+ 2.*c 1 .+ 2.*c 2,
               C 1 3 .= c 2,
               C 2 3 .= c 1]

--not sure about the correctness of this
dodec = cayley [C 1 1 .= 3.*c 0 .+ c 2,
                C 2 2 .= 6.*c 0 .+ 2.*c 1 .+ c 2 .+ 2.*c 3 .+ 2.*c 4,
                C 3 3 .= 6.*c 0 .+ 2.*c 1 .+ c 3 .+ 2.*c 2 .+ 2.*c 4,
                C 4 4 .= 3.*c 0 .+ c 2,
                C 5 5 .= c 0,
                C 1 2 .= 2.*c 1 .+ c 2 .+ c 3,
                C 1 3 .= 2.*c 4 .+ c 2 .+ c 3,
                C 1 4 .= c 3 .+ 3.*c 5,
                C 2 3 .= 6.*c 5 .+ 2.*c 1 .+ 2.*c 2 .+ c 3 .+ 2.*c 4,
                C 2 4 .= 2.*c 4 .+ c 2 .+ c 3,
                C 3 4 .= 2.*c 1 .+ c 2 .+ c 3,
                C 1 5 .= c 4,
                C 2 5 .= c 3,
                C 3 5 .= c 2,
                C 4 5 .= c 1]

--5-cell, the only 4d platonic solid I care to implement
k5 = cayley [C 1 1 .= 4.*c 0 .+ 3.*c 1]

complex = cayley [C 1 1 .= (-1).*c 0]
