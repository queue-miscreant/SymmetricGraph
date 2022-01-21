#!/usr/bin/env python3
import networkx as nx
import scipy.linalg as linalg
import matplotlib.pyplot as plt
import sys

import timeit

def spectrum_to_mults(spec, tol=1e-14):
	"""
	Convert spectrum array to a dict, with eigenvalues as indices and
	multiplicities as entries.
	"""
	ret = {}
	for eig in spec:
		stop = False
		for i in ret.keys():
			if abs(eig - i) < tol:
				ret[i] += 1
				stop = True
				break
		if not stop:
			ret[eig] = 1
	return ret

# You'd think that a sensible graph theory library would use optimal eigenvalue 
# algorithms....
def mult_spectrum_naive(g, tol=1e-7):
	"""
	Compute eigenvalue dict of graph
	"""
	return spectrum_to_mults(nx.spectrum.adjacency_spectrum(g), tol)

# But the method above doesn't take into account the undirectedness of the graph
# (and hence, the symmetricity of its adjacency matrix)
def mult_spectrum(g, tol=1e-7):
	"""Compute eigenvalue dict of undirected graph, using scipy.eigh"""
	spectrum = linalg.eigh(nx.adjacency_matrix(g).todense()
		, eigvals_only=True, check_finite=False)
	return spectrum_to_mults(spectrum, tol)

def load_graph(path):
	"""Read a file containing a python list of edges into a NetworkX graph"""
	g = nx.Graph()
	with open(path) as a:
		# fortunately, Haskell and Python both have the same syntax for lists
		# No, I don't care how ill-advised this is
		g.add_edges_from(eval(a.readline()))
	return g
	
if __name__ == "__main__":
	g = load_graph(sys.argv[1])
	spec = mult_spectrum(g)
	[print(i, j) for i,j in spec.items()]
