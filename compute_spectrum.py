#!/usr/bin/env python3
import networkx as nx
import scipy.linalg as linalg
import matplotlib.pyplot as plt
import sys

import timeit

def spectrum_to_mults(spec, tol=1e-14):
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

def mult_spectrum_naive(g, tol=1e-7):
	return spectrum_to_mults(nx.spectrum.adjacency_spectrum(g), tol)

def mult_spectrum(g, tol=1e-7):
	spectrum = linalg.eigh(nx.adjacency_matrix(g).todense(), eigvals_only=True, check_finite=False)
	return spectrum_to_mults(spectrum, tol)

def kernel_dim(g, tol=1e-7):
	return len( linalg.eigh(nx.adjacency_matrix(g).todense(), 
							eigvals_only=True, 
							check_finite=False, 
							subset_by_value=[-tol,tol]) )

def load_graph(path):
	g = nx.Graph()
	with open(path) as a:
		#no, I don't care how ill advised this is
		g.add_edges_from(eval(a.readline()))
	return g
	
if __name__ == "__main__":
	g = load_graph(sys.argv[1])
	spec = mult_spectrum(g)
	[print(i, j) for i,j in spec.items()]
