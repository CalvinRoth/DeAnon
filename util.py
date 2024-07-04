import networkx as nx 
import numpy as np 

def clustering(G):
    return nx.transitivity(G)

def subgraph_centrality(G):
    pass 

def harmonic_cetnrality(G):
    return nx.centrality.harmonic_centrality(G)

def lambda1(G):
    vals = nx.linalg.adjacency_spectrum(G)
    return max(vals)


## 2nd largest eigenvalue of laplacian 
def lap2(G):
    vals = nx.linalg.laplacian_spectrum(G)
    return sorted(vals)[1]
