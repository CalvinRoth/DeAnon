import networkx as nx 
import numpy as np 


def addDel(G, k=1000):
    """Takes a networkx graph and does (add a currently non existing edge, delete an existing) k times"""
    edges = list(G.edges())
    np.random.shuffle(edges)
    n = len(G)
    G_new = nx.empty_graph(n)

    for i in range(k):
        while(True):
            u = np.random.randint(0, n)
            v = np.random.randint(0, n)
            if( (u,v) not in edges):
                break 
        a,b = edges.pop()
        edges.append((u,v))
    return nx.Graph(edges)
            