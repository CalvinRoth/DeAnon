import networkx as nx
import numpy as np

"By k-hop matrix I mean a matrix such that A(i,j)=1 if only if j is k hops away(0 else). "
# Preserving Link Privacy in Social Network Based Systems

def tHopMatrix(G,t):
    "Given a networkx graph, make a t-hop matrix"
    A = nx.adjacency_matrix(G).todense()
    n = len(G)
    for i in range(n):
        for j in range(n): #unweight graph if weighted
            if(A[i,j] > 0): A[i,j] = 1
    At = np.linalg.matrix_power(A, t)
    return At  

def randomWalk(A, v):
    """Given a matrix where  from i return a random one"""
    valids = [i for i in range(len(A[v,:])) if A[v,i]>0 ]
    return np.random.choice(valids)



def Anon(G, args):
    """Given networkx graph G and args return anonymized version of the network
        Args are [size of random walk to take t, number of tries to take on any random walk]
    """
    t = int(args[0])
    k_iters = int(args[1])
    n = len(G)
    G_new = np.zeros((n,n))
    walk_mat = tHopMatrix(G, t)
    for u in G.nodes:
        count = 1 
        for v in G.neighbors(u):
            loop = 1
            z = u
            while(loop < k_iters and (u == z or G_new[int(u),int(z)] )):
                z = randomWalk(walk_mat, v)
                loop += 1
            if(loop < k_iters):
                if(count == 1):
                    G_new[int(u),int(z)] = 1
                    count += 1
                else:
                    deg = G.degree(u)
                    term = (0.5*deg - 1) / (deg -1)
                    coin = np.random.random()
                    if(coin < term): 
                        G_new[int(u),int(z)] = 1
                    count += 1
    return nx.Graph(G_new), {i:i for i in range(n)}


