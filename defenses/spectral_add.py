import numpy as np 
import networkx as nx 

"Source https://epubs.siam.org/doi/abs/10.1137/1.9781611972788.67"


def find_increaser(G, L, edges, edge, lambdas, v1, mus, u2):
    """
    Takes network G
    laplacian L,
    list of edges
    edge candidate to drop 
    eigenvalues of G
    first eigenvector G, 
    eigenvalues of L,
    second eigenvector of L
    ++++++++++++++++++++++
    Return: an edge that if it is add and 'edge' would increase the largest eigenvalue
    """
    n = len(G)
    (i,j) = edge 

    for p in range(n):
        for q in range(n):
            if( (p,q) in edges or (q,p) in edges): continue
            t1 = bool(v1[i]*v1[j] - v1[p]*v1[q] >0 )
            t2 = bool(u2[i]*u2[j] - u2[p]*u2[q] < 0)
            a = sum([i**2 for i in (u2[i], u2[j], u2[p],u2[q])])
            b = 2 * (u2[p]*u2[q] - u2[i]*u2[j] )
            if(b==0): continue
            t3 = bool(mus[2] - mus[1]  > (a/b))
            if(t1 and t2 and t3): 
                return (p,q)
    return (None,None) 



def find_decreaser(G, L, edges, edge, lambdas, v1, mus, u2):
    """
    Takes network G
    laplacian L,
    list of edges
    edge candidate to drop 
    eigenvalues of G
    first eigenvector G, 
    eigenvalues of L,
    second eigenvector of L
    ++++++++++++++++++++++
    Return: an edge that if it is add and 'edge' would decrease the largest eigenvalue
    """
    n = len(G)
    (i,j) = edge 

    for p in range(n):
        for q in range(n):
            if( (p,q) in edges or (q,p) in edges): continue
            t1 = bool(v1[i]*v1[j] - v1[p]*v1[q] < 0 )
            t2 = bool(u2[i]*u2[j] - u2[p]*u2[q] > 0)
            a = sum([i**2 for i in (v1[i], v1[j], v1[p],v1[q])])
            b = 2 * (v1[p]*v1[q] - v1[i]*v1[j] )
            if(b==0):continue
            t3 = bool(lambdas[0] - lambdas[1]  > (a/b))
            if(t1 and t2 and t3): 
                return (p,q)
    return (None,None) 

def find_eigs(A):
    """Given adj. matrix find the eigenvalue/vectors. This can be speed up as we don't need all of them just the 2 largest"""
    values, vectors = np.linalg.eig(A)
    return values, vectors[:, :2]

def Anon(G,args):
    """ Given a networkx Graph G and [number of iterations] perform the spectral preserving add/del method 
    """
    k_iters = int(args[0])
    L = nx.laplacian_matrix(G).todense()
    A = nx.adjacency_matrix(G).todense()
    lambdas, v1 = find_eigs(A) #dont need all of them
    v1 = np.real(v1[:, 0])
    mus, u2 = find_eigs(L)
    u2 = np.real(u2[:,1])
    print(len(G))
    k = 0 
    edges = {edge for edge in G.edges()}
    while(k <k_iters):
        (t,w) = edges.pop()
        t = int(t)
        w = int(w) 
        if(k / 2 == 0 ):
            (u,v) = find_increaser(G, L, edges, (t,w), lambdas, v1, mus, u2)
            if(u == None): 
                edges.add((t,w))
                k += 1
                continue
            else: edges.add((u,v))
        else: 
            (u,v) = find_decreaser(G, L, edges, (t,w), lambdas, v1, mus, u2)
            if(u == None): 
                edges.add((t,w))
                k += 1
                continue
            else: edges.add((u,v))
        k += 1 
    H = nx.empty_graph(len(G))
    print(len(H))
    for edge in edges:
        H.add_edge(edge[0], edge[1])
    #H = nx.from_edgelist(edges)
    return H, {i : i for i in H.nodes}
    