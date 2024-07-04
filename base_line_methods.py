import numpy as np 
import networkx as nx 


## First two are randomizing social networks: a specturm preserving approach 

def simple_addDel(G, k_iter):
    n = len(G)
    G_new = nx.Graph(G)
    edges = [edge for edge in G.edges]
    np.random.shuffle(edges) ## shuffle order 
    for k in range(k_iter):
        (i,j) = edges[k]
        G_new.remove_edge(i,j)
        while(True):
            i_p = np.random.randint(0,n)
            j_p = np.random.randint(0,n)
            if((i_p, j_p) not in G_new.edges): 
                G_new.add_edge(i_p, j_p)
                break ## add line if G is directed 
        edges =  [edge for edge in G_new.edges]
        #np.random.shuffle(edges)
    return G_new  

# def spectrum_addDel(G, epsilon):
#     k = 0
#     lambdas, V = np.linalg.eig(G)
#     l1 = lambda
#     while(j2(k) <= 1 - epsilon):
#         if(k==0 or k==1):

def simple_swap(G, k_iter):
    n = len(G)
    G_new = nx.Graph(G)
    edges = [edge for edge in G.edges]
    np.random.shuffle(edges) ## shuffle order 
    k = 0
    while(k < k_iter):
        (t,w) = edges[k]
        c = 0 
        while(True):
            if(c == k): 
                c += 1 
                continue 
            (u,v) = edges[c]
            if((t,v) not in edges and (u,w) not in edges  ):
                G_new.add_edge(t,v)
                G_new.add_edge(u,w)
                G_new.remove_edge(u,v)
                G_new.remove_edge(t,w)
                edges.append((t,v))
                edges.append((u,w))
                if(c > k):
                    edges.pop(c)
                    edges.pop(k)

                else:
                    edges.pop(k)
                    edges.pop(c)
                np.random.shuffle(edges) ## shuffle order 
                break 
            c += 1 
            if(c > len(edges)): 
                print("Uh oh, couldn't find a valid edge to switch")
                break 
        k += 1
    return G_new

#TODO sprectum preserving methods 



def k_walk(G, u, k):
    return u 

def randomWalk(G, n_hop, k_iters):
    n = len(G)
    G_new = nx.empty_graph(n)
    for u in G.nodes:
        count = 0 
        for v in G.neighbors(u): 
            loop = 0 
            while(loop < k_iters):
                z = k_walk(G,v, n_hop-1)
                if(u == z): break 
                if((u,z) in G_new.eges()): break 
            if(loop < k_iters):
                if(count == 0):
                    G_new.add_edgge(u,z)
                else: 
                    degree = len(list(G.neighbors(u))) 
                    coin_flip = np.random.random() 
                    prob_heads = (0.5*degree - 1)/(degree -1)
                    if(coin_flip < prob_heads):
                        G_new.add_edge(u,z)
            count += 1
    return G_new


