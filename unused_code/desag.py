import numpy as np 
import networkx as nx 
import pygmtools as pygm 
norm = np.linalg.norm


def Delta(G1, mapping):
    nodes = set()

    for v in mapping:
        for neigh in G1.neighbors(v):
            if(neigh not in mapping): nodes.add(neigh)
    return list(sorted(nodes)) 


def inverse(mapping): return {mapping[i] : i for i in mapping}

def s(u, v, G1, G2, mapping,all_shorts_G1, all_shorts_G2,scores):
    struct_s1 = np.array([G1.degree(u), nx.closeness_centrality(G1, u)])
    struct_s2 = np.array([G2.degree(v), nx.closeness_centrality(G2, v)])
    struct_sim = np.dot(struct_s1, struct_s2)/(norm(struct_s1,ord=2) *  norm(struct_s2,ord=2))
    vec1 = all_shorts_G1[u,:] / norm(all_shorts_G1[u,:],ord=2)
    vec2 = all_shorts_G2[v,:] /  norm(all_shorts_G2[u,:],ord=2)
    dist_sim = np.dot(vec1, vec2)
    neigh1 = nx.neighbors(G1, u)
    neigh2 = nx.neighbors(G2,v)
    inhert_sim = 0
    n_pairs = 0
    for x in neigh1:
        for y in neigh2:
            if(x in mapping and y == mapping[x]): 
                inhert_sim +=  scores[x,y]
                n_pairs += 1
    if(n_pairs != 0):
        inhert_sim *= 0.9/n_pairs
        d1 = nx.degree(G1, u)
        d2 = nx.degree(G2, v)
        inhert_sim *= (1 - (np.abs(d1 -d2))/(max(d1, d2)))
    return (1/3) * (struct_sim + dist_sim + inhert_sim)


def deAnon(G1, G2, mapping,theta=0.15, eps=0.1, k_iters=10):
    k = 0 
    flag = True 
    n1 = len(G1)
    n2 = len(G2)
    diam_G1 = nx.diameter(G1)
    diam_G2 = nx.diameter(G2)
    all_shorts_G1 = (1/diam_G1) * nx.floyd_warshall_numpy(G1)
    all_shorts_G2 = (1/diam_G2) * nx.floyd_warshall_numpy(G2)
    scores = np.zeros((n1,n2))
    while(flag):
        neigh1 = Delta(G1, mapping)
        neigh2 = Delta(G2, inverse(mapping))
        s1 = len(neigh1)
        s2 = len(neigh2)
        if(s1==0 or s2==0): break
        bi_GH = np.zeros((s1,s2))
        for i in range(s1):
            for j in range(s2):
                u = neigh1[i]
                v = neigh2[j]
                scores[u,v] =  s(u,v,G1, G2, mapping, all_shorts_G1, all_shorts_G2,scores)
                bi_GH[i,j] = scores[u,v]
        match_raw = pygm.hungarian(bi_GH)
        match_set = []
        for i in range(s1):
            for j in range(s2):
                if(bi_GH[i,j] < theta): continue
                u = neigh1[i]
                v = neigh2[j]
                if(match_raw[i,j] == 1): match_set.append((u,v,bi_GH[i,j])) 
        big_K = int(max(1, np.ceil(eps * len(match_set))))
        top_K_match = sorted(match_set, key = lambda x : x[2], reverse=True)[:big_K]
        if(len(top_K_match) == 0): break 
        for (u,v,_) in top_K_match:
            mapping[u] = v
        k += 1 
        if(k > k_iters): break 
    return mapping



