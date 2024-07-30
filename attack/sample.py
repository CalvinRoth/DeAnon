import numpy as np
import networkx as nx
import distance_vec as dist 

def invert(mapping):
    return {mapping[i] : i for i in mapping}

def eccentricity(scores):
    snd = sorted(scores)[-2]
    frst = np.max(scores)
    return (frst - snd)/np.std(scores)

def MatchScore(G1, G2, mapping, node):
    n1 = len(G1)
    n2 = len(G2)
    scores = np.zeros((n2))
    for (u,v) in G1.edges():
        if(v != node): continue ## write fasters
        if(u not in mapping): continue
        rnbr = mapping[u]
        for (x,y) in G2.edges():
            if(x != rnbr): continue
            if(y in mapping.items()): continue
            scores[x] += 1/(G2.degree(y)**0.5)
    for (v, u) in G1.edges():
        if(v != node): continue
        if(u not in mapping): continue
        rnbr = mapping[u]
        for (y,x) in G2.edges():
            if(x != rnbr): continue
            if(y in mapping.items()): continue
            scores[x] += 1/(G2.degree(y)**0.5)
    return scores 



def propagationStep(G1, G2, mapping, theta=0.15):
    n1 = len(G1)
    n2 = len(G2)
    change = False 
    for i in range(n1):
        if( i in mapping): continue
        scores = MatchScore(G1, G2, mapping, i)
        #if eccentricity(scores) < theta: continue
        rnode = np.argmax(scores)
        score = MatchScore(G2, G1, invert(mapping), rnode)
        reverse_match = np.argmax(score)
        if(reverse_match == i): 
            mapping[i] = rnode 
            print(i, rnode)
            change = True
    return mapping, change
    



def deAnon(G1, G2, mapping, k_iters=20):
    for k in range(k_iters):
        mapping, change = propagationStep(G1, G2, mapping)
        if(not change): 
            print(k)
            break
    return mapping



# G1 = nx.read_adjlist("../../data/local_data.txt", nodetype=int)
# G2 = nx.read_adjlist("../../data/social_network.txt", nodetype=int)
G1 = nx.read_edgelist("../data/email-Eu-core.txt", nodetype=int)
G2 = nx.read_edgelist("../results/anon_graph.txt", nodetype=int)

mapping = {i : i for i in range(15)}
print(mapping)
print("###########################")
mapping = propagationStep(G1, G2, mapping,10000)
print(mapping)
