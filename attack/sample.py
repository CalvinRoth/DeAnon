import numpy as np
import networkx as nx
import attack.distance_vec as dist 

""" Implementation of 
De-anonymizing Social Networks by De-anonymizing Social Networks
by Arvind Narayanan and Vitaly Shmatikov
Most methods are a very direct translation of the paper's pseudo-code into python
"""

""" given a hashmap reverse the key-value pairs"""
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
        if(v != node): continue
        if(u not in mapping): continue
        rnbr = mapping[u]
        for (x,y) in G2.edges():
            if(x != rnbr): continue
            if(y in mapping.items()): continue
            scores[y] += 1/(G2.degree(y)**0.5)
    for (v, u) in G1.edges():
        if(v != node): continue
        if(u not in mapping): continue
        rnbr = mapping[u]
        for (y,x) in G2.edges():
            if(x != rnbr): continue
            if(y in mapping.items()): continue
            scores[x] += 1/(G2.degree(x)**0.5)
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
            change = True
    return mapping, change
    



def deAnon(G1, G2, mapping, k_iters=2000):
    for k in range(k_iters):
        mapping, change = propagationStep(G1, G2, mapping)
        if(not change): 
            break
    return mapping

