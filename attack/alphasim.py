import numpy as np
import networkx as nx 
import pygmtools as pygm

pygm.set_backend('numpy')
np.random.seed(0)

def findMatching(G1, G2, scores):
    n1 = len(G1)
    n2 = len(G2)
    A = np.zeros((n1+n2, n1+n2))
    G1_nodes = [v for v in G1.nodes]
    G2_nodes = [v for v in G2.nodes]
    for i in range(n1):
        for j in range(n2):
            A[i,j] = scores[G1_nodes[i], G2_nodes[j]]
    M = pygm.hungarian(A)    
    score = 0
    for i in range(n1):
        for j in range(n2):
            if(M[i,j] == 1):
                score += A[i,j]
                break 
    return score 

def initialize_scores(G1, G2, nRounds, beta, gamma):
    n1 = len(G1)
    n2 = len(G2)
    V1 = {i for i in range(n1)}
    V2 = {i for i in range(n2)}
    E1 = G1.edges()
    E2 = G2.edges()
    # Step 1: Initialize the score matrix to all ones
    score = {(u, v): 1 for u in V1 for v in V2}

    for _ in range(nRounds):
        score_prime = {}
        for u in V1:
            for v in V2:
                top = findMatching(G1.subgraph(G1.neighbors(u)),G2.subgraph(G2.neighbors(v)), score)
                bot = max(G1.degree(u), G2.degree(v))
                r = top/bot 
                score_prime[(u, v)] = (1 - beta) * r + beta  # Step 5
        score = score_prime  # Step 7
    return score  # Step 9


def compute_similarity_scores(G1, G2, nRounds, alpha, beta, gamma):
    n1 = len(G1)
    n2 = len(G2)
    V1 = {i for i in range(n1)}
    V2 = {i for i in range(n2)}
    E1 = G1.edges()
    E2 = G2.edges()
    # Step 1: Initialize similarity scores
    score = initialize_scores(V1, V2)
    for _ in range(nRounds):
        score_prime = {}
        for u in V1:
            top_u = max(score.get((u, v), 0) for v in V2)  # Step 5: highest score related to u
            theta = alpha * top_u  # Step 6
            for v in V2:
                if (u,v) not in score: continue
                if score[(u, v)] >= theta:  # Step 8
                    top = findMatching(G1.subgraph(G1.neighbors(u)),G2.subgraph(G2.neighbors(v)), score)
                    bot = max(G1.degree(u), G2.degree(v))
                    r = top/bot 
                    score_prime[(u, v)] = (1 - beta) * r + beta  # Step 10
        score = score_prime  # Step 14
    return score  # Step 16




def node_mapping(G1, G2, score, r):
    n1 = len(G1)
    n2 = len(G2)
    V1 = {i for i in range(n1)}
    V2 = {i for i in range(n2)}
    E1 = G1.edges()
    E2 = G2.edges()
    nodeMapping = {}
    unmatched_pairs = {(u, v) for u in V1 for v in V2}
    
    while unmatched_pairs:
        A = set()
        # Fill candidate set A with unmatched pairs with score >= r
        while not A:
            # Match the pair (u, v) with the highest score
            best_pair = max(unmatched_pairs, key=lambda pair: score(pair[0], pair[1]), default=None)
            if best_pair is None:
                break
            u, v = best_pair
            nodeMapping[(u, v)] = score(u, v)
            unmatched_pairs.remove(best_pair)
            
            # Increase the score of neighboring pairs
            for neighbor_u in G1.neighbors(u):
                for neighbor_v in G2.neighbors(v):
                    if (neighbor_u, neighbor_v) in unmatched_pairs:
                        score[neighbor_u, neighbor_v] += score(u, v)
            
            A = {pair for pair in unmatched_pairs if score(pair[0], pair[1]) >= r}

        if A:
            # Pick a pair (u, v) with the highest score in A
            best_pair = max(A, key=lambda pair: score(pair[0], pair[1]))
            u, v = best_pair
            nodeMapping[(u, v)] = score(u, v)
            unmatched_pairs.remove(best_pair)
            
            # Increase the score of neighboring pairs
            for neighbor_u in G1.neighbors(u):
                for neighbor_v in G2.neighbors(v):
                    if (neighbor_u, neighbor_v) in unmatched_pairs:
                        score[neighbor_u, neighbor_v] += score(u, v)
                        pass

    return nodeMapping


