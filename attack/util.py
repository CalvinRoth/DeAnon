import numpy as np 
import networkx as nx 
import itertools


def topk_from_dict(scores_dict,k ):
    score_tuples = sorted([ (i, scores_dict[i]) for i in scores_dict], key= lambda x : x[1], reverse=True)
    top_k = [i[0] for i in score_tuples[:k]]
    return top_k

def to_matrix(D, ln):
    results = np.zeros((ln,ln))
    for i in D: 
        for j in i[1]:
            results[int(i[0]),int(j)] = i[1][j]
    return results 

def find_landmarks(G,k):
    centrals = nx.betweenness_centrality(G)
    return topk_from_dict(centrals, k)


def accuracy(matching, answers):
    total = 0 
    right = 0 
    for i in answers:
        if((i, answers[i]) in matching or (answers[i],i)in matching):
            right += 1 
        total += 1
    return right/total