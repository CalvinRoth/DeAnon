import pygmtools as pygm
import numpy as np 
import networkx as nx 
import itertools


pygm.set_backend('numpy')
np.random.seed(0)


def to_matrix(D, ln):
    results = np.zeros((ln,ln))
    for i in D: 
        for j in i[1]:
            results[int(i[0]),int(j)] = i[1][j]
    return results 


def topk_from_dict(scores_dict,k ):
    score_tuples = sorted([ (i, scores_dict[i]) for i in scores_dict], key= lambda x : x[1], reverse=True)
    top_k = [i[0] for i in score_tuples[:k]]
    return top_k


def find_landmarks(G,k):
    centrals = nx.betweenness_centrality(G)
    return topk_from_dict(centrals, k)


def to_matrix(D, ln):
    results = np.zeros((ln,ln))
    for i in D: 
        for j in i[1]:
            results[int(i[0]),int(j)] = i[1][j]
    return results 

## this omits landmarks
def generate_scores(G_anon, G_pub, lands_anon, lands_pub):
    n1 = len(G_anon)
    n2 = len(G_pub)
    D_anon = nx.all_pairs_shortest_path_length(G_anon)
    D_pub = nx.all_pairs_shortest_path_length(G_pub)
    n_nonlands1 = n1-len(lands_anon)
    n_nonlands2 = n2-len(lands_pub)
    D_anon = to_matrix(D_anon, n1)
    D_pub = to_matrix(D_pub, n2)
    weights = np.zeros((n1, n2))
    for i in range(n1):
        if(i in lands_anon): 
            continue
        for j in range(n2):
            if(j in lands_pub): 
                continue
            score = 0
            for k in range(len(lands_anon)):
                d1 = D_anon[i][int(lands_anon[k])]
                d2 = D_pub[j][int(lands_pub[k])]
                score += (d1 - d2)**2
            weights[i,j] = -np.sqrt(score) 
    for i in range(len(lands_anon)):
        weights[lands_anon[i], :] = -99999
        weights[:, lands_pub[i]] = -99999
        weights[lands_anon[i], lands_pub[i]] = 1 
    return weights

def find_matching(weights, lands_anon, lands_pub):
    matching = pygm.hungarian(weights)
    score = 0 
    for edge in matching:
        a = int(edge[0])
        b = int(edge[1])
        score += weights[a,b]
    return matching, score 



def distance_vector_method(G_anon, G_pub, args):
    if(len(args) == 1 ):
        k = int(args[0])
        k_iters = 1
    elif(len(args) == 2):
        k = int(args[0])
        k_iters = int(args[1])
    else:
        print("Specify k landmarks to find")
    best_score = np.Inf 
    best_matching = None 
    best_perm = None 
    lands_anon = find_landmarks(G_anon,k)
    lands_pub = find_landmarks(G_pub,k)
    c = 0
    for perm in itertools.permutations([i for i in range(k)]):
        c += 1
        mapping_anon = [i for i in lands_anon] ## this is always unshuffled 
        mapping_pub = [lands_pub[i] for i in perm]
        W  = generate_scores(G_anon, G_pub, mapping_anon, mapping_pub)
        match, score = find_matching(W, mapping_anon, mapping_pub)
        if(score < best_score):
            best_score = score 
            best_matching = match
            best_perm = perm 
        if(c >= k_iters): break 
    matching = [(i, np.argmax(best_matching[i,:])) for i in range(len(best_matching))]
    return matching



# G = nx.read_edgelist("data/email-Eu-core.txt", nodetype=int)
# H = nx.read_edgelist("results/anon_graph.txt", nodetype=int)
# distance_vector_method(G, H, 40)
