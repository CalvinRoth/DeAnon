import networkx as nx 
import numpy as np 
import matplotlib.pyplot as plt 


def to_matrix(D, ln):
    results = np.zeros((ln,ln))
    for i in D: 
        for j in i[1]:
            results[int(i[0]),int(j)] = i[1][j]
    return results 

def clustering(G):
    return nx.transitivity(G)

def subgraph_centrality(G):
    pass 

def harmonic_cetnrality(G):
    return nx.centrality.harmonic_centrality(G)

def lambda1(G):
    vals = nx.linalg.adjacency_spectrum(G)
    return max(vals)


## 2nd largest eigenvalue of laplacian 
def lap2(G):
    vals = nx.linalg.laplacian_spectrum(G)
    return sorted(vals)[1]


def jointDegreeGraph(G1, G2):
    degree_sequence1 = sorted((d for n, d in G1.degree()), reverse=True)
    degree_sequence2 = sorted((d for n, d in G2.degree()), reverse=True)
    dmax = max(max(degree_sequence1), max(degree_sequence2))
    plt.plot(degree_sequence1, label="Degree Dist of G1")
    plt.plot(degree_sequence2, label="Degree Dist of G2")
    plt.legend()
    plt.show()


def jointShortestPath(G1, G2):
    paths1 = nx.all_pairs_shortest_path_length(G1)
    spaths1 = sorted(to_matrix(paths1, len(G1)).flatten(), reverse=True)
    paths2 = nx.all_pairs_shortest_path_length(G2)
    spaths2 =  sorted(to_matrix(paths2, len(G2)).flatten(), reverse=True)
    plt.plot(spaths1, label="Degree Dist of G1")
    plt.plot(spaths2, label="Degree Dist of G2")
    plt.legend()
    plt.show()

def accuracy(answers, guesses):
    n = len(answers)
    m = len(answers)
    answer = sorted(answers, key= lambda x : x[0])
    guesss = sorted(guesses, key= lambda x : x[0])
    answer_idx = [a[0] for a in answer]
    guess_idx = [a[0] for a in guesses]
    total = 0 
    right = 0
    wrong = 0
    for i in range(max(m,n)):
        if(i in answer_idx and i in guess_idx):
            if(answer[i][1] == guesss[i][1]): 
                right += 1 
            else: wrong += 1
        total += 1
    return right/total 
