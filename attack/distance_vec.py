from util import * 


def generate_scores(G_anon, G_pub, lands_anon, lands_pub):
    n1 = len(G_anon)
    n2 = len(G_pub)
    D_anon = nx.all_pairs_shortest_path_length(G_anon)
    D_pub = nx.all_pairs_shortest_path_length(G_pub)
    D_anon = to_matrix(D_anon, n1)
    D_pub = to_matrix(D_pub, n2)

    weights = np.zeros((n1,n2))
    for i in range(n1):
        for j in range(n2):
            score = 0
            for k in range(len(lands_anon)):
                score += (D_anon[i][lands_anon[k]] - D_pub[j][lands_pub[k]])**2
            weights[i,j] = np.sqrt(score) 
    # non_lands_anon = np.array([i for i in range(n1) if i not in lands_anon])
    # non_lands_pub = np.array([i for i in range(n2) if i not in lands_pub])
    # return weights[np.ix_(non_lands_anon, non_lands_pub)]
    return weights 

def find_matching(weights, lands_anon, lands_pub):
    G = nx.to_networkx_graph(weights)
    matching = nx.min_weight_matching(G)
    score = 0 
    for edge in matching:
        score += weights[edge[0], edge[1]]
    return matching, score 

def distance_vector_method(G_anon, G_pub, k):
    best_score = np.Inf 
    best_matching = None 
    best_perm = None 
    lands_anon = find_landmarks(G_anon,k)
    lands_pub = find_landmarks(G_pub,k)
    c = 0
    for perm in itertools.permutations([i for i in range(k)]):
        print(c)
        c += 1
        mapping_anon = [i for i in lands_anon] ## this is always unshuffled 
        mapping_pub = [lands_pub[i] for i in perm]
        W  = generate_scores(G_anon, G_pub, mapping_anon, mapping_pub)
        match, score = find_matching(W, mapping_anon, mapping_pub)
        if(score < best_score):
            best_score = score 
            best_matching = match
            best_perm = perm 
    return best_score, best_matching

def accuracy(matching, answers):
    total = 0 
    right = 0 
    for i in answers:
        if((i, answers[i]) in matching or (answers[i],i)in matching):
            right += 1 
        total += 1
    return right/total



# print(len(G), len(G_relabelled))
# distance_vector_method(G, G_relabelled, 4)

# G1 = nx.fast_gnp_random_graph(100, 0.05)
# G2 = nx.fast_gnp_random_graph(100,0.05)
# k = 4
# lands_anon = find_landmarks(G1,k)
# lands_pub = find_landmarks(G1,k)


# print(distance_vector_method(G1, G2, 4))