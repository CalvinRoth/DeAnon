from util import *

def find_spanning_trees(G, n_trees):
    iterator  = nx.algorithms.tree.SpanningTreeIterator(G)
    current = 0
    results = [] 
    for tree in iterator:
        results.append(tree)
        current += 1 
        if(current >= n_trees):
            break 
    return results 

def tree2tree_edit(T1, T2, lands_pub, lands_anon):
    pass 


def tree2tree_score(T1,T2, mapping):
    return nx.graph_edit_distance(T1, T2, timeout=5)

def tree_attack(G1, G2, n_trees, n_seeds):
    #lands_pub = find_landmarks(G1,n_seeds)
    #lands_anon = find_landmarks(G2, n_seeds)
    trees_pub = find_spanning_trees(G1, n_trees)
    trees_anon = find_spanning_trees(G2, n_trees)
    best_map = None 
    best_score = np.Inf
    for i in range(n_trees):
        #mapping = tree2tree_edit(trees_pub[i], trees_anon[i], lands_pub, lands_anon)
        mapping = None 
        score = tree2tree_score(trees_pub[i], trees_anon[i], mapping)
        if(score < best_score):
            best_score = score 
            best_map = mapping
    return best_score, best_map



