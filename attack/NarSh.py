import numpy as np 

def invert(mapping):
    return {mapping[i] : i for i in mapping}

def max2(items):
    return sorted(items)[-2]


def eccentricity(items):
    return (max(items) - max2(items)) / np.std(items)

def matchScores(lgraph, rgraph, mapping, lnode):
    scores = [0 for rnode in rgraph.nodes]
    for (lnbr, lnode) in lgraph.edges:
        if lnbr not in mapping.keys(): continue
        rnbr = mapping[lnbr]
        for (rnbr, rnode) in rgraph.edges:
            if rnode in mapping.items(): continue
            scores[rnode] += 1 / (rgraph.degree(rnode) ** 0.5)
    for (lnode, lnbr) in lgraph.edges:
        if lnbr not in mapping: continue
        rnbr = mapping[lnbr]
        for (rnode, rnbr) in rgraph.edges:
            if rnode in mapping.items(): continue
            scores[rnode] += 1 / (rgraph.degree(rnode) ** 0.5)

    return scores

def propagationStep(lgraph, rgraph, mapping, theta):
    scores = [0 for rnode in rgraph.nodes]
    for lnode in lgraph.nodes:
        pass 
        #scores[lnode] = matchScores(lgraph, rgraph, mapping, lnode)
        #if (eccentricity(scores[lnode]) < theta): continue
        #rnode = (pick node from rgraph.nodes where scores[lnode][node] = max(scores[lnode]))
        #scores[rnode] = matchScores(rgraph, lgraph, invert(mapping), rnode)
        #if eccentricity(scores[rnode]) < theta: continue
        #reverse_match = (pick node from lgraph.nodes where scores[rnode][node] = max(scores[rnode]))
        #if reverse_match != lnode:
        #    continue
    # mapping[lnode] = rnode

