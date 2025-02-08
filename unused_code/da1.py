import numpy as np 
import networkx as nx 

def Anon(G, theta=15):
    n = len(G)
    H = nx.empty_graph(n)
    tix = np.zeros((n))
    degress = [(id, deg) for (id,deg) in G.degree()]
    degrees = sorted(degress, key=lambda x : x[1])
    while(True):
        change = False 
        for v in range(n):
            i = degrees[v][0]
            if( tix[i] <= theta):
                best = []
                score = 9999
                for u in nx.neighbors(G, i):
                    if(tix[u] < theta):
                        if(tix[u] < score): 
                            score = tix[u]
                            best = [u]
                        if(tix[u] == score):
                            best.append(u)
                if(not best): break
                u = best[np.random.randint(0, len(best))]
                H.add_edge(i,u)
                tix[i] += 1 
                tix[u] += 1 
                change = True
        if(not change): break 
    print(max(tix))
    return H 



import numpy as np 
import networkx as nx 
import matplotlib.pyplot as plt
G = nx.fast_gnp_random_graph(1000,0.06)
H = Anon(G,100 )
degrees1 = [G.degree(i) for i in G]
degrees2 = [H.degree(i) for i in H]

print(np.mean(degrees1), np.mean(degrees2))