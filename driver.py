from base_line_methods import * 
from util import * 
import matplotlib.pyplot as plt 


G = nx.read_edgelist("data/EUmail/email-Eu-core.txt")

def generate_alter(G, method, *args):
    return method(G,*args)


def map_alters(G, anom_method, score_method, argslst):
    n = len(argslst)
    results = np.zeros(len(argslst))
    i = 0 
    for arg in argslst:
        print(arg)
        results[i] = score_method(generate_alter(G, anom_method, arg))
        i += 1 
    return results 

Ks = np.linspace(1,200, 10, dtype=np.int32)



plt.plot(map_alters(G, simple_addDel, clustering, Ks))
plt.plot(map_alters(G, simple_swap, clustering, Ks))
plt.show()