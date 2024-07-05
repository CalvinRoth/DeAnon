#from base_line_methods import * 
from util import * 
import matplotlib.pyplot as plt 
import sys 



def naive_shuffle(G):
    n = len(G)
    mapping = [i for i in range(n)]
    np.random.shuffle(mapping)
    answers = {}
    for i in range(n):
        answers[i] = mapping[i]
    G_new = nx.relabel_nodes(G, answers)
    return G, answers 

anon_algos = {"naive_shuffle" : naive_shuffle}
deanon_algos = []
util_algos = []

n_args = len(sys.argv)
if(n_args < 2):
    print("Usage is\n")
    exit(0)

if(sys.argv[1] == "-a"):
    if(n_args < 5):
        print("Usage: python driver.py -a [algorithm] [input file] [output location]\n")
        exit(0)
    if(sys.argv[2] in anon_algos):
        G = nx.read_edgelist(sys.argv[3])
        G_new, answer = anon_algos[sys.argv[2]](G)
        nx.write_edgelist(G_new, sys.argv[4] + "/anon_graph.txt")
        with open(sys.argv[4] + "/answers.txt", "w" ) as output:
            for key,value in answer.items():
                output.write("%s %s\n" % (key, value))
        # run algorithm
        # outputs should be new graph and answer key 
    
if(sys.argv[2] == "-d"):
    if(n_args < 5):
        print("Specify an algorithm, a file, and an output location \n")
        exit(0)
    if(sys.arg[2] in deanon_algos):
        pass 
        # run algorithm
        # outputs proposed matching 


if(sys.argv[3] == "-u"):
    if(n_args < 5):
        print("Specify an algorithm, a file, and an output location \n")
        exit(0)
    if(sys.arg[2] in util_algos):
        pass 
        # run algorithm
        # outputs should be new graph and answer key 