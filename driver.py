#from base_line_methods import * 
from util import * 
import matplotlib.pyplot as plt 
import sys 
import numpy as np
from base_line_methods import simple_addDel , simple_swap
import attack.distance_vec



def naive_shuffle(G, args):
    n = len(G)
    new_nodes = [i for i in G.nodes]
    np.random.shuffle(new_nodes)
    node_mapping = dict(zip(G.nodes(), new_nodes))
    G_new = nx.relabel_nodes(G, node_mapping)
    #G_new = nx.empty_graph(n)
    return G_new, node_mapping


def help():
    print("Anonymizatin Algos: ")
    for a in anon_algos:
        print(a, anon_algos[a][1])
    print("###########################################\n")
    print("Deaonymization Algorithms")
    for d in deanon_algos:
        print(d, deanon_algos[d][1])
    print("###########################################\n")
    print("Utility Algorithms")
    for u in util_algos:
        print(u, util_algos[u][1])


anon_algos = {"naive_shuffle" : (naive_shuffle, "None"),
               "addDel" : (simple_addDel, "kChanges"),
               "swap" : (simple_swap, "kChanges")} 
deanon_algos = {"distance-vec":(attack.distance_vec.distance_vector_method, "kSeeds")}
util_algos = {"degree-dist" : (jointDegreeGraph, ""), 
              "path-dist" : (jointShortestPath, ""),
              "accuracy" : (accuracy, "Predicted-graph Answers.txt")}

n_args = len(sys.argv)
print(sys.argv)
if(n_args < 2):
    print("Usage is\n")
    exit(0)

if(sys.argv[1] == "-a"):
    if(n_args < 5):
        print("Usage: python driver.py -a [algorithm] [input file] <args> [output location] \n")
        exit(0)
    if(sys.argv[2] in anon_algos):
        G = nx.read_edgelist(sys.argv[3])
        print("Starting Algorithm: ", anon_algos[sys.argv[2]])
        output_name = sys.argv[-1]
        if(n_args > 5): 
            G_new, answer = anon_algos[sys.argv[2]][0](G, sys.argv[4:])
        else:
            G_new, answer = anon_algos[sys.argv[2]][0](G, [])

        nx.write_edgelist(G_new, output_name + "/anon_graph.txt", data=False)
        with open(output_name + "/answers.txt", "w" ) as output:
            for key,value in answer.items():
                output.write("%s %s\n" % (key, value))
        print("Finished Algorithm")
        # run algorithm
        # outputs should be new graph and answer key 
    
if(sys.argv[1] == "-d"):
    if(n_args < 5):
        print("Specify an algorithm, a file, and an output location \n")
        exit(0)
    if(sys.argv[2] in deanon_algos):
        G = nx.read_edgelist(sys.argv[3])
        H = nx.read_edgelist(sys.argv[4])
        output_name = sys.argv[-1]
        if(n_args > 6): 
            matching = deanon_algos[sys.argv[2]][0](G, H, sys.argv[5:-1])
        else:
            matching = deanon_algos[sys.argv[2]][0](G, [])     
        with open(output_name + "/guess.txt", "w") as output:
            for (u,v) in matching: 
                output.write("%d %d\n" % (u,v))
        print("Finished Algorithm")




if(sys.argv[1] == "-u"):
    if(n_args < 5):
        print("Specify an algorithm, a file, and an output location \n")
        exit(0)
    if(sys.argv[2] in util_algos):
        print(sys.argv[2])
        G = nx.read_edgelist(sys.argv[3])
        G1 = nx.read_edgelist(sys.argv[4])
        util_algos[sys.argv[2]][0](G, G1)

if(sys.argv[1] == "-h"):
    help() 


if(sys.argv[1] == "-c"):
    if(n_args != 4):
        print("specify answer key and guessed pairs")
        exit(0)
    predicted = []
    answers = []
    with open(sys.argv[2], "r") as input1:
        for line in input1.readlines():
            u,v = line.split(" ")
            answers.append((int(u),int(v)))
    with open(sys.argv[3], "r") as input1:
        for line in input1.readlines():
            u,v = line.split(" ")
            predicted.append((int(u),int(v)))
    print("Score of: %f \n" % accuracy(answers, predicted))