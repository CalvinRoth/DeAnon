### Not Written by Calvin Roth see original source below: 
### Source https://github.com/ksinghs/Identity-anonymization-on-graphs?tab=readme-ov-file 
import pandas as pd
import networkx as nx
import numpy as np
import random as rn

# Graph anonymization 
def graph_anonymization(orignal_Graph,k_degree):

  dv = [ d[1] for d in orignal_Graph.degree()]
  degree_sequence = np.sort(dv)[::-1]

  # step1: degree anonymizer using dynamic programming
  anonymised_sequence = degree_anonymization(degree_sequence,k_degree) 
  # step2: Construct graph using ConstructGraph algorithm
  Ga = construct_graph(anonymised_sequence,orignal_Graph)

  return Ga

# step1 : Degree Anonymization using Dynamic programming 
def degree_anonymization(degree_sequence,k_degree):

  C = anonymisation_cost(degree_sequence,k_degree)
  n = np.size(degree_sequence)
  Da = np.full(n,np.inf)
  sequences = [None] * n
  cost, anonymised_sequence = degree_anonymization_recursion(degree_sequence,k_degree,C,n,Da,sequences)

  return anonymised_sequence

def anonymisation_cost(degree_sequence,k):
    n = np.size(degree_sequence)
    C = np.full([n,n],np.inf)
    for i in range(n-1):
        for j in range(i+k-1,np.min([i+2*k,n])):
          if C[i,j-1] == np.inf:
            C[i,j] = assignment_cost_addition(degree_sequence[i:j+1])           
          else:
            C[i,j] = C[i,j-1] + degree_sequence[i] - degree_sequence[j]
    return C

def assignment_cost_addition(degree_sequence):
  return np.sum(degree_sequence[0]-degree_sequence)


def degree_anonymization_recursion(degree_sequence,k,C,n,Da,sequences):
  group_degree = degree_sequence[0]
  
  all_group_sequence = np.full(n,group_degree)
  all_group_cost = C[0,n-1]  
      
  if n < 2*k:
      return all_group_cost, all_group_sequence
  else:
    min_cost = np.inf
    min_cost_sequence = np.empty(0)
    
    for t in range(np.max([k-1,n-2*k]),n-k):
      
      if Da[t] == np.inf:
        cost, sequence = degree_anonymization_recursion(degree_sequence[0:t+1],k,C,t+1,Da,sequences)
        Da[t] = cost
        sequences[t] = sequence
      else:
        cost = Da[t]
        sequence = sequences[t]
        cost = cost + C[t+1,n-1]
      
      if cost < min_cost:
        min_cost = cost
        min_cost_sequence = np.concatenate((sequence,np.full(np.size(degree_sequence[t+1:]),degree_sequence[t+1])))                
    min_cost_squence_return = (min_cost, min_cost_sequence) if min_cost < all_group_cost else (all_group_cost, all_group_sequence)
  return min_cost_squence_return


#Step2: Graph construction 
def construct_graph(degree_sequence, original_graph):
  
  n = len(degree_sequence)
  if np.sum(degree_sequence) % 2 != 0:
    return None
            
  G = nx.empty_graph(n)
  vd = [(v,d) for v,d in enumerate(degree_sequence)]

  while True:
    
    vd.sort(key=lambda tup: tup[1], reverse=True)
    if vd[-1][1] < 0:
      return None
    
    tot_degree = 0
    for vertex in vd:
      tot_degree = tot_degree + vertex[1]
      
    if tot_degree == 0:
      return G
        
    remaining_vertices = [i for i,vertex in enumerate(vd) if vertex[1] > 0]
    idx = remaining_vertices[rn.randrange(len(remaining_vertices))]
    v = vd[idx][0]
  
    for i,u in enumerate(vd):
      
      if vd[idx][1] == 0:
        break
        
      if u[0] == v:
        continue
            
      if G.has_edge(u[0],v):
        continue
            
      if original_graph.has_edge(v,u[0]) and u[1] > 0:
        G.add_edge(v,u[0])     
        vd[i] = (u[0],u[1] - 1)      
        vd[idx] = (v,vd[idx][1] - 1)
                
        
    for i,u in enumerate(vd):
      
      if vd[idx][1] == 0:
        break
          
      if u[0] == v:
        continue
            
      if G.has_edge(v,u[0]):
        continue
            
      if not original_graph.has_edge(v,u[0]):
        G.add_edge(v,u[0])
        vd[i] = (u[0],u[1] - 1)
        vd[idx] = (v,vd[idx][1] - 1)
