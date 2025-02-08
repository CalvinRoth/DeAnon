# DeAnon

This code based has a few central parts to discuss: attacks, defenses, proposed method, and . 

## Attack 
    Attacks are in the folder attack and there are 3 attacks. 
    Alpha: is code that I have only modified, . It is a command line code that expects two files ../data/crawled.txt and  ../data/anonymized.txt
    to do the matching with. These files should both be 1 indexed and be a list of edges where each line(but the first) is of the form "node1 node2". The first 
    line should be the number of nodes number of edges.  If you wish to reset it back to before I modified it that can be found here: https://github.com/shuyang790/deanonymization

   The other two attacking methods take some number of problems but both at least the two networks in a function called DeAnon that attempts to match nodes in one graph with the other. 

## Defenses 
    KAnon is a defense not written by me but takes in a graph and a parameter k and returns a new network. 
    rw short for random walk and spectral add both take a network and parameters to a function called Anon. 

    Proposed method is the code written by Xuan Bi and Tianxi Li. It is written in R 

## Unused code 
    This is some leftover bits of code that are not currently being used. 

## Main_results 
    This is a notebook that puts all results together and applies them to a dataset of eu emails.