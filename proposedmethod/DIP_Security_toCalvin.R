
rm(list=ls())
source("GraphDIP-Source.R")
source("lsm1.R")
library(doParallel)
library(randnet)
library(igraph)


n=1000 #number of nodes
n1=0.5*n #number of nodes for a subnetwork for two-fold cross protection
lambda=25 #average degrees
for (k in 1:5) {

    # Simulate a network
    model <- LSM.Gen(n = n, k = k, K = 1, avg.d = lambda)
    A <- matrix(as.numeric(readMM("mmEu.txt")), ncol=1005, nrow=1005)
    #P <- model$P
    #A <- model$A
    # The proposed method
    result.DIP <- defense.dip(A = A, K = k, n1 = n1)
    #A.dip <- Matrix(result.DIP$A.dip, sparse = TRUE)
    writeMM(result.DIP$A.dip,file=paste('result_', as.character(k), '.txt', sep=""))
}