library(rmutil)
library(EnvStats) 
library(prodlim) 
library(xtable) 
library(diffpriv)
library(randnet)
library(RSpectra)
library(irlba)
library(HCD)
library(igraph)

source("Core-DP-Subfunctions.R")
f <- function(X) X ## target function



ase <- function(A,K){
  A2 <- A^T%*%A
  eig <- eigs_sym(A2,k=K)
  Xhat <- t(t(eig$vectors[,1:K, drop = FALSE])*eig$values[1:K]^(1/4))
  return(Xhat)
}

find.Procrustes <- function(A,B){
  p <- ncol(A)
  SVD <- svd(t(A)%*%B,nv=p)
  Omat <- SVD$u[,1:p, drop = FALSE]%*%t(SVD$v[,1:p, drop = FALSE])
  return(Omat)
}


net2mom <- function(A){
  k <- nrow(A)
  diag(A) <- 0
  
  A2 <- A %*% A
  diag(A2) <- 0
  
  Uv <- sum(A2)/(k*(k-1)*(k-2)/3)
  
  Xv_exact <- sum(cal_k_star(A,2))
  
  Uv_exact <- (Xv_exact/(k*(k-1)*(k-2)/6))
  
  Utri <- sum(diag(A %*% A %*% A)) / (k*(k-1)*(k-2))
  
  vv <- colSums(A)
  vv2 <- colSums(A^2)
  vv3 <- colSums(A^3)
  
  Utstar <- sum(vv^3 - 3*vv2*vv + 2*vv3) /6 / (k*(k-1)*(k-2)*(k-3)/24)
  Xstar_exact <- sum(cal_k_star(A,3))
  Utstar_exact <-  ( Xstar_exact/ (k*(k-1)*(k-2)*(k-3)/24))
  
  
  
  
  return(list(UR = c(Uv,Utri,Utstar), UR_exact = c(Uv_exact,  Utri, Utstar_exact)))
}

get.stats <- function(g){
  n <- length(V(g))
  
  edge.count <- length(E(g))
  
  tri <- sum(count_triangles(g))/3
  
  over.count <- sum((degree(g) * (degree(g)-1)/2))
  
  sup.v.count <- over.count - tri*2
  
  stats <- c(edge.count/choose(n=n,k=2),sup.v.count/choose(n=n,k=3),tri/choose(n=n,k=3))
  
  rho.n <- edge.count/choose(n=n,k=2)
  
  stats <- stats/c(rho.n^1,rho.n^2,rho.n^3)
  
  return(stats)
  
}


RDPG.dip <- function(A,K,n1,eps=1,rho=0.1){
  print("This function has been used.")
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  
  X.tilde <- ase(A,K)
  
  X2.hat <- ase(A22,K)
  
  #norm(X.tilde[(n1+1):n,]-X2.hat,"F")/norm(X2.hat,"F")
  
  Ohat <- find.Procrustes(X.tilde[(n1+1):n,],X2.hat)
  
  X.trans <- X.tilde%*%Ohat
  #norm(X.trans[(n1+1):n,]-X2.hat,"F")/norm(X2.hat,"F")
  
  # head(X.trans[(n1+1):n,])
  # head(X.true[(n1+1):n,])
  # 
  # Ohat2 <- find.Procrustes(X2.hat,X.true[(n1+1):n,])
  # X2.trans <- X2.hat%*%Ohat2
  # norm(X.true[(n1+1):n,]-X2.hat,"F")/norm(X.true[(n1+1):n,],"F")
  # norm(X.true[(n1+1):n,]-X2.trans,"F")/norm(X.true[(n1+1):n,],"F")
  # 
  # Ohat1 <- find.Procrustes(X.tilde[(n1+1):n,],X.true[(n1+1):n,])
  # X.trans1 <- X.tilde%*%Ohat1
  # norm(X.true[(n1+1):n,]-X.tilde[(n1+1):n,],"F")/norm(X.true[(n1+1):n,],"F")
  # norm(X.true[(n1+1):n,]-X.trans1[(n1+1):n,],"F")/norm(X.true[(n1+1):n,],"F")
  
  X1.hat <- X.trans[1:n1,]
  cat(paste("Calling DIP with eps=",eps,"\n",sep=""))
  X1.dip <- dip_mv(X1.hat,eps,X2.hat)#, rho) #先不dip_mv了
  #X1.dip <- X1.hat # 直接用estimate
  
  P1.dip <- X1.dip%*%t(X1.dip)
  #summary(as.numeric(P1.dip))
  #summary(as.numeric(X1.hat%*%t(X1.hat)))
  
  P1.dip[P1.dip<0] <- 0
  P1.dip[P1.dip>1] <- 1
  
  A1.dip <- gen.A.from.P(P1.dip)
  
  g1 <- graph.adjacency(A11,"undirected")
  g1.dip <- graph.adjacency(A1.dip,"undirected")
  
  return(list(A11=A11,A11.dip=A1.dip,g1=g1,g1.dip=g1.dip,X1.hat=X1.hat,X1.dip=X1.dip, P1.dip = P1.dip))
}




RDPG.naive.Laplace <- function(A,K,n1,eps=1){
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  
  X.tilde <- ase(A,K)
  
  X1.hat <- X.tilde[1:n1,]
  X1.Lap <- Add.Laplace(X=X1.hat, eps = eps) #先不Add.Laplace了
  #X1.Lap <- X1.hat # 直接用estimate
  
  P1.Lap <- X1.Lap%*%t(X1.Lap)

  P1.Lap[P1.Lap<0] <- 0
  P1.Lap[P1.Lap>1] <- 1
  
  A1.Lap <- gen.A.from.P(P1.Lap)
  
  g1 <- graph.adjacency(A11,"undirected")
  g1.Lap <- graph.adjacency(A1.Lap,"undirected")
  
  return(list(A11=A11,A11.Lap=A1.Lap,g1=g1,g1.Lap=g1.Lap,X1.hat=X1.hat,X1.Lap=X1.Lap))
}



Add.Laplace <- function(X,eps=1){
  p <- ncol(X)
  n <- nrow(X)
  pparams <- DPParamsEps(epsilon = eps/p) ## desired privacy budget
  print(pparams)
  X_lap=X
  for (j in 1:p){ #Post processing; truncating unreasonably large values
    mechanism <- DPMechLaplace(target = f, sensitivity = max(abs(X[,j])), dims = n)
    r_lap <- releaseResponse(mechanism, privacyParams = pparams, X = X[,j])
    X_lap[,j]=r_lap$response
  }
  return(X_lap)
}

