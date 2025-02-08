library(truncnorm)


defense.dip <- function(A,K,n1,ep=1,rho=0.1){
  n <- nrow(A)
  
  full.estimate <- LSM.PGD(A, K, niter = 100)
  X.tilde <- cbind(full.estimate$alpha, full.estimate$Z)

  X1.hat <- X.tilde[1:n1,]
  X2.hat <- X.tilde[(n1+1):n,]

  results_list <- list()

    # cat(paste("Calling DIP with eps=", ep, "\n", sep=""))
    X1.dip <- dip_mv_new(X1.hat, ep, X2.hat)
    X2.dip <- dip_mv_new(X2.hat, ep, X1.hat)    
    X.dip <- rbind(X1.dip, X2.dip)

    alpha.dip <- X.dip[, 1]
    Z.dip <- X.dip[, -1]

    theta.dip <- alpha.dip %*% t(rep(1, n)) + rep(1, n) %*% t(alpha.dip) + Z.dip %*% t(Z.dip)
    P.dip <- 1 / (1 + exp(-theta.dip))        
    
    A.dip <- gen.A.from.P(P.dip)
    
    results_list <- list(
      X.dip = X.dip, P.dip = P.dip, A.dip = A.dip
    )
  
  return(results_list)
}
    


# defense.dip.trans <- function(A,K,n1,ep=1,rho=0.1){
#   n <- nrow(A)
#   A22 <- A[(n1+1):n,(n1+1):n]
#   A11 <- A[1:n1,1:n1]
  
#   full.estimate <- LSM.PGD(A, K, niter = 100)
#   X.tilde <- cbind(full.estimate$alpha, full.estimate$Z)
  
#   part.estimate1 <- LSM.PGD(A11, K, niter = 100)
#   X1.hat <- cbind(part.estimate1$alpha, part.estimate1$Z)
  
#   part.estimate2 <- LSM.PGD(A22, K, niter = 100)
#   X2.hat <- cbind(part.estimate2$alpha, part.estimate2$Z)

#   Ohat1 <- find.Procrustes(X1.hat[,-1], X.tilde[1:n1,-1])
#   X1.trans <- cbind(X1.hat[, 1], X1.hat[, -1] %*% Ohat1)
  
#   Ohat2 <- find.Procrustes(X2.hat[, -1], X.tilde[(n1+1):n, -1])
#   X2.trans <- cbind(X2.hat[, 1], X2.hat[, -1]%*%Ohat2)

#   results_list <- list()

#     X1.dip <- dip_mv_new(X1.trans, ep, X2.trans)
#     X2.dip <- dip_mv_new(X2.trans, ep, X1.trans)    
#     X.dip <- rbind(X1.dip, X2.dip)

#     alpha.dip <- X.dip[, 1]
#     Z.dip <- X.dip[, -1]

#     theta.dip <- alpha.dip %*% t(rep(1, n)) + rep(1, n) %*% t(alpha.dip) + Z.dip %*% t(Z.dip)
#     P.dip <- 1 / (1 + exp(-theta.dip))        
    
#     A.dip <- gen.A.from.P(P.dip)
    
#     results_list <- list(
#       X.dip = X.dip, P.dip = P.dip, A.dip = A.dip
#     )
  
#   return(results_list)
# }
    

LSM.Gen <- function(n, k, K, avg.d = NULL) {
  alpha <- runif(n, 1, 3)
  alpha <- -alpha / 2# / sum(alpha)
  
  mu <- matrix(runif(K * k, -1, 1), K, k)
  idx <- sample(1:K, n, replace = TRUE)
  mu <- mu[idx, ]
  
  Z <- mu + matrix(rtruncnorm(n * k, -2, 2), n, k)
  J <- diag(n) - rep(1, n)%*%t(rep(1, n))/ n
  Z <- J%*%Z
  G <- Z%*%t(Z)
  Z <- Z / sqrt(sqrt(sum(G ^ 2)) / n)
  
  theta <- alpha%*%t(rep(1, n)) + rep(1, n)%*%t(alpha) + Z%*%t(Z)
  P <- 1 / (1 + exp(-theta))
  
  if(!is.null(avg.d)){
    for (i in 1:10) {
      ratio <- mean(rowSums(P)) / avg.d
      beta <- -log(ratio)
      alpha <- alpha + beta / 2
      theta <- alpha%*%t(rep(1, n)) + rep(1, n)%*%t(alpha) + Z%*%t(Z)
      P <- 1 / (1 + exp(-theta))
    }
  }
  
  upper.index <- which(upper.tri(P))
  upper.p <- P[upper.index]
  upper.u <- runif(length(upper.p))
  upper.A <- rep(0, length(upper.p))
  upper.A[upper.u < upper.p] <- 1
  A <- matrix(0, n, n)
  A[upper.index] <- upper.A
  A <- A + t(A)
  diag(A) <- 0
  return(list(A = A, g = idx, P = P, alpha = alpha, Z = Z))
}

LSM.dip <- function(A,K,n1,eps=c(1),rho=0.1){
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  full.estimate <- LSM.PGD(A, K, niter = 100)
  X.tilde <- cbind(full.estimate$alpha, full.estimate$Z)
  
  part.estimate <- LSM.PGD(A22, K, niter = 100)
  X2.hat <- cbind(part.estimate$alpha, part.estimate$Z)
  
  Ohat <- find.Procrustes(X.tilde[(n1+1):n, -1], X2.hat[, -1])
  X.trans <- cbind(X.tilde[, 1], X.tilde[, -1]%*%Ohat)

  X1.hat <- X.trans[1:n1,]

  results_list <- list()

  for (ep in eps) {
    cat(paste("Calling DIP with eps=", ep, "\n", sep=""))
    X1.dip <- dip_mv_new(X1.hat, ep, X2.hat)
    
    alpha1.hat <- X1.hat[, 1]
    Z1.hat <- X1.hat[, -1]
    alpha1.dip <- X1.dip[, 1]
    Z1.dip <- X1.dip[, -1]
    
    theta1.dip <- alpha1.dip %*% t(rep(1, n1)) + rep(1, n1) %*% t(alpha1.dip) + Z1.dip %*% t(Z1.dip)
    P1.dip <- 1 / (1 + exp(-theta1.dip))
    theta1.hat <- alpha1.hat %*% t(rep(1, n1)) + rep(1, n1) %*% t(alpha1.hat) + Z1.hat %*% t(Z1.hat)
    P1.hat <- 1 / (1 + exp(-theta1.hat))
    
    A1.dip <- gen.A.from.P(P1.dip)
    A1.hat <- gen.A.from.P(P1.hat)
    
    g1 <- graph.adjacency(A11, "undirected")
    g1.dip <- graph.adjacency(A1.dip, "undirected")
    g1.hat <- graph.adjacency(A1.hat, "undirected")
    
    # 保存每个 eps 的结果到列表
    results_list[[paste("eps=", ep, sep="")]] <- list(
      A11 = A11, A11.dip = A1.dip, g1 = g1, g1.dip = g1.dip,
      X1.hat = X1.hat, X1.dip = X1.dip, P1.dip = P1.dip, P1.hat = P1.hat,
      g1.hat = g1.hat, A11.hat = A1.hat
    )
  }
  
  return(results_list)
}

LSM.naive.Laplace <- function(A,K,n1,eps=c(1)) {
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  full.estimate <- LSM.PGD(A, K, niter = 100)
  X.tilde <- cbind(full.estimate$alpha, full.estimate$Z)
  
  X1.hat <- X.tilde[1:n1,]

  results_list <- list()

  for (ep in eps) {
    X1.Lap <- Add.Laplace(X=X1.hat, eps = ep)
    
    alpha1.Lap <- X1.Lap[, 1]
    Z1.Lap <- X1.Lap[, -1]
    theta1.Lap <- alpha1.Lap%*%t(rep(1, n1)) + rep(1, n1)%*%t(alpha1.Lap) + Z1.Lap%*%t(Z1.Lap)
    P1.Lap <- 1 / (1 + exp(-theta1.Lap))
    
    A1.Lap <- gen.A.from.P(P1.Lap)
  
    g1 <- graph.adjacency(A11,"undirected")
    g1.Lap <- graph.adjacency(A1.Lap,"undirected")
    
    # 保存每个 eps 的结果到列表
    results_list[[paste("eps=", ep, sep="")]] <- list(
      A11 = A11, A11.Lap = A1.Lap, g1 = g1, g1.Lap = g1.Lap,
      X1.hat = X1.hat, X1.Lap = X1.Lap, P1.Lap = P1.Lap
    )
  }
  
  return(results_list)
}

LSM.dip1 <- function(A,K,n1,eps=1,rho=0.1){
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  full.estimate <- LSM.PGD(A, K, niter = 100)
  Z.tilde <- full.estimate$Z
  alpha.tilde <- full.estimate$alpha
  
  part.estimate <- LSM.PGD(A22, K, niter = 100)
  Z2.hat <- part.estimate$Z
  alpha2.hat <- part.estimate$alpha
  
  #norm(X.tilde[(n1+1):n,]-X2.hat,"F")/norm(X2.hat,"F")
  
  Ohat <- find.Procrustes(Z.tilde[(n1 + 1):n, ], Z2.hat)
  
  Z.trans <- Z.tilde%*%Ohat
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
  
  #alpha1.hat <- matrix(full.estimate$alpha[1:n1], ncol = 1)
  Z1.hat <- Z.trans[1:n1, ]
  alpha1.hat <- alpha.tilde[1:n1, ]
  cat(paste("Calling DIP with eps=",eps,"\n",sep=""))
  Z1.dip <- dip_mv(Z1.hat, 2 * eps / 3, Z2.hat)#, rho) #先不dip_mv了
  #X1.dip <- X1.hat # 直接用estimate
  
  value <- list("continuous", as.vector(alpha2.hat))
  alpha1.dip <- dip(alpha1.hat, eps / 3, "empirical", value)
  theta1.dip <- alpha1.dip%*%t(rep(1, n1)) + rep(1, n1)%*%t(alpha1.dip) + Z1.dip%*%t(Z1.dip)
  X1.hat <- cbind(alpha1.hat, Z1.hat)
  X1.dip <- cbind(alpha1.dip, Z1.dip)
  P1.dip <- 1 / (1 + exp(-theta1.dip))
  theta1.hat <- alpha1.hat%*%t(rep(1, n1)) + rep(1, n1)%*%t(alpha1.hat) + Z1.hat%*%t(Z1.hat)
  P1.hat <- 1 / (1 + exp(-theta1.hat))
  #summary(as.numeric(P1.dip))
  #summary(as.numeric(X1.hat%*%t(X1.hat)))
  
  #P1.dip[P1.dip<0] <- 0
  #P1.dip[P1.dip>1] <- 1
  
  A1.dip <- gen.A.from.P(P1.dip)
  A1.hat <- gen.A.from.P(P1.hat)
  
  g1 <- graph.adjacency(A11,"undirected")
  g1.dip <- graph.adjacency(A1.dip,"undirected")
  g1.hat <- graph.adjacency(A1.hat, "undirected")
  
  return(list(A11=A11,A11.dip=A1.dip,g1=g1,g1.dip=g1.dip,X1.hat=X1.hat,X1.dip=X1.dip, P1.dip = P1.dip, P1.hat = P1.hat, g1.hat = g1.hat, A11.hat = A1.hat))
}

LSM.Gen.without.alpha <- function(n, k, K, avg.d = NULL) {
  mu <- matrix(runif(K * k, -1, 1), K, k)
  idx <- sample(1:K, n, replace = TRUE)
  mu <- mu[idx, ]
  
  Z <- mu + matrix(rtruncnorm(n * k, -2, 2), n, k)
  J <- diag(n) - rep(1, n)%*%t(rep(1, n))/ n
  Z <- J%*%Z
  G <- Z%*%t(Z)
  Z <- Z / sqrt(sqrt(sum(G ^ 2)) / n)
  
  theta <- Z%*%t(Z)
  P <- 1 / (1 + exp(-theta))
  
  #if(!is.null(avg.d)){
  #  for (i in 1:10) {
  #    ratio <- mean(rowSums(P)) / avg.d
  #    beta <- -log(ratio)
  #    alpha <- alpha + beta / 2
  #    theta <- alpha%*%t(rep(1, n)) + rep(1, n)%*%t(alpha) + Z%*%t(Z)
  #    P <- 1 / (1 + exp(-theta))
  #  }
  #}
  
  upper.index <- which(upper.tri(P))
  upper.p <- P[upper.index]
  upper.u <- runif(length(upper.p))
  upper.A <- rep(0, length(upper.p))
  upper.A[upper.u < upper.p] <- 1
  A <- matrix(0, n, n)
  A[upper.index] <- upper.A
  A <- A + t(A)
  diag(A) <- 0
  return(list(A = A, g = idx, P = P, Z = Z))
}

LSM.PGD.without.alpha <- function (A, k, step.size = 0.3, niter = 500, trace = 0) {
  N <- nrow(A)
  ones = rep(1, N)
  M = matrix(1, N, N)
  Jmat <- diag(rep(1, N)) - M/N
  P.tilde <- USVT(A)
  P.tilde[P.tilde > (1 - 1e-05)] <- (1 - 1e-05)
  P.tilde[P.tilde < 1e-05] <- 1e-05
  Theta.tilde <- logit(P.tilde)
  G <- Jmat %*% Theta.tilde %*% Jmat
  eig <- eigs_sym(A = G, k = k)
  eig$values[eig$values <= 0] <- 0
  Z_0 <- t(t(eig$vectors[, 1:k]) * sqrt(eig$values[1:k]))
  obj <- NULL
  step.size.z <- step.size/norm(Z_0, "2")^2
  for (i in 1:niter) {
    Theta.hat <- Z_0 %*% t(Z_0)
    Phat <- sigmoid(Theta.hat)
    tmp.obj <- (sum(A * log(Phat)) + sum((1 - A) * log(1 - Phat)) - sum(diag(log(1 - Phat))))/2
    if (trace > 0) {
      print(tmp.obj)
    }
    obj <- c(obj, tmp.obj)
    Z <- Z_0 + 2 * step.size.z * (A - Phat) %*% Z_0
    Z <- Jmat %*% Z
    Z_0 <- Z
  }
  Theta.hat <- Z_0 %*% t(Z_0)
  Phat <- sigmoid(Theta.hat)
  tmp.obj <- (sum(A * log(Phat)) + sum((1 - A) * log(1 - Phat)) - 
                sum(diag(log(1 - Phat))))/2
  obj <- c(obj, tmp.obj)
  return(list(Z = Z, Phat = Phat, obj = obj))
}

LSM.dip.without.alpha <- function(A,K,n1,eps=1,rho=0.1){
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  full.estimate <- LSM.PGD.without.alpha(A, K, niter = 100)
  Z.tilde <- full.estimate$Z
  
  part.estimate <- LSM.PGD.without.alpha(A22, K, niter = 100)
  Z2.hat <- part.estimate$Z
  
  #norm(X.tilde[(n1+1):n,]-X2.hat,"F")/norm(X2.hat,"F")
  
  Ohat <- find.Procrustes(Z.tilde[(n1 + 1):n, ], Z2.hat)
  
  Z.trans <- Z.tilde%*%Ohat
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
  
  #alpha1.hat <- matrix(full.estimate$alpha[1:n1], ncol = 1)
  Z1.hat <- Z.trans[1:n1, ]
  cat(paste("Calling DIP with eps=",eps,"\n",sep=""))
  Z1.dip <- dip_mv(Z1.hat, eps, Z2.hat)#, rho) #先不dip_mv了
  #X1.dip <- X1.hat # 直接用estimate
  
  theta1.dip <- Z1.dip%*%t(Z1.dip)
  P1.dip <- 1 / (1 + exp(-theta1.dip))
  theta1.hat <- Z1.hat%*%t(Z1.hat)
  P1.hat <- 1 / (1 + exp(-theta1.hat))
  #summary(as.numeric(P1.dip))
  #summary(as.numeric(X1.hat%*%t(X1.hat)))
  
  #P1.dip[P1.dip<0] <- 0
  #P1.dip[P1.dip>1] <- 1
  
  A1.dip <- gen.A.from.P(P1.dip)
  A1.hat <- gen.A.from.P(P1.hat)
  
  g1 <- graph.adjacency(A11,"undirected")
  g1.dip <- graph.adjacency(A1.dip,"undirected")
  g1.hat <- graph.adjacency(A1.hat, "undirected")
  
  return(list(A11=A11,A11.dip=A1.dip,g1=g1,g1.dip=g1.dip,Z1.hat=Z1.hat,Z1.dip=Z1.dip, P1.dip = P1.dip, P1.hat = P1.hat, g1.hat = g1.hat, A11.hat = A1.hat))
}

LSM.naive.Laplace.without.alpha <- function(A,K,n1,eps=1){
  n <- nrow(A)
  A22 <- A[(n1+1):n,(n1+1):n]
  A11 <- A[1:n1,1:n1]
  
  full.estimate <- LSM.PGD.without.alpha(A, K, niter = 100)
  Z.tilde <- full.estimate$Z
  
  #alpha1.hat <- matrix(full.estimate$alpha[1:n1], ncol = 1)
  Z1.hat <- Z.tilde[1:n1,]
  Z1.Lap <- Add.Laplace(X=Z1.hat, eps = eps) #先不Add.Laplace了
  #X1.Lap <- X1.hat # 直接用estimate
  
  theta1.Lap <- Z1.Lap%*%t(Z1.Lap)
  P1.Lap <- 1 / (1 + exp(-theta1.Lap))
  
  #P1.Lap[P1.Lap<0] <- 0
  #P1.Lap[P1.Lap>1] <- 1
  
  A1.Lap <- gen.A.from.P(P1.Lap)
  
  g1 <- graph.adjacency(A11,"undirected")
  g1.Lap <- graph.adjacency(A1.Lap,"undirected")
  
  return(list(A11=A11,A11.Lap=A1.Lap,g1=g1,g1.Lap=g1.Lap,Z1.hat=Z1.hat,Z1.Lap=Z1.Lap, P1.Lap = P1.Lap))
}