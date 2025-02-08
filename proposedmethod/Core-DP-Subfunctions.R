# © Regents of the University of Minnesota. All rights reserved.  
# Authors: Xuan Bi and Xiaotong Shen, University of Minnesota

library(rmutil)
library(EnvStats)
library(prodlim) 

#cdf of G
G<-function(x,b=1){
  ex=exp(x/b)
  e1=exp(-1/b)
  y=0.5*b*ex*(1-e1)*(x<0)+(x+0.5*b/ex-0.5*b*ex*e1)*(x>=0 & x<=1)+(1-0.5*b/e1/ex*(1-e1))*(x>1)
  return(y)
}

#Univariate DIP
#Input is the to-be-privatized data Y, privacy factor ep, distribution type "empirical", information of the hold-out set X (to create the empirical cdf)
#Output is the privatized \tilde{Y}
#"value" contains information of the hold-out sample
#"value" has to be a list of length 2
#value[[1]] (and value[[2]]) has to be either length 1 or length n
dip<-function(Y,ep,distn,value){
  n=length(Y)
  eta=rlaplace(n,0,1/ep)
  if (distn=="empirical"){
    if (length(value)==2){
      if (value[[1]]=="continuous"){
        FY=pemp(Y,obs = value[[2]]) #FY=pemp(Yrelease,obs = Yholdout)
        Z=FY+eta
        GZ=G(Z,1/ep)
        Yptb=qemp(GZ,obs = value[[2]])
      }else if (value[[1]]=="discrete"){
        U=runif(n,0,1) #Now we only support distance=1 (equal dist)
        V=Y-U
        FV=pemp(V,obs = value[[2]])
        Z=FV+eta
        GZ=G(Z,1/ep)
        Yptb_raw=qemp(GZ,obs = value[[2]])
        Yptb=ceiling(Yptb_raw)
      }else{
        print("value[[1]] for empirical distributions has to be either `continuous` or `discrete`")
      }
    }else{
      print("Parameter vector length has to be 2")
    }
  }else{
    print("The input distribution is not supported")
  }
  return(Yptb)
}



#The subfunction for conditional privatization, namely equation (S7)
## The function is going to priviatize one more column based on the other columns
#zp is the to-be-privatized $Z_p$
#zlp is the privatized $(\tilde{Z}_1,\ldots,\tilde{Z}_{p-1})$
#zlo is the original $(Z_1,\ldots,Z_{p-1})$
#ep is the privacy factor
#X is the hold-out sample
#rho is the percentage of the nearest neighbors (in the holdout set) to be used in the kernel smoothing
#Called in the dip_mv() function below

#This version is based on Tianxi's idea of using a kernel smoothing (5% nearest neighbors) to estimate the conditional cdf
#but uses the original data for F, and the privatized data for F^{-1}

#This version standardizes the first p-1 columns of the original data, the privatized data, and the holdout data
#and also uses the matrix form to avoid for loops
dip_cond <- function(zp,zlp,zlo,ep,X,rho=0.05){

  x.full = as.matrix(cbind(zlo,zp)) # x.full is the original sample (to be privatized)
  n <- nrow(x.full)
  p <- ncol(x.full)

  zlp=as.matrix(zlp) #zlp is the privatized $(\tilde{Z}_1,\ldots,\tilde{Z}_{p-1})$
  zlo=as.matrix(zlo) #zlo is the original $(Z_1,\ldots,Z_{p-1})$

  xo=X[,1:p] # xo is the holdout sample (p columns)

  # Standardization
  #A_standardized <- scale(zlo) #raw data
  #B_standardized <- scale(xo[,1:(p-1)]) #holdout data
  A_standardized <- zlo #raw data
  B_standardized <- as.matrix(xo[,1:(p-1)]) #holdout data
  # Calculate Manhattan distance matrix
  dist_matrix <- as.matrix(dist(rbind(A_standardized, B_standardized), method = "manhattan"))
  # pull the distance between the raw release data zlo and the holdout data xo
  n_A <- nrow(A_standardized)
  n_B <- nrow(B_standardized)
  dst <- dist_matrix[1:n_A, (n_A+1):(n_A+n_B)]
  #For each raw datapoint, find the 5% nearest neighbors in the holdout data
  ind <- t(apply(dst, 1, function(x) order(x)[1:ceiling(n_B*rho)])) 
  
  #For the p-th variable to be privatized, calculate the ecdf based on the 5% nearest neighbors in the holdout data
  xo_ind = matrix(xo[c(ind),p],dim(ind)[1],dim(ind)[2])
  xo_ind_minus_zp = (xo_ind - zp%*%t(rep(1,dim(ind)[2])) <= 0)
  Fx2i = apply(xo_ind_minus_zp, 1, mean) 
  #DIP procedure
  eta <- rlaplace(n_A,0,1/ep)
  z <- Fx2i+eta
  gz =G(z,1/ep)

  #Standardize the privatized data (columns 1 to p-1)
  # C_standardized <- scale(zlp)
  C_standardized <- zlp
  #Calculate the Manhattan distance matrix between the privatized data and the holdout data
  dist_matrix2 <- as.matrix(dist(rbind(C_standardized, B_standardized), method = "manhattan"))
  n_C <- nrow(C_standardized)
  dst2 <- dist_matrix[1:n_C, (n_C+1):(n_C+n_B)]
  #For each privatized datapoint, find the 5% nearest neighbors in the holdout data
  ind2 <- t(apply(dst2, 1, function(x) order(x)[1:ceiling(n_B*rho)]))

  #Among the 5% nearest neighbors, calculate the quantile of gz, which serves as $\hat{F}^{-1}(gz)$
  xo_ind2 = matrix(xo[c(ind2),p],dim(ind2)[1],dim(ind2)[2])
  xp.zip <- mapply(quantile, split(xo_ind2, row(xo_ind2)),gz)
  xt2 <- as.vector(xp.zip)

  return(xt2)
}

#Multivariate DIP
#Input is the to-be-privatized data Z, privacy factor ep, the hold-out set X (to create the empirical cdf)
#Output is the privatized \tilde{Z}
#Accommodating both continuous and discrete variables
dip_mv_new=function(Z,ep,X){
  p=dim(Z)[2]
  s=dim(Z)[1]
  n=dim(X)[1]
  disc_ind=rep(NA,p) #indicator of a discrete variable, not supporting mixed r.v. for now
  for (j in 1:p){
    disc_ind[j]=(length(unique(Z[,j]))<s) #This needs to be changed for mixed r.v.
  }
  p1=sum(disc_ind)
  XC=X
  XC[,disc_ind]=X[,disc_ind]-matrix(runif(n*p1,0,1),n,p1)
  ZC=Z
  ZC[,disc_ind]=Z[,disc_ind]-matrix(runif(s*p1,0,1),s,p1)
  ZCdip=ZC
  value=list("continuous",XC[,1])
  ZCdip[,1]=dip(ZC[,1],ep/p,"empirical",value)
  for (j in 2:p){
    ZCdip[,j]=dip_cond(ZC[,j],ZCdip[,1:(j-1)],ZC[,1:(j-1)],ep/p,XC)
  }
  Zdip=ZCdip
  Zdip[,disc_ind]=ceiling(ZCdip[,disc_ind])
  #Sanity check: For discrete variables, conduct a second ceiling() if their minimal value is outside the support of their counterparts in X
  for (j in 1:p){
    if (disc_ind[j]==1)
      Zdip[,j]=sapply(Zdip[,j], max, min(X[,j]))
  }
  return(Zdip)
}

# DIP.circle.condition & dip_mv_old is the previous version by Tianxi Li?
## The function is going to priviatize one more column based on the other columns
## xo is the holdout sample (p columns)
## xt1 is the ZIP sample (p-1 columns)
## x.full is the original sample (p columns, with the first p-1 columns corresponding to xt1)
DIP.circle.condition <- function(xt1,x.full,xo,ep,rho) {
  n <- nrow(x.full)
  p <- ncol(x.full)
  
  xt1 <- as.matrix(xt1)
  xt2 <- rep(0,n)
  for(i in 1:n){
    x <- xt1[i,]
    xp <- x.full[i,p]
    
    ## Mahattan distance
    dst <- rowSums(abs(t(t(xo[,1:(p-1)])-x)))
    
    ind <- sort(dst,decreasing=FALSE,index.return=TRUE)$ix[1:ceiling(n*rho)]
    
    Fx2i <- mean(xo[ind,p]<= xp)
    
    eta <- rlaplace(1,0,1/ep)
    
    z <- Fx2i+eta
    gz <- G(z,1/ep)
    
    xp.zip <- quantile(xo[ind,p], gz)
    xt2[i] <- xp.zip
    
  }
  return(xt2)
}

#Multivariate DIP
#Input is the to-be-privatized data Z, privacy factor ep, the hold-out set X (to create the empirical cdf)
#Output is the privatized \tilde{Z}
#Accommodating both continuous and discrete variables
dip_mv_old=function(Z,ep,X,rho){
  print(rho)
  p=dim(Z)[2]
  s=dim(Z)[1]
  n=dim(X)[1]
  disc_ind=rep(NA,p) #indicator of a discrete variable, not supporting mixed r.v. for now
  for (j in 1:p){
    disc_ind[j]=(length(unique(Z[,j]))<s) #This needs to be changed for mixed r.v.
  }
  p1=sum(disc_ind)
  XC=X
  XC[,disc_ind]=X[,disc_ind]-matrix(runif(n*p1,0,1),n,p1)
  ZC=Z
  ZC[,disc_ind]=Z[,disc_ind]-matrix(runif(s*p1,0,1),s,p1)
  ZCdip=ZC
  value=list("continuous",XC[,1])
  ZCdip[,1]=dip(ZC[,1],ep/p,"empirical",value)
  for (j in 2:p){
    ZCdip[, j] = DIP.circle.condition(ZCdip[, 1:(j-1)], ZC[, 1:j], XC, ep/p, rho)
  }
  Zdip=ZCdip
  Zdip[,disc_ind]=ceiling(ZCdip[,disc_ind])
  #Sanity check: For discrete variables, conduct a second ceiling() if their minimal value is outside the support of their counterparts in X
  for (j in 1:p){
    if (disc_ind[j]==1)
      Zdip[,j]=sapply(Zdip[,j], max, min(X[,j]))
  } 
  return(Zdip)
}

#Kullback-Leibler Divergence for Bernoulli distributions
#p is a vector of true probabilities; q is a vec of estimated probabilities
KL_Bern<-function(p,q){
  D=mean(p*log(p/q)+(1-p)*log((1-p)/(1-q))) #KL(P||Q)
  return(D)
}

#Generating data from different distributions
datagen<-function(n,distn,value){
  if (distn=="exp"){
    if (length(value)==1){
      Y=rexp(n,value[[1]])
    }else{
      print("Parameter vector length has to be 1")
    }
  }else if (distn=="norm"){
    if (length(value)==2){
      Y=rnorm(n,value[[1]],value[[2]])
    }else{
      print("Parameter vector length has to be 2")
    }
  }else if (distn=="beta"){
    if (length(value)==2){
      Y=rbeta(n,value[[1]],value[[2]])
    }else{
      print("Parameter vector length has to be 2")
    }
  }else if (distn=="unif"){
    if (length(value)==2){
      Y=runif(n,value[[1]],value[[2]])
    }else{
      print("Parameter vector length has to be 2")
    }
  }else if (distn=="binom"){
    if (length(value)==2){
      Y=rbinom(n,value[[1]],value[[2]])
    }else{
      print("Parameter vector length has to be 2")
    }
  }else if (distn=="pois"){
    if (length(value)==1){
      Y=rpois(n,value[[1]])
    }else{
      print("Parameter vector length has to be 1")
    }
  }else if (distn=="geom"){
    if (length(value)==1){
      Y=rgeom(n,value[[1]])
    }else{
      print("Parameter vector length has to be 1")
    }
  }else{
    print("The input distribution is not supported")
  }
  return(Y)
}