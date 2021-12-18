#====CONSOLE=====
#how many times algorithm should be run for given K?
initializations=4
C=3

#minimal and maximal tested K
K=3
maxK=3

#desired precistion for likelihood ratio
epsilon=0.000001

gauss=function(t,L,m,s) L*exp(-((t-m)/s)^2)+0.0001

fAlpha=function(i,j,k,cc,P.=P){
  Bj=!missing(j)
  Bc=!missing(cc)
  Bk=!missing(k)
  if(Bj){
    tt=R[[i]][j,1]
  }
  if(Bj & Bc & Bk) return(gauss(tt,P.[k,cc,1],P.[k,cc,2],P.[k,cc,3]))
  else if(Bj & !Bc & Bk) return(gauss(tt,P.[k,,1],P.[k,,2],P.[k,,3]))
  else if(Bj & !Bc & !Bk) return(gauss(tt,P.[,,1],P.[,,2],P.[,,3]))
  else if(!Bj & !Bc & Bk){
    tt=R[[i]][,1]
    O=matrix(nrow=J[i],ncol=C)
    for(i in 1:J[i]) O[i,]=gauss(tt[i],P.[k,,1],P.[k,,2],P.[k,,3])
    return(O)
  }
  else 
    return('error')
}

#number of parameters
n_p=3

#boundaries 
setLBandUB=function(){
  LB<<-c(rep(0,times=K*C),rep(-50,times=K*C),rep(10,times=K*C))
  UB<<-c(rep(100,times=K*C),rep(150,times=K*C),rep(100,times=K*C))
}
#==change in optim==

#dataset
X=EXT
X=data.matrix(X)

#choose random P model
#====END OF CONSOLE====

fQq=function(p){
  fp=array(p,dim=c(K,C,n_p))
  sumq=0
  for(i in 1:N)
    for(k in 1:K){
      alpha=fAlpha(i,,k,P.=fp)
      sumq=sumq+PI[i,k]*sum(
        apply((alpha-1)*log(x[[i]])-lgamma(alpha),1,sum)+
          lgamma(apply(alpha,1,sum)))
    }
  return(-sumq)
}

flogL=function(){
  logL=0
  for(i in 1:N){
    sumk=0
    for(k in 1:K){
      prodj=1
      for(j in 1:J[i]) prodj=prodj*ddirichlet(x[[i]][j,],fAlpha(i,j,k))
      sumk=sumk+Tau[k]*prodj
    }
    logL=logL+log(sumk)
  }
  return(logL)
}

j=1
for(i in unique(X[,1])){
  X[X[,1]==i,1]=j
  j=j+1
}

maxlogL=-Inf
bestBIC=+Inf
OW=TRUE 
N=max(X[,1])
n=nrow(X)
R=list(1)
x=list(1)
J=NA

for(i in 1:max(X[,1])){
  R[[i]]=X[X[,1]==i,c(2,3)]
  x[[i]]=X[X[,1]==i,4:ncol(X)]
  J[i]=nrow(matrix(X[X[,1]==i],ncol=ncol(X)))
}
library('mc2d')

while(OW==TRUE){ #loop for different Ks
  OW=FALSE
  cat("\n Now testing K=",K,"...")
  setLBandUB()
  for(Init in 1:initializations){ #loop for given K
    cat("\n", Init,"...")
    # cat(Init, "\n", file="Nov2.txt",append=TRUE)
    P=array(dim=c(K,C,n_p))
    PI=matrix(nrow=N,ncol=K)
    logL=0
    oldlogL=-Inf
    ratio=+Inf
    Tau=rep(1/K,K)
    Bredraw=FALSE
    #----redrawing-------
    while(Bredraw==FALSE){
      Tau=rep(1/K,K)
      P=array(c(rep(10,times=K*C),runif(K*C,0,100),rep(50,times=K*C)),dim=c(K,C,n_p))
      for(i in 1:N){
        for(k in 1:K){
          sum=0
          for(k1 in 1:K){
            sumj=0
            for(j in 1:J[i]){
              sumc=0
              for(c in 1:C) sumc=sumc+(fAlpha(i,j,k1,c)-fAlpha(i,j,k,c))*log(x[[i]][j,c])-lgamma(fAlpha(i,j,k1,c))+lgamma(fAlpha(i,j,k,c))
              sumj=sumj+sumc+lgamma(sum(fAlpha(i,j,k1)))-lgamma(sum(fAlpha(i,j,k)))
            }
            sum=sum+exp(log(Tau[k1])-log(Tau[k])+sumj)
          }
          PI[i,k]=1/sum
        }
      }
      Tau=colSums(PI)/N
      logL=flogL()
      Bredraw=prod(c(Tau>0.1,logL!=-Inf))
      cat('*')
      
    }
    cat(Tau,'\n')
    
    # cat('START P:\n',P[1,,1], "\n",P[2,,1], "\n",P[1,,2], "\n",P[2,,2], "\n",P[1,,3], "\n",P[2,,3], "\n Tau: ", Tau,'Q=', -fQq(P),'logL= ',logL, "\n", file="Nov2.txt",append=TRUE)
    while(ratio>=epsilon){ #loop for given random initialization


      cat(logL,'\n')
      #---------------E step-----------
      for(i in 1:N){
        for(k in 1:K){
          sum=0
          for(k1 in 1:K){
            sumj=0
            for(j in 1:J[i]){
              sumc=0
              for(c in 1:C) sumc=sumc+(fAlpha(i,j,k1,c)-fAlpha(i,j,k,c))*log(x[[i]][j,c])-lgamma(fAlpha(i,j,k1,c))+lgamma(fAlpha(i,j,k,c))
              sumj=sumj+sumc+lgamma(sum(fAlpha(i,j,k1)))-lgamma(sum(fAlpha(i,j,k)))
            }
            sum=sum+exp(J[i]*(log(Tau[k1])-log(Tau[k]))+sumj)
          }
          PI[i,k]=1/sum
        }
      }
      
      # cat(Tau,' , ')
      #-----------M step------------------
      Tau=colSums(PI)/N
      newdata=as.vector(unlist(
        optim(as.vector(P),
              fn=fQq,
              method="L-BFGS-B",
              lower=LB,
              upper=UB
        )[1:2]))

      P=array(newdata[1:(K*C*n_p)],dim=c(K,C,n_p))
      oldlogL=logL
      logL=flogL()
      Q=-newdata[K*C*n_p+1]
      for(i in 1:N)
        for(k in 1:K) Q=Q+PI[i,k]*Tau[k]
      ratio=abs((logL-oldlogL)/oldlogL)
      if(prod(Tau>0.000001)==0) ratio=epsilon/2
      cat('Tau= ',Tau,'\n Q=',Q,' l=',logL,' ',ratio,"\n"
          # ,file="Nov2.txt",append=TRUE
      )
      
    }
    #SETTING THE BEST SET OF SOLUTIONS 
    BIC=-2*logL+(K*C*n_p+K)*log(N)
    # cat('OSTATECZNIE: ',P[1,,1], "\n",P[2,,1], "\n",P[1,,2], "\n",P[2,,2], "\n",P[1,,3], "\n",P[2,,3], "\n", Tau, "\n"
    # ,file="Nov2.txt",append=TRUE
    # )
    if(BIC<bestBIC){
      maxlogL=logL
      bestP=P
      bestTau=Tau
      bestBIC=BIC
      bestPI=PI
      OW=TRUE
      cat("New best solution found!\n")
      # cat("The best solution:\n","K=",K,"\n Run=",Init,"\n Tau=",bestTau,"\n Alpha=",bestAlpha,"\n LogL=",maxlogL,"\n BIC=",bestBIC,"\n \n"
      # ,file="best.txt",append=TRUE
      # )
    }
    
  }
  if(K==maxK) OW=FALSE
  else K=K+1
}
