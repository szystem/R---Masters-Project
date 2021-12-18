#====CONSOLE=====
#how many times algorithm should be run for given K?
initializations=10
C=3

#minimal tested K
K=2


#desired precistion for likelihood ratio
epsilon=0.01

#dataset
x=rbind(rdirichlet(500,c(0.5,4,8)),rdirichlet(200,c(5,1,3)))


#====END OF CONSOLE==== 

#preliminary declarations
library("mc2d")
logddirichlet = function(x,Alpha){
  sum((Alpha-1)*log(x))+lgamma(sum(Alpha))-sum(lgamma(Alpha))
}
maxlogL=-Inf
bestBIC=+Inf
OW=TRUE 
n=nrow(x)

while(OW==TRUE){ #loop for different Ks
  OW=FALSE
  cat("Now testing K=",K,"...\n")
  for(Init in 1:initializations){ #loop for given K
    cat(Init,"...")
    Alpha=matrix(rgamma(K*C,2,0.5),ncol=C)
    Tau=rep(1/K,K)
    # cat(Init,K,Alpha,Beta, "\n", file="starty.txt",append=TRUE)
    SumPi=NA
    Pi=matrix(nrow=n,ncol=K)
    logL=0
    oldlogL=-Inf
    ratio=+Inf
    while(ratio>=epsilon){ #loop for given random initialization
      #E step
      for(i in 1:n){
        for(k in 1:K){
          sum=0
          for(k1 in 1:K){
            sum=sum+Tau[k1]*ddirichlet(x[i,],Alpha[k1,])
          }
          Pi[i,k]=Tau[k]*ddirichlet(x[i,],Alpha[k,])/sum
        }
      }
      ?ddirichlet
      # for(i in 1:n){
      #   for(k in 1:K){
      #     sum=0
      #     for(k1 in 1:K){
      #       if(k1!=k) sum=sum+exp(log(Tau[k1])-log(Tau[k])+logddirichlet(x[i],Alpha[k1])-logddirichlet(x[i],Alpha[k]))
      #     }
      #     Pi[i,k]=1/(1+sum)
      #   }
      # }
      #M step
      Tau=colSums(Pi)/n
      flog=function(e_Alpha,e_x,e_Tau,e_K,e_C,e_n){
        flogL=0
        for(i in 1:e_n){
          psum=0
          for(k in 1:e_K) psum=psum+e_Tau[k]*ddirichlet(e_x[i,],e_Alpha[(e_C*(k-1)+1):(e_C*k)])
          flogL=flogL+log(psum)          
        }
        return(-flogL)
      }
    
      newdata=as.vector(unlist(
        optim(as.vector(t(Alpha)),
              fn=flog,
              e_x=x,e_n=n,e_Tau=Tau,e_K=K,e_C=C,
              method="L-BFGS-B",
              lower=rep(0.1,times=C*K),
              upper=rep(30,times=C*K))[1:2]))
      Alpha=matrix(newdata[1:(K*C)],byrow=T,ncol=C)
      oldlogL=logL
      logL=-newdata[(C*K+1)]
      ratio=abs((logL-oldlogL)/oldlogL)
      cat(ratio)
    }
    
    
    #SETTING THE BEST SET OF SOLUTIONS 
    BIC=-2*logL+((C+1)*K-1)*log(n)
    cat(Init,",",K,",",logL,",",BIC,",",Alpha,",",Tau,"\n"
       # ,file="wyniki.txt",append=TRUE
    )
    if(BIC<bestBIC){
      maxlogL=logL
      bestAlpha=Alpha
      bestTau=Tau
      bestBIC=BIC
      OW=TRUE
      cat("New best solution found!\n")
      cat("The best solution:\n","K=",K,"\n Run=",Init,"\n Tau=",bestTau,"\n Alpha=",bestAlpha,"\n LogL=",maxlogL,"\n BIC=",bestBIC,"\n \n"
          # ,file="best.txt",append=TRUE
          )
    }
    
  }
  K=K+1
}
#output

cat("The best solution:\n","K=",K-2,"\n Tau=",bestTau,"\n Alpha=\n",bestAlpha,"\n LogL=",maxlogL,"\n BIC=",bestBIC)
# 
bestAlpha




