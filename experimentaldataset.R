#=============TRIVIAL DATASET=======================
EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:200)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(-10),logis(-5),logis(2)))))
        c=c+1
      }
for(i in 1:100)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+200,A,S,t,rdirichlet(1,c(logis(0),logis(5),logis(-8)))))
        c=c+1
      }
for(i in 1:150)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+300,A,S,t,rdirichlet(1,c(logis(10),logis(6),logis(6)))))
        c=c+1
      }
EXT=EXT[-1,]
tail(EXT)
tail(PI)

fAlpha=function(i,j,k,cc,p=P,r=R,cJ=J,cC=C){
  Bj=!missing(j)
  Bc=!missing(cc)
  Bk=!missing(k)
  if(Bj){
    A=r[[i]][j,1]
    S=r[[i]][j,2]
    t=r[[i]][j,3]
  }
  if(Bj & Bc & Bk) return(logis(p[k,cc,1]))
  else if(Bj & !Bc & Bk) return(logis(p[k,,1]))
  else if(Bj & !Bc & !Bk) return(logis(p[,,1]))
  else if(!Bj & !Bc & Bk){
    A=r[[i]][,1]
    S=r[[i]][,2]
    t=r[[i]][,3]
    return(logis(rep(1,times=J[i])%*%t(p[k,,1])))
  }
  else if(!Bj & !Bc & !Bk){
    A=r[[i]][,1]
    S=r[[i]][,2]
    t=r[[i]][,3]
    dim=c(K,cC,cJ[i])
    O=array(dim=dim)
    for(j in 1:cJ[i]) O[,,j]=logis(p[,,1])
    return(O)
  }
}

#============ONE PARAMETER===================
#only Area
EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:200)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(-5-3*A),logis(-3-7*A),logis(A)))))
        c=c+1
      }
for(i in 1:100)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+200,A,S,t,rdirichlet(1,c(logis(0+5*A),logis(5-3*A),logis(-8+3*A)))))
        c=c+1
      }
for(i in 1:150)
  for(t in 10*(1:1))
    for(A in 0:1)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+300,A,S,t,rdirichlet(1,c(logis(10+3*A),logis(6+3*A),logis(6+3*A)))))
        c=c+1
      }
EXT=EXT[-1,]

# A=0
# c(logis(-5-3*A),logis(-3-7*A),logis(A))
# c(logis(0+5*A),logis(5-3*A),logis(-8+3*A))
# c(logis(10+3*A),logis(6+3*A),logis(6+3*A))
# fAlpha(1,1)
# 
# A=1
# fAlpha(1,2)
# c(logis(-5-3*A),logis(-3-7*A),logis(A))
# c(logis(0+5*A),logis(5-3*A),logis(-8+3*A))
# c(logis(10+3*A),logis(6+3*A),logis(6+3*A))

#===================TIME PARAMETER===============
logis(10+20)
EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:40)
  for(t in 10*(1:10))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(10+0.2*t),logis(20-0.2*t),logis(5)))))
        c=c+1
      }
for(i in 1:20)
  for(t in 10*(1:10))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+40,A,S,t,rdirichlet(1,c(logis(0+0.15*t),logis(5+0.5*t),logis(30-0.2*t)))))
        c=c+1
      }
for(i in 1:30)
  for(t in 10*(1:10))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+60,A,S,t,rdirichlet(1,c(logis(20),logis(-5-0.2*t),logis(+0.2*t)))))
        c=c+1
      }
EXT=EXT[-1,]


#===================non parametric clusters===============
logis(10+20)
EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:40){
  for(t in 10*(1:7))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(10),logis(20),logis(5)))))
        c=c+1
      }
}
for(i in 1:7){
  for(t in 10*(1:3))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+40,A,S,t,rdirichlet(1,c(logis(10),logis(20),logis(5)))))
      c=c+1
      }
}

for(i in 1:20){
  for(t in 10*(1:9))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+47,A,S,t,rdirichlet(1,c(logis(0),logis(0),logis(0)))))
        c=c+1
      }
}

for(i in 1:5){
  for(t in 10*(1:4))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+67,A,S,t,rdirichlet(1,c(logis(0),logis(0),logis(0)))))
        c=c+1
      }
}

for(i in 1:30)
  for(t in 10*(1:10))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+72,A,S,t,rdirichlet(1,c(logis(20),logis(-5),logis(0)))))
        c=c+1
      }
#102 countries 47+25+30
EXT=EXT[-1,]


EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:1)
  for(t in 10*(1:10))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(10),logis(20),logis(5)))))
        c=c+1
      }
EXT=EXT[-1,]

#----very small blocks--------------
EXT=matrix(nrow=1,ncol=8)
c=1
for(i in 1:40){
  for(t in 10*(1:2))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i,A,S,t,rdirichlet(1,c(logis(10),logis(20),logis(5)))))
        c=c+1
      }
}
for(i in 1:7){
  for(t in 10*(1:2))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+40,A,S,t,rdirichlet(1,c(logis(10),logis(20),logis(5)))))
        c=c+1
      }
}

for(i in 1:20){
  for(t in 10*(1:2))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+47,A,S,t,rdirichlet(1,c(logis(10),logis(10),logis(10)))))
        c=c+1
      }
}

for(i in 1:5){
  for(t in 10*(1:2))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+67,A,S,t,rdirichlet(1,c(logis(10),logis(10),logis(10)))))
        c=c+1
      }
}

for(i in 1:90)
  for(t in 10*(1:2))
    for(A in 0:0)
      for(S in 0:0){
        EXT=rbind(EXT,c(c,i+72,A,S,t,rdirichlet(1,c(logis(20),logis(-5),logis(0)))))
        c=c+1
      }
EXT=EXT[-1,]


#---change of function to gaussian, weights added
EXT=matrix(nrow=0,ncol=6)
for(i in 1:30)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,25,50,80),gauss(10*t,50,20,30),gauss(10*t,20,10,70)))))
for(i in 31:70)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,15,30,70),gauss(10*t,60,40,30),gauss(10*t,20,10,70)))))
for(i in 71:80)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,15,30,70),gauss(10*t,60,40,30),gauss(10*t,20,-20,50)))))

# Correcttau: c(.375,.5,.125)

#---difficult set
EXT=matrix(nrow=0,ncol=6)
for(i in 1:30)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,25,50,80),gauss(10*t,50,20,30),gauss(10*t,20,10,70)))))
for(i in 31:70)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,15,30,70),gauss(10*t,60,40,30),gauss(10*t,20,10,70)))))
for(i in 71:80)
  for(t in 1:10) EXT=rbind(EXT,c(i,10*t,10,rdirichlet(1,c(gauss(10*t,25,30,70),gauss(10*t,60,20,30),gauss(10*t,20,10,80)))))
