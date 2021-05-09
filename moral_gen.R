# Reproduce M=20 Exponentials
# for entire human race

lambda <- 0.52
p11 <- 0.6
P <- matrix(0,nrow = 10, ncol=10)
P[1,1] = p11
for (k in 2:10){
  for (r in 1:k){
    P[k,r] <- exp(-lambda*(k+r))*p11
    P[r,k] <- P[k,r]
  }
}
for ( r in 1:10){
  P[r,]<-P[r,]/sum(P[r,])
}


Y1 <- floor(rexp(1000,rate=0.5))+1
Y1[Y1>10]<-10

library(markovchain)
mcVals = new("markovchain", states = as.character(seq(1,10)),
            transitionMatrix = P,          
            name = "Vals")

y1=Y1
all.Y<-matrix(0,nrow=1000,ncol=20)
all.Y[,1]<-Y1
for (kk in 1:1000) {
  outs <- rmarkovchain(n = 19, object = mcVals, what = "list")
  all.Y[kk,2:20]<-outs  
}

get.curve<-function(kk){
  Z<-table(all.Y[,kk])
  v<-rep(0,10)
  for (r in 1:10){
    v[r]<-Z[as.character(r)]
  }
  v<-v/sum(v)
  v
}
for (r in 2:10){
  a<-summary(lm(log(get.curve(r))~t10))$r.squared
  print(a)
}



