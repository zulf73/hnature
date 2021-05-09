# Zulf's Pairwise Variable Invariant for Bad People

collapse.byclass<-function(tenSquare){
  threeSquare<-matrix(0,nrow=3,ncol=3)
  S<-tenSquare
  T<-threeSquare
  
  T[1,1]<-sum(S[1:3,1:3])
  T[1,2]<-sum(S[1:3,4:7])
  T[1,3]<-sum(S[1:3,8:10])
  
  T[2,1]<-sum(S[4:7,1:3])
  T[2,2]<-sum(S[4:7,4:7])
  T[2,3]<-sum(S[4:7,8:10])
  
  T[3,1]<-sum(S[8:10,1:3])
  T[3,2]<-sum(S[8:10,4:7])
  T[3,3]<-sum(S[8:10,8:10])
  
  T
}

pairwise.dist<-function( v1, v2 ){
  A<-table(extreme[,c(v1,v2)])
  B<-collapse.byclass(A)
  B<-B/sum(B)
  v<-rep(0,9)
  v[1:3]<-B[1,]
  v[4:6]<-B[2,]
  v[7:9]<-B[3,]
  v
}

pairwise.df<-data.frame()

vars <- names(extreme)[4:22]
K<-length(vars)
for (kt in 1:K){
  for (jt in 1:K){
    iq <- vars[kt]
    tq <- vars[jt]
    if ( iq == 'Q192' | tq == 'Q192' | iq == tq){
      next
    }
    S<-as.vector(pairwise.dist(iq,tq))
    if (is.null(pairwise.df)){
      pairwise.df<-rbind(pairwise.df,S)
    }
    pairwise.df<-rbind(pairwise.df,S)
  }
}

