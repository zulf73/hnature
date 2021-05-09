# Persistence of Character

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

same.class.stats<-function( v1, v2 ){
  A<-table(extreme[,c(v1,v2)])
  B<-collapse.byclass(A)
  iclass<-c(1,2,3)
  psame<-rep(0,3)
  pdiff<-rep(0,3)
  psame[1] <- B[1,1]/sum(B[1,])
  pdiff[1] <- 1 - psame[1]

  psame[2] <- B[2,2]/sum(B[2,])
  pdiff[2] <- 1 - psame[2]

  psame[3] <- B[3,3]/sum(B[3,])
  pdiff[3] <- 1 - psame[3]
  
  list(iclass=iclass,psame=psame,pdiff=pdiff)
}

vars <- names(extreme)[4:22]
iquest <- c()
tquest <- c()
iclass <- c()
psame  <- c()
pdiff <- c()
K<-length(vars)
for (kt in 1:K){
  for (jt in 1:K){
    iq <- vars[kt]
    tq <- vars[jt]
    if ( iq == 'Q192' | tq == 'Q192' | iq == tq){
      next
    }
    S<-same.class.stats( iq, tq )
    J<-length( S$psame)
    iquest<-append(iquest, rep(iq,J))
    tquest<-append(tquest, rep(tq,J))
    iclass<-S$iclass
    psame<-S$psame
    pdiff<-S$pdiff
  }
}
char.persist<-data.frame( iq=iquest, tq=tquest, 
                          ic=iclass, psame=psame, 
                          pdiff=pdiff)
