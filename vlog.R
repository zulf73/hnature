vlog.pll<-function( y, k ){
  N<-length(y)
  t<-1:N
  if (k==1){
    m <- lm( y ~ t)
    a <- summary(m)$coef[2,1]
    obj <- -logLik(m)+2*log(length(y))
    out<-list(obj=obj,cut=1,m1=m,m2=m,a1=a,a2=a)    
  } else {
    y1 <- y[1:k]
    y2 <- y[(k+1):N]
    t1 <- t[1:k]
    t2 <- t[(k+1):N]
    m1 <- lm( y1 ~ t1 )
    m2 <- lm( y2 ~ t2 )
    a1 <- summary(m1)$coef[2,1]
    a2 <- summary(m2)$coef[2,1]
    obj<- -logLik(m1) + 2.1*log(k)
    obj<- obj - logLik(m2) + 2.1*log(N-k)
    obj<- obj + 2*exp(-4*abs(sign(a1)-sign(a2)))*log(N)
    if (is.infinite(-obj)){
      obj<-10000.0
    }
    out<-list(obj=obj,cut=k,m1=m1,m2=m2,a1=a1,a2=a2)    
  }
}


best.vlog<-function( y ) {
  N <- length(y)
  t <- 1:N
  ll.vlog<-rep(10000,N)
  
  for ( k in 3:(N-3) ){
    v <- vlog.pll(y, k)
    ll.vlog[k] <- v$obj
    #print(paste('k=',k,'a1=',a1,'a2=',a2,'obj=',obj))
  }
  ll.vlog[1] <- -logLik( lm( y ~ t) ) + 2*log(N)
  minidx <- which.min( ll.vlog )
  print(minidx)
  vmin<-vlog.pll( y, minidx )
  list(vmin=vmin,ll.vlog=ll.vlog)
}
