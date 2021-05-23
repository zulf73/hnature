# determine t_1, t_2, t_3, t_4 assuming t_0=0

adjust_times<-function( abparams, t0, data ){
  a<-abparams[1:5]
  b<-abparams[6:10]
  objective<-function( t ){
    q <- matrix( 0, nrow=length(t)+1, ncol=5)
    tp<-c(0,t)
    for ( r in 1:5 ){
      for (s in 1:length(tp)){
        q[s,r] <- b[r] + a[r]*tp[s]
      }
    }
    error <- 0
    for (r in 1:5){
      de <- norm( data[,r] - q[,r], type="2")^2
      error <- error + de
    }
    error<-sqrt(error)
    #print(error)
    error
  }
  print('opt done')
  res <- optim( t0, objective, method="Nelder-Mead")
  list(t=res$par, error=res$value )
}

adjust_abs <- function( t, data) {
  as <- rep(0,5)
  bs <- rep(0,5)
  tp <- rep(1,5)
  tp[2:5] <- tp[2:5]+t 
  print(tp)
  for (r in 1:5){
    y<-data[,r]
    mod<-lm(y~ tp )
    print(paste('r2=',summary(mod)$r.squared))
    as[r] <- coef(mod)[2]
    bs[r] <- coef(mod)[1]
  }
  out<-c(as,bs)
  out
}

det.trust.pars<-function(data){
  error <- 1e8
  t0<-1:4
  ab <- adjust_abs(t0, data)
  t <- t0
  count<-0
  perror<-1e8
  while ( error > 0.01  & count < 150){
    w <- adjust_times( ab, t, data)
    t <- w$t
    ab <- adjust_abs( t, data)
    perror <- error
    error <- w$error
    print(count)
    count<-count+1
  }
  list(ab=ab,t=t)
}
