# Zulf's Proportional Chi-Square
library(compiler)
enableJIT(3)

prop.chisq<-function( data ){
  m<-dim(data)[1]
  n<-dim(data)[2]
  t<-0
  v0<-data[1,]/sum(data[1,])
  for (j in 2:m){
    v<-data[j,]/sum(data[j,])
    t<- t + sum( (v-v0)^2/v0^2)
  }
  df<-1
  t0<-qchisq(0.95, df=df)
  pval <- 1 - pchisq( t, df=df)
  list(tstat=t,pval=pval,crit=t0)
}


g<-function(x){
  #print(x)
  log(-log(x)+0.01)
}

zulf.sigma2<-function(z){
  n<-length(z)
  if (n==1){
    return(1)
  }
  #print(n)
  D<-matrix(0, nrow=n, ncol=n)
  diag(D)<-1
  #print(dim(D))
  s<-0
  for (j in 2:n){
    for (k in 1:j){
      gg = abs(z[j] - z[k])
      D[j,k] <- gg
      D[k,j] <- gg
      s<-s + gg
    }
  }
  out<- 2*s/(n^2-n)
  out
}

zulf.sigma<-cmpfun(zulf.sigma2)

zulf.chisq2<-function( data ){
  data<-as.matrix(data)
  m<-dim(data)[1]
  n<-dim(data)[2]
  if ( m<=1 || n<=1){
    return(list(tstat=0,pval=1,t0=1))
  }
  t<-0
  eps<-0.00001
  w0 <- (data[1,]+eps)/sum(data[1,]+eps)
  #print('this')
  #print(length(w0))
  
  v0<- log(-log(abs(w0)+eps))
  #print(v0)
  if (is.na(v0)){
    return(list(tstat=0,pval=1,t0=1))
  }
  sigma0 <- zulf.sigma( v0 )
  mu0 <- mean(v0)
  for (j in 2:m){
    w<-(data[j,]+eps)/sum(data[j,]+eps)
    #print(w)
    v<-g(w)
    #print('works')
    #print(v)
    #print('---')
    sigma <- zulf.sigma( v )
    mu <- mean(v)
    z <- v/sigma
    z0 <- v0/sigma0
    t<- t + sum( ( z - z0 )^2)
  }
  df<-(n-1)*(m-1)
  t0<-qchisq(0.95, df=df)
  pval <- 1 - pchisq( t, df=df)
  list(tstat=t,pval=pval,crit=t0)
}

zulf.chisq<-cmpfun(zulf.chisq2)


# calculate the discrete correlation
# matrix of WVS7
tablify<-function(a, b){
  H<-na.omit(data.frame(a,b))
  T<-matrix(1,nrow=1,ncol=1)
  tryCatch( { T<-table(H) } )
  T
}

cor.d<-function( data ){
  n<-dim(data)[2]
  out<-matrix(0,nrow=n, ncol=n)
  diag(out)<-1
  if (n==2){
    out[1,1]<-1
    out[2,2]<-1
    a<-data[,1]
    b<-data[,2]
    X<-tablify( a, b )
    metrics <- zulf.chisq(X)
    out[1,2] <- 1 - metrics$pval
    out[2,1] <- 1 - metrics$pval
  }
  
  if (n>2){
    for (j in 2:n){
      cvs<-c()
      for (k in 1:j ){
        a<-data[,j]
        b<-data[,k]
        X<-tablify( a, b )
        metrics <- zulf.chisq(X)
        cv<-1 - metrics$pval
        cvs<-append(cvs, cv)
        out[j,k] <- cv
        out[k,j] <- cv
      }
      print(j)
      print(tail(cvs,10))
    }
  }
  out
}

