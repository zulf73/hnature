# GHD shape fitting

z<-cubicspline(1:4,CfGov[4,],xi=seq(0,5,by=0.01))
z<-z/(sum(z)*0.01)
t<-seq(0,5,by=0.01)
g<-function( theta ){
  lambda<-theta[1]
  mu <- theta[2]
  sigma <- theta[3]
  gamma <- theta[4]
  alpha.bar <- theta[5]
  out <- ghyp( lambda=lambda,mu=mu,sigma=sigma,
               gamma=gamma,alpha.bar=alpha.bar)
  out
}

fit_ghd_shape<-function( t, z0, theta0=NULL, upper0=NULL, 
                         lower0=NULL){
  delta <- t[2]-t[1]
  eps<-1e-6
  z<-cubicspline(1:length(z0),z0,xi=t)
  z[z<eps]<-eps
  z<-z/(sum(z[t>0.5 & t<4.5])*delta)
  y<-z
  objective<-function( theta ){
    yp <- dghyp( t, object=g(theta))
    out<-sum( delta*(y[t>0.9 & t<4.3]- yp[t>0.9 & t < 4.3])^2)
    if (is.na(out)){
      print(theta)
      print(yp)    
    }
    out
  }
  if (is.null(theta0)){
    theta0 <-c(-3.0,0,0.5,1.5,1.0)
  }
  if (is.null(lower0)){
    lower0<-c(-1000,0,0.001,-Inf,0)
  }
  if (is.null(upper0)){
    upper0<-c(Inf,100,Inf,Inf,500)
  }
  res<-optim( theta0, fn=objective,
              lower=lower0,
              upper=upper0,
              method="L-BFGS-B",control=list(trace=1,maxit=5000))
  
  yp<-dghyp( t, object=g(res$par))
  list(theta=res$par,t=t,x=z,y=yp)
}

fit_ghd_table<-function( A ){
  t<-seq(0,5,by=0.01)
  idx<-which(t>=1.0 & t<= 4.0)
  nrow.A <- dim(A)[1]
  A.interp<-matrix(0,nrow=nrow.A,ncol=length(idx) )
  A.fitted<-matrix(0,nrow=nrow.A,ncol=length(idx) )
  thetas<-data.frame()
  delta<-t[2]-t[1]
  for (k in 1:nrow.A){
    cur.fit<-fit_ghd_shape(t,as.vector(A[k,]))
    thetas<-rbind( thetas, c( row.names(A)[k], cur.fit$theta))
    A.interp[k,] <- nrm(cur.fit$x[idx])/delta
    A.fitted[k,] <- nrm(cur.fit$y[idx])/delta
  }
  names(thetas)<-c("eth",
                   "lambda", "mu", "sigma",
                   "gamma","alpha.bar")
  for (r in 2:6){
    thetas[,r]<-as.numeric(thetas[,r])
  }
  
  row.names(A.interp)<- row.names(A)
  row.names(A.fitted)<- row.names(A)
  
  list(theta=thetas, interp=A.interp, fitted=A.fitted, t=t[idx])
}

ethtbl<-function( var, data ){
  table(na.omit(data[,c("eth",var)]))
}

eff.weight<-function(d,y,delta){
  sum( d^2*delta)/sum( y^2*delta)
}  

