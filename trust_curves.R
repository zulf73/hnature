# Use Other to determine the trust
# densities of the human race
ab <- c( 0.010585097,  0.019156924, -0.006620259, 0.007031738,
         0.011095266,  1.943972143, -2.919369783, -0.247401488,
         0.518246653,  1.363879411)

tref <- c( 0, 35.30684, 42.12769, 287.43995,  53.64879)

# Inference 0-35.3 is closer than neighbor or known
# person

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

trust_theta<-function( t ){
  tau<-rep(0,5)
  a<-ab[1:5]
  b<-ab[6:10]
  for (r in 1:5){
    tau[r] <- b[r] + a[r]*t
  }
  theta <- exp( tau )
  theta  
}

trust_curve<-function( x, t ){
  theta <- trust_theta( t )
  dghyp(x, object=g(theta) )
}


