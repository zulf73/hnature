# We will use logistic regression
# with x variable artificially created for Q7-Q17
# We take N=500 points to determine p, lambda
# Then we use these to assign random x values
# for all the other values
# then we fit logistic regression on the (x,g)

chdesc<-c("good manners", "independence", "hard work",
          "feeling of responsibility", "imagination", 
          "tolerance and respect for others", "thrift saving money",
          "perserverence","religious faith", "unselfishness","obedience")

samp.binary.exp<-function( grps, lambda ){
  grps<-as.vector(t(grps))
  n <- length(grps)
  print(n)
  gvals <- unique(grps)
  #print(gvals)
  bval <- 0
  sval <- 1
  if ( length(gvals) == 2 ){
    if ( sum(gvals==gvals[1]) >= n/2 ){
      bval <- gvals[1]
      sval <- gvals[2]
    } else {
      sval <- gvals[1]
      bval <- gvals[2]
    }
  } else {
    return(NULL)
  }
  xs<- rep( 0, n)
  for ( r in 1:n ){
    done <- F
    
    while ( done == F){
      pickx <- rexp(1,rate=1/lambda)
      #print(pickx)
      if (pickx <= log(2)/lambda){
        if ( grps[r] == sval){
          xs[r] <- pickx
          done <- T
        }  
      }
      if (pickx > log(2)/lambda){
        if ( grps[r] == bval){
          xs[r] <- pickx
          done <- T
        }  
      }
    }
  }
  xs
}

dataset.logit<-function(var,lambda){
  y<-na.omit(polv[,var])
  G<-as.numeric(as_factor(t(y)))-1
  p<-sum(G==0)/length(G)
  if ( p < 0.5) {
    G<-1-G
  }
  p<-sum(G==0)/length(G)
  print(p)
  
  x<-samp.binary.exp( G, lambda)
  out<-data.frame( x=x,G=G)
  names(out) <- c("x","G")
  out
}

dataset.logit.fixed.lambda<-function(var){
  y<-na.omit(polv[,var])
  G<-as.numeric(as_factor(t(y)))-1
  p<-sum(G==0)/length(G)
  if ( p < 0.5) {
    G<-1-G
  }
  p<-sum(G==0)/length(G)
  print(p)
  lambda <- 2*log( p/(1-p) )
  #print(head(y))
  x<-samp.binary.exp( y, max(lambda,2.4))
  out<-data.frame( x=x,G=G)
  names(out) <- c("x","G")
  out
}

if (FALSE){
lq14<-dataset.logit.fixed.lambda("Q14")
mq14 = glm( G ~ x, family="binomial",data=lq14)
summary(mq14)
}



dataset.logit.eth<-function(var,eth){
  y0<-na.omit(polv[,var])
  G0<-as.numeric(as_factor(t(y0)))-1
  p0<-sum(G0==0)/length(G0)
  if ( p0 < 0.5) {
    G0<-1-G0
  }
  p0<-sum(G0==0)/length(G0)
  lambda0 <- 2*log( p0/(1-p0) )
  #lambda <- max(min(lambda,2.7),1.5)
  lambda0 <- max(lambda0,2)
  #print(head(y))
  x0<-samp.binary.exp( y0, lambda0)
  
  idx<-as.character(polv$eth)==eth
  y<-na.omit(polv[idx,var])
  G<-as.numeric(as_factor(t(y)))-1
  x<-x0[idx]
  p<-sum(G==0)/length(G)
  if ( p < 0.5) {
    G<-1-G
  }
  p<-sum(G==0)/length(G)
  #print(p)
  lambda <- 2*log( p/(1-p) )
  out<-data.frame( x=x,G=G)
  names(out) <- c("x","G")
  list(df=out,lambda=lambda)
}

dataset.logit.eth2<-function(var,eth){
  y<-na.omit(polv[as.character(polv$eth)==eth,var])
  G<-as.numeric(as_factor(t(y)))-1
  p<-sum(G==0)/length(G)
  if ( p < 0.5) {
    G<-1-G
  }
  p<-sum(G==0)/length(G)
  print(p)
  lambda <- 2*log( p/(1-p) )
  #lambda <- max(min(lambda,2.7),1.5)
  lambda <- min(max(lambda,0.3),2)
  #print(head(y))
  x<-samp.binary.exp( y, lambda)
  out<-data.frame( x=x,G=G)
  names(out) <- c("x","G")
  list(df=out,lambda=lambda)
}

vars<-c("Q7","Q8","Q9","Q10","Q11","Q12",
        "Q13","Q14","Q15","Q16","Q17")

eths<-c("Arab", "Black","East Asian", "Indian", "Other", "White")

child.eth<-function(){
  lambdas <- matrix( 0, nrow=length(vars), ncol=length(eths))
  alphas <- matrix( 0, nrow=length(vars), ncol=length(eths))
  pvals <- matrix( 0, nrow=length(vars), ncol=length(eths))
  bdf <- data.frame()
  for (r in 1:length(vars)){
    for (s in 1:length(eths)){
      A<-dataset.logit.eth( vars[r], eths[s])
      m <- glm( G ~ x, family="poisson", data=A$df)
      print(summary(m))
      lambdas[r,s]<-A$lambda
      alphas[r,s]<-summary(m)$coefficients[2,1]
      pvals[r,s]<-summary(m)$coefficients[2,4]
      bdf<-rbind(bdf,c(vars[r],eths[s],alphas[r,s],lambdas[r,s],pvals[r,s]))
    }
  } 
  names(bdf)<-c("var","eth","alpha","lambda","pval")
  list(df=bdf,lambda=lambdas,alpha=alphas,pvals=pvals)
}


lgchoose<-function(n,k){
  out<- log( n/(sqrt(2*pi)*k*(n-k))) + 
    n*log(n/exp(1)) - k*log(k/exp(1)) - (n-k)*log((n-k)/exp(1))
  out
}

binom<-function(p,n,k){
  exp(lgchoose(n,k)+k*log(p)+(n-k)*log(1-p))
}

bv<-function(p,n,alpha){
  z<-rep(0,10)
  for (k in 1:10){
    z[k] <- binom(p*alpha,n,k*alpha)
  }
  nrm(z) 
}

bv5<-function(p,n,alpha){
  z<-rep(0,5)
  for (k in 1:5){
    z[k] <- binom(p,n*alpha,k*alpha)
  }
  nrm(z) 
}


binom.err<-function( theta ){
  p <- theta[1]
  n <- theta[2]
  alpha <- theta[3]
  y <- bv5(p,n,alpha)
  print(y)
  norm( y - nrm(z[1:5]), type="2")
}

binom.err2<-function( theta, z){
  p <- theta[1]
  n <- theta[2]
  alpha <- theta[3]
  y <- bv(p,n,alpha)
  #print(y)
  norm( y - nrm(z), type="2")
}


fit.binom.quad<-function( z, theta0){

  f<-function(theta){
    binom.err2(theta,z)
  }

  lb0<-c(0.01,10,0.01)
  res<-nloptr(theta0,f,lb=lb0,
              opts=list("algorithm"="NLOPT_LN_NELDERMEAD",
                      "xtol_rel"=1.0e-6,
                      "maxeval"=5000,
                      "print_level"=1))

  theta<-res$solution
  y<-bv( theta[1], theta[2], theta[3])
  
  s<-1:10
  weights<-exp(-s/2)
  weights<-nrm(weights)
  qmod<-lm( log(z) ~ poly(log(y),2), weights=weights)
  print(summary(qmod))
  yp<-exp(predict(qmod))
  yp<-nrm(yp)
  
  list(yp=yp,bpars=theta,qpars=coef(qmod))
}

lin.scale<-function(qpars){
  s1<-as.numeric(qpars[3])
  s2<-as.numeric(qpars[2])
  s3<-as.numeric(qpars[1])
  c1<-sqrt(s1)
  c2<-s2/(2*c1)
  list(c1=c1,c2=c2)
}
mvar<-c("Q177",
        "Q178",
        "Q179",
        "Q180",
        "Q181",
        "Q182",
        "Q183",
        "Q184",
        "Q185",
        "Q186",
        "Q187",
        "Q188",
        "Q189",
        "Q190",
        "Q191",
        "Q192",
        "Q193",
        "Q194",
        "Q195"
)

fit.linscale<-function(name.vars){
  N<-length(name.vars)
  out<-data.frame()
  for (r in 1:N){
    print(r)
    v<-name.vars[r]
    print(v)
    
    z<-nrm(table(na.omit(polv[,v])))
    print('z is ok')
    pp<-fit.binom.quad(z,c(0.1,100,0.1))
    c12<-lin.scale(pp$qpars)
    print(c12)
    out<-rbind(out, c(v,c12$c1,c12$c2))
  }
  names(out)<-c("var","c2","c1")
  out[,2]<-as.numeric(out[,2])
  out[,3]<-as.numeric(out[,3])
  out
}
