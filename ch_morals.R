library(nloptr)
require(compiler)
enableJIT(3)

# We have 11 genuine bona fide Bernoulli random
# variables from Nature here and not from
# all manner of disreputable institutions
# from Nevada and Atlantic City and so on
# and it is a great opportunity to actually
# do Bernoulli analysis for inference for
# true parameter

# Diatribe:  the true parameter of a Bernoulli
# random variable from Nature is not a "biased coin"
# I cannot stand Science and Man's great ambitions 
# regarding the truth of Nature reduced to sordid
# business of gambling and all sorts of unsavory 
# and depressing characters without meaning in their
# lives who have their savings taken away by these
# predatory charlatans.  
# We will be saving the reputation of binary random
# variables from gambling houses.

chvars<-c("Q7","Q8","Q9","Q10", "Q11",
          "Q12", "Q13", "Q14", "Q15", "Q16", "Q17")


zulf.bernoulli.p.est<-function( x, K){
  N <- length(x)
  xd<-matrix(0, nrow = N-K, ncol = K)
  for ( r in 1:(N-K)){
    xd[r,] <- t(x[r:(r+(K-1)),1])
  }

  objfun<-function( p ){
    v <- 0
    for (k in 1:2^K){
      for (r in 1:K) {
        theoretical <- log(choose(K,r)) + 
                      r*log(p+1e-6) + (K-r)*log(1-p+1e-8)
        
        empirical <- log(count_r_heads( xd )+1e-8)
        v <- v + ( theoretical - empirical )^2
      }
    }
    return (sqrt(v))
  }
  x0<-0.5
  res<-nloptr(x0, eval_f=objfun, eval_grad_f = NULL, 
         lb = 0.0, ub = 1.0)
  res$solution
}



K <- 15
eps<-1e-10
VERBOSE<-0
y<-na.omit(polv[,"Q16"])
x<-y[10001:15000,1]
print(dim(x))
N <- dim(x)[1]
xd<-matrix(0, nrow = N-K, ncol = K)
for ( r in 1:(N-K)){
  xd[r,] <- t(x[r:(r+(K-1)),1])
}
#xdtbl<-table( data.frame(xd))

count_r_heads<-function( xd, r, K){
  vc <- 0
  nr <- nrow( xd )
  for (s in 1:nr){
   tfreq <- table( xd[s,])
   if ( tfreq[1] == r ){
     vc <- vc + 1
   }
  }
  vc/nr
}
  
objfun<-function( p){
  v <- 0
  if (VERBOSE){
    print('-------OBJECTIVE------')
    print(paste('p=',p))
  }
  for (r in 1:K) {
      theoretical <- log(choose(K,r)) + 
        r*log(abs(p)+eps) + (K-r)*log(abs(1-p)+eps)
      
      empirical <- log(count_r_heads( xd, r, K)+eps)
      delta <- theoretical - empirical
      
      if (VERBOSE){
      print(paste('r=',r,
                  'log(theory)=', theoretical,
                  'log(emp)=', empirical,
                  'delta=', delta))
      }
      v <- v + delta^2
  }
  print(paste('sum squared=',v))
  v
}

gradfun<-function(p){
  v <- 0
  for (r in 1:K) {
    theoretical <- log(choose(K,r)) + 
      r*log(abs(p)+eps) + (K-r)*log(abs(1-p)+eps)
      
    dtheoretical <- r/(p+eps) - (K-r)/(1-p+eps)
    empirical <- log(count_r_heads( xd, r, K)+eps)
    v <- v + 2*( theoretical - empirical )*dtheoretical
  }
  v
}

x0<-c(0.1)

nopts <- list("algorithm"="NLOPT_GN_DIRECT",
             "xtol_rel"=1.0e-12, "maxit"=1000)

res<-nloptr(x0, eval_f=objfun, eval_grad_f = gradfun, 
            lb = 1e-6, ub = 1-1e-6, opts = nopts )

res$solution
p.zulf<-res$solution
samp.heads<-table(x)[1]
samp.tails<-table(x)[2]
p.samp<-samp.heads/(samp.heads+samp.tails)
true.heads<-table(y)[1]
true.tails<-table(y)[2]
p.true<-true.heads/(true.heads+true.tails)

sample.freq.err<-abs(p.samp-p.true)
zulf.err<-abs(p.zulf-p.true)


