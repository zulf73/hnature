# We will use logistic regression
# with x variable artificially created for Q7-Q17
# We take N=500 points to determine p, lambda
# Then we use these to assign random x values
# for all the other values
# then we fit logistic regression on the (x,g)

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
      if (pickx <= 5){
        if ( grps[r] == sval){
          xs[r] <- pickx
          done <- T
        }  
      }
      if (pickx > 5){
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
  print(head(y))
  x<-samp.binary.exp( y, lambda)
  G<-as.numeric(as_factor(t(y)))-1
  out<-data.frame( x=(x-min(x))/(max(x)-min(x)),G=G)
  names(out) <- c("x","G")
  out
}
