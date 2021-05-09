library(ggplot2)
# We are given a vector X of categorical 
# values v_1, ..., v_p
# We want to randomly sample from X
# and examine the probability distribution 
# in the simplex of R^p
# This we will apply to questions of 
# Human Nature 
# we set p=4 to use with Pew 2014 globatt
# data
# we want to get a nice density 2D plot
# with the intensity
freq_dist<-function(X0, ssize=1000, nsampling=1){
  X <- X0[ !is.na(X0) ]
  out<-matrix(0,nrow=nsampling,ncol=4)
  tf<-1:4
  N <- length(X)
  for (k in 1:nsampling){
    y <- sample(X, ssize)
    fmed<-table(y)
    fmed<-fmed[1:4]
    fmed<-fmed/sum(fmed)
    out[k,]<-fmed
  }
  out
}

freq_dist_minspread<-function(X0, ssize=1000, nsampling=1,
                              minspread=20){
  countries<-X0[,1]
  X <- X0[,2]
  nc<-length(unique(na.omit(X)))
  out<-matrix(0,nrow=nsampling,ncol=nc)
  tf<-1:nc
  N <- length(X)
  for (k in 1:nsampling){
    
    spread_met <- F
    idxy <- sample(seq(1,N), ssize)
    while (!spread_met){

      if (length(unique(countries[idxy])) > minspread){
        y <- X[idxy]
        break;
      }
      idxy <- sample(seq(1,N), ssize)
    }
    fmed<-table(y)
    fmed<-fmed[1:nc]
    fmed<-fmed/sum(fmed)
    out[k,]<-fmed
  }
  out
}

freq_dist_fixed_country<-function(X0, ssize=1000, nsampling=1,
                              country=840){
  countries<-X0[,1]
  X <- X0[X0[,1]==country,2]
  print(dim(X))
  nc<-dim(unique(X[,1]))[1]
  print(nc)
  out<-matrix(0,nrow=nsampling,ncol=nc)
  tf<-1:nc
  N <- dim(X)[1]
  for (k in 1:nsampling){
    
    idxy <- sample(seq(1,N), ssize,replace=T)
    y<-X[idxy,]
    head(y)
    #print(length(unique(y)))
    fmed<-table(y)
    fmed<-fmed[1:nc]
    fmed<-fmed/sum(fmed)
    out[k,]<-fmed
  }
  out
}

stat_curve<-function( rawdata ){
  nc<-dim(rawdata)[2]
  x<-rep(0,nc)
  sx<-rep(0,nc)
  for (jj in 1:nc){
    x[jj]<-mean(rawdata[,jj],na.rm=T)*100
    sx[jj]<-sd(rawdata[,jj],na.rm=T)*100
  }
  tf<-1:nc
  curve.data <-data.frame(x=x,sx=sx,tf=tf)
  curve.data
}

shaded_curve<-function( rawdata ){
  curve.data <- stat_curve( rawdata )
  fig<-ggplot(curve.data, aes(x = tf , y = x)) +
    geom_line() +
    geom_ribbon(aes(ymin = x - sx,
                    ymax = x + sx),
                fill = "#00abff",
                alpha = 0.2)
  fig
}
#fmed.dat<-as.data.frame(globatt)[!is.na(globatt$Q38D),c("COUNTRY","Q38D")]

exp_lambda<-function( curve ){

  clen<-length(curve$x)
  ctime<-1:clen
  mod<-lm( log( curve$x) ~ ctime)
  lambda<-summary(mod)$coef[2,1]
  rsq<-summary(mod)$r.squared
  list(lambda=lambda, rsq=rsq)
}

country_lambda<-function( X0 ){
  
  cts<-unique(X0[,1])
  print(cts)
  lds<-rep(0,dim(cts)[1])
  rsqs<-rep(0,dim(cts)[1])
  for ( k in 1:dim(cts)[1] ){
    country<-(cts[k,1])[[1]]
    print(country)
    rd<-freq_dist_fixed_country(steal2,ssize=1000,nsampling=500,
                                country=country)
    crv<-stat_curve(rd)
    mr<-exp_lambda( crv )
    lds[k]<-mr$lambda
    rsqs[k]<-mr$rsq
  }
  data.frame( country=cts, lambda=lds, rsq=rsqs)
}

shade_curve <- function(MyDF, zstart, zend, fill = "red", alpha = .5){
  geom_area(data = subset(MyDF, x >= zstart
                          & x < zend),
            aes(y=y), fill = fill, color = NA, alpha = alpha)
}

unused<-function(){
  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
}


lambda.table<-function( vars ){
  out <- NULL
  for ( j in 1:length(vars) ){
    data <- na.omit(wvs7[,c("B_COUNTRY",vars[j])])
    ld <- country_lambda( data )
    if ( is.null(out) ){
      out <- ld
      names(out)[(2*j):(2*j+1)]<-c( vars[j], paste('r2.',vars[j],sep=''))
    } else {
      out<-merge(out, ld, by="B_COUNTRY")
      names(out)[(2*j):(2*j+1)]<-c( vars[j], paste('r2.',vars[j],sep=''))
    }
  }
  out
}
