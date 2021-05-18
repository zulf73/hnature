# Suppose the data x_1,...,x_N
# come from ghd(mu,theta).
# We do not know the actual mu at all
# Our task is to compute an estimator
# mu0.  What is the confidence interval 
# for mu0 in ( mu - a, mu + a)?

library(ghyp)
confint.ghyp<-function( x, fudge ){
  n<-length( x )
  fit.x <- fit.ghypuv(x,mu=0.1,
            sigma=sd(x),
            control=list(maxit=5000),
            silent=T)
  fx<-coef(fit.x)
  mu<-fx$mu
  print(summary(fit.x))
  M<-500
  tcount<-0
  error.dist<-ghyp(
                  mu = 0,
                  lambda=fx$lambda,
                   alpha.bar=fx$alpha.bar,
                   sigma = fx$sigma,
                   gamma = fx$gamma)

  tail<-qghyp( 0.95 )/(n/fudge)
  f <- sum((x - mu)^2)/n^2
  for ( r in 1:M ){
    gsmp<-rghyp(n, object=error.dist)
    e<-sum(gsmp^2)/n^2
    diff<-abs(f-e)
    if (diff> tail^2){
      tcount<-tcount+1
    }
    
    if (r%%10==0){
      print(paste(r,diff,tcount/M))
    }
  }
  width<-tcount/M
  list(estimate=mu,low=mu-width,high=mu+width,fit=fit.x)
}

fit_and_plot_density<-function( var ){
  par(mar=c(3,3,3,2.1))
  V<-as.numeric(unlist(na.omit(wvs7[,var])))
  gg<-NULL
  thresh<-seq(17,30,by=0.2)
  for (t in thresh){
    gg<-confint.ghyp(V, 17.8)
    w <- gg$high - gg$low
    if ( w > 0.005 && w < 0.04 ){
      break
    }
  }
  caption<-paste(var, "density and GHD fit")
  out<-plot(density(V,bw=0.5),ylim=c(0,0.4),main=caption)
  xt<-seq(0,10,by=0.1)
  lines(xt,dghyp(xt,object=gg$fit),col=4,lwd=3)
  list(plt=out, data=gg)
}
