# Bayesian Comparison of sampling
# from an empirical distribution

#data <- na.omit(polv[,c("eth","Q12")])
data <- na.omit(polv[,c("eth","Q16")])
tolerance<-table(data)

# Examine the distribution with N samples of each
eth.success.failures<-function( eth, N, data){
  sdata <- as.numeric(unlist(data[data$eth == eth,2]))
  samp <- sample(sdata, N,replace=TRUE)
  print(head(samp))
  st<-table(samp)
  print(st)
  print(as.numeric(st[1]))
  list(s=st[1],f=st[2])
}

ps<-seq(0,1,by=0.001)
N <- 200
eths<-row.names(tolerance)
pdensities<-function(N){
  out<-data.frame()
  for (e in 1:6){
    eth<-eths[e]
    sf<-eth.success.failures( eth, N, data)
    s<-sf$s
    f<-sf$f
    vs<-exp(s*log(ps)+f*log(1-ps))
    vs<-1000*vs/sum(vs)
    print(paste(s,f))
    for (r in 1:length(ps)){
      p<-ps[r]
      v<-vs[r]
      out<-rbind(out, c(eth,p,v ))
    }
  }
  names(out)<-c("eth","p","val")
  out$p<-as.numeric(out$p)
  out$val<-as.numeric(out$val)
  out
}
ggplot(aes(x=p,y=val,group=eth,colour=eth),data=pden)+geom_line(lwd=3)
