# Search for understanding 
# terrorism supporters
polv <- na.omit(wvs7[,c("Q290","Q46", "Q57","Q177","Q178","Q179","Q180","Q181","Q182","Q183","Q184","Q185","Q186","Q187","Q188","Q189","Q190","Q191","Q192","Q193","Q194","Q195","Q275","Q192")])
polv$eth<-ethnicity_map(as_factor(polv$Q290))
extreme<-polv[which(polv$Q192 == 9 | polv$Q192 == 10),]

uvgrp<-function( var ){
  a <-nrm(table(polv[,c(var)]))
  b <-nrm(table(extreme[,c(var)]))
  data.frame(gen=a*100,ext=b*100)
}

vars <- names(extreme)[4:22]
lds<-rep(0,18)
rsq<-rep(0,18)
t4<-1:4
for (k in 1:length(vars)){
  y <- rev(log(uvgrp(vars[k])[,4]+1e-6))[1:4]
  mod<-lm( y ~ t4 )
  lds[k]<- summary(mod)$coef[2,1]
  rsq[k]<- summary(mod)$r.squared
}
hobbes<-data.frame(var=vars,lambda=lds,rsq=rsq)
