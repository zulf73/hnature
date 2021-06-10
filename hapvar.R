# Happiness Q46
# Life Satisfaction Q49
# How do these vary with other finite variables?

sq4<-function(mtx){
  a<-dim(mtx)[1]
  b<-dim(mtx)[2]
  out<-matrix(0,nrow=2,ncol=2)
  out[1,1]<-sum(mtx[1:(a/2),1:(b/2)])
  out[1,2]<-sum(mtx[1:(a/2),((b/2)+1):b])
  out[2,2]<-sum(mtx[((a/2)+1):a,((b/2)+1):b])
  out[2,1]<-sum(mtx[((a/2)+1):a,1:(b/2)])
  out
}

hsensitivity<-function( variable ){
  dh<-table(na.omit(polv[,c("Q46",variable)]))
  ds<-table(na.omit(polv[,c("Q49",variable)]))
  Bh<-sq4(dh)
  Bs<-sq4(ds)
  hh <- Bh[1,1]/sum(Bh[,1])
  hl <- Bh[1,2]/sum(Bh[,2])
  sh<- Bs[2,1]/sum(Bs[,1])
  sl<- Bs[2,2]/sum(Bs[,2])
  list(htable=dh,stable=ds,hsq=Bh,ssq=Bs,hh=hh,hl=hl,sh=sh,sl=sl)  
}

happiness.var<-function(varlist){
  K <- length(varlist)
  haplo<-rep(0,K)
  haphi<-rep(0,K)
  satlo<-rep(0,K)
  sathi<-rep(0,K)
  for ( r in 1:K){
      sens <- hsensitivity( varlist[r] )
      haplo[r] <- sens$hl
      haphi[r] <- sens$hh
      satlo[r] <- sens$sl
      sathi[r] <- sens$sh
  }
  out<-data.frame(v=varlist,hh=haphi,hl=haplo,sh=sathi,sl=satlo)
  out
}

happiness.morals.sensitivity<-function(){
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
  mdesc <- c("undeserved govt benefits", 
             "avoid paying public transport", 
             "stealing property", 
             "cheating on taxes", 
             "taking bribes on duty",
             "homosexuality", 
             "prostitution", 
             "abortion", 
             "divorce", "sex before marriage", "suicide", "euthanasia", "man beating wife", "beating children", "violence against other people", "terrorism as political, religious mean", "casual sex", "political violence", "death penalty")
    out<-happiness.var(mvar)
    out$sd<-out$sh-out$sl
    out$hd<-out$hh-out$hl
    out$desc<-mdesc
    out
}
nvars<-c("Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26")

nhs<-happiness.var(nvars)

nhs$sd<-nhs$sh-nhs$sl

nhs$hd<-nhs$hh-nhs$hl

data.frame(v=nhs$v,sd=nhs$sd,hd=nhs$hd)

ndesc<-c("drug addicts", "other race", "AIDS", "immigrants","homosexuals", "other religion","heavy drinkers", "unmarried couples", "other language")
memdesc<-c("church", "sports", "art", "labor" ,"political","environment" ,"pro","charitable","consumer" ,"selfhelp","women","other")
memvars<-c("Q94","Q95","Q96","Q97","Q98","Q99","Q100","Q101","Q102","Q103","Q104","Q105")

