# Happiness Q46
# Life Satisfaction Q49
# How do these vary with other finite variables?

hsensitivity<-function( variable ){
  dh<-table(na.omit(polv[,c("Q46",variable)]))
  ds<-table(na.omit(polv[,c("Q49",variable)]))
  Bh<-sq4(dh)
  Bs<-sq4(ds)
  hl <- Bh[1,1]/sum(Bh[,1])
  hh <- Bh[1,2]/sum(Bh[,2])
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
