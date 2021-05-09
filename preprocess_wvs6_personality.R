# Load dataset (Stata format)wvs <-import("WV6_Stata_v_2016_01_01.dta")# Code missing valuestrait.vars <-c("V160A", "V160B", "V160C", "V160D", "V160E","V160F", "V160G", "V160H", "V160I", "V160J")
# Code missing values
trait.vars <- c("V160A", "V160B", "V160C", "V160D", "V160E",
                "V160F", "V160G", "V160H", "V160I", "V160J")
wvs[trait.vars][wvs[trait.vars] < 0] <- NA

wvs[trait.vars][wvs[trait.vars] < 0] <- NA
# Reverse code and save variables

wp<-wvs
wp$o1 <- wp$V160J
wp$o2 <- (wp$V160E-6)*-1

wp$c1 <- wp$V160H
wp$c2 <- (wp$V160C-6)*-1

wp$e1 <- wp$V160F
wp$e2 <- (wp$V160A-6)*-1

wp$a1 <- wp$V160B
wp$a2 <- (wp$V160G-6)*-1

wp$s1 <- wp$V160D
wp$s2 <- (wp$V160I-6)*-1
wp$male <- wvs$V240
wp$male[wp$male < 0] <- NA
wp$male[wp$male == 2] <- 0

wp$age <- wvs$V242
wp$age[wvs$age < 0] <- NA
wp[wp<0]<-NA


nms<-names(wp)
r2s<-rep(0,430)
for (k in 1:430){
  print(k)
  nu<-length(unique(wp[,k]))
  fna <-sum(is.na(wp[,k])) 
  if ( nu >1 && fna/length(wp[,k]) <0.2 ){
    mk<-lm( wp[,k] ~ wp$o1 + wp$o2
              + wp$c1 + wp$c2 
              + wp$e1 + wp$e2
              + wp$a1 + wp$a2
              + wp$s1 + wp$s2 )
        r2s[k]<-summary(mk)$r.squared
    }
}


univ.r2s.wp<-data.frame(name=nms[1:430],r.squared=r2s)

max.pcor<-function( r2sdf ){
  nms<-r2sdf[,1]
  correls<-rep(0,length(nms))
  pvar <-c("o1","o2","c1","c2",
           "e1","e2","a1","a2",
           "s1","s2")
  for ( k in 1:length(nms) ){
    
    cvec<-cor( wp[nms[k]],wp[,pvar],
               use = "complete.obs")
    
    cmax<-0
    imx<-which.max(abs(cvec))
    if(length(imx)>0){
      cmax<-cvec[imx]
    }
    correls[k]<-cmax
  }
  out<-data.frame(name=nms, mc=correls)
  out  
}

all_stability_estimate<-function(){
  nms <- names(wp)[1:430]
  sds<-rep(0,430)
  for (k in 1:430){
    print(k)
    df <- data.frame( country=wp$V2, 
                      v=wp[,nms[k]])
    fdf<-freq_dist_minspread(df,ssize=5000,
                             nsampling=3000,
                             minspread=20)
    cdf<-stat_curve(fdf)
    sds[k]<-mean(cdf$sx)
  }
  out<-data.frame( name=nms, sds=sds)
}

get_curve<-function( var ){
  
  rawdata<-freq_dist_minspread(wvs[,c("V2",var)],
                               ssize=5000,nsampling=5000,
                               minspread=10)
  out <- stat_curve( rawdata)
  out
}
